(ns script
  {:clj-kondo/config '{:lint-as {script/plet clojure.core/let}}}
  (:require
   [clojure.string :as str]
   ["puppeteer" :as puppeteer]
   ["airtable" :as airtable]
   ["cheerio" :as cheerio]))

(defmacro plet
  "Macro for painless working with js promises, NOTE: output is in js format,
  so you might need to interop before passing it further .eg (js->clj x)"
  [bindings & body]
  (let [binding-pairs (reverse (partition 2 bindings))
        body (cons 'do body)]
    (reduce (fn [body [sym expr]]
              (let [expr (list '.resolve 'js/Promise expr)]
                (list '.then expr (list 'clojure.core/fn (vector sym)
                                        body))))
            body
            binding-pairs)))

(def airtable-cfg
  {:api-key (.-AIRTABLE_API_KEY js/process.env)
   :base (.-AIRTABLE_BASE js/process.env)})

(.configure airtable {:apiKey (airtable-cfg :api-key)})

(defn base [name]
  ((.base airtable (airtable-cfg :base)) name))

(defn fburl? [url]
  (let [u (js/URL. url)]
    (set! (. u -search) "")
    (if (str/ends-with? (.-hostname u) "facebook.com")
      true
      false)))

(defn get-events []
  (let [v (transient [])]
    (-> (base "Event")
        (.select #js{:view "Grid view"})
        (.eachPage
         (fn [records fetchNextPage]
           (doseq [r records]
             (let [url ((js->clj (.-fields r)) "Event URL")]
               (when (fburl? url)
                 (conj! v {:url url
                           :id (.-id r)}))))
           (fetchNextPage)))
        (.then (fn [err]
                 (if err
                   (throw (ex-info (.-message err) nil))
                   (persistent! v)))))))

(defn to-record [e]
  {:id (e :id)
   :fields {"Event URL" (e :url)
            "Event Name" (e :name)
            "Date" (js/Date. (e :from))
            "End Date" (js/Date. (e :to))
            "Location" (e :location)
            "Ticket URL" (e :ticket-url)
            "Notes" "<updated by crawler>"}})

(defn push-update [e]
  (prn (e :url) "Airtable: Start updating record")
  (-> (base "Event")
      (.update (clj->js [(to-record e)]))
      (.then (fn [records]
               (doseq [r records]
                 (prn ((js->clj (.-fields r)) "Event URL") "Airtable: Updated record"))
               records))
      (.catch #(js/console.error (e :url) "Airtable: Error updating event:" (.-message %)))))

(defn event-from-html [html]
  (let [$ (.load cheerio html)
        name (-> ($ "#seo_h1_tag") (.text))
        detail (-> ($ "#event_summary") (.find "ul") (.children) (.first))
        [from to] (-> detail
                      (.find "div[content]")
                      (.attr "content")
                      (str/split "to")
                      (#(map str/trim %)))]
    {:name name
     :from from
     :to to
     :location (-> (.next detail)
                   (.find "span[style]")
                   (.text))
     :ticket-url (-> (.next detail)
                     (.next)
                     (.children)
                     (.attr "href"))}))

(defn process-page [{:keys [id url]} browser]
  (plet [page (.newPage browser)
         selector "#event_header"
         html (-> (.goto page url #{:waitUntil "networkidle0"})
                  (.then #(.waitForSelector page selector))
                  (.then #(.getProperty % "innerHTML"))
                  (.then #(.jsonValue %)))
         _ (.close page)
         event (event-from-html html)]
        (-> (push-update (assoc (js->clj event) :id id :url url))
            (.catch (fn [err]
                      (js/console.error url "Error fetching events for" (.-message err))
                      (-> (.close page)
                          (.then #(identity []))))))))

(defn process [evts]
  (plet [browser (.launch puppeteer)
         records (->> evts
                      (map #(process-page % browser))
                      (.all js/Promise))
         _ (.close browser)]
        records))

(defn run []
  (plet [events (get-events)
         updated (->> (partition-by #(% :url) events)
                      (map first)
                      (#(do (prn "Fetched" (count %) "events") %))
                      (#(-> (process %)
                            (.catch (fn [err]
                                      (prn "Error:" (.-message err))
                                      (js/process.exit 1))))))]
        (prn "Updated" (count updated) "events")))

(run)
