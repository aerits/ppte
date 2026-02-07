(ns server.core
  (:require [io.pedestal.connector :as conn]
            [io.pedestal.interceptor :as ic]
            [io.pedestal.service.interceptors :as interceptors]
            [io.pedestal.http.ring-middlewares :as ring-middlewares]
            [io.pedestal.http.route :as route]
            [io.pedestal.service.resources :as resources]
            [io.pedestal.service.websocket :as webs]
            [io.pedestal.http.http-kit :as hk]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [nrepl.server :as nrepl])
  (:gen-class))

(def header-interceptor
  (ic/interceptor
   {:name ::add-header
    :leave (fn [context]
             (-> (assoc-in context [:response :headers "Content-Security-Policy"]
                           "script-src 'self' 'unsafe-inline' 'unsafe-eval'")
                 (assoc-in [:response :headers "X-Frame-Options"] "SAME-ORIGIN")))}))

(defn ok [message]
  {:status 200 :body message})
   ;; :headers {"Content-Security-Policy" "script-src 'self' 'unsafe-inline' 'unsafe-eval; frame-ancestors 'self''"
   ;;           "X-Frame-Options" "SAME-ORIGIN"}})

(defmacro create-content-type
  {:clj-kondo/lint-as 'clojure.core/def}
  [name type]
  `(defn ~name [response#] (assoc-in response# [:headers "Content-Type"] ~type)))

(macroexpand-1 '(create-content-type html "text/html"))

(create-content-type html "text/html")
(create-content-type json "application/json")

(defmacro defroute
  {:clj-kondo/lint-as 'clojure.core/defn}
  [name args & body]
  `(do
     (defn ~(symbol (str name "--route")) ~args
       ~@body)
     (defn ~name ~args
       (~(symbol (str name "--route")) ~args))))

(macroexpand-1
 '(defroute index [request]
    (-> (ok "hi ppte <br> <a href='game'>play</a>")
        (html))))

(defroute greet [request]
  (-> (ok (json/write-str {"bruh" :b}))
      (json)))

(defroute game [request]
  (-> (ok "<!DOCTYPE html><head>
<script src='js/main.js'></script>
</head><body>
<iframe src='/index.html?play=true&server=0' height='800px' width='600px'></iframe>
</body></html>")
      (html)))

(defroute index [request]
  (-> (ok "hi <br> <a href='game'>play</a>")
      (html)))

(defroute ws [request]
  (-> (ok "yay")
      (html)))

(defmacro create-routes
  {:clj-kondo/lint-as 'clojure.core/def}
  [& routes]
  `#{~@(-> (for [route routes]
             (cond
               (contains? route :fn)
               (let [route (:fn route)]
                 {:name
                  (if (= "index" (str route)) "/" (str "/" route))
                  :get route
                  :route-name (keyword (str route "-route"))})

               :else (throw (Exception. "bad args"))))
           (flatten)
           (#(map
              (fn [el] [(:name el) :get (:get el) :route-name (:route-name el)]) %)))})

(def ws-options {:on-text (fn [chan obj str] (println str))})

(def routes
  (-> (create-routes
       {:fn greet}
       {:fn game}
       {:fn index})
      (conj ["/ws" :get [(webs/websocket-interceptor nil ws-options) ws] :route-name :ws])))

(defn create-connector [& {:keys [port]
                           :or {port "8890"}
                           :as opts}]
  (let [port (if (integer? port) (str port) port)
        port (if (string? port) (Integer/parseInt port) (throw (Exception. "Wrong type for port")))]
    (println "Hosting on 0.0.0.0:" port)
    (-> (conn/default-connector-map "0.0.0.0" port)
      ;; (conn/with-default-interceptors)
        (conn/with-interceptors
          [interceptors/log-request
           interceptors/not-found
           (ring-middlewares/content-type)
           route/query-params
           (io.pedestal.http.body-params/body-params)
           (io.pedestal.http.secure-headers/secure-headers)
           header-interceptor])
        (conn/with-routes routes (resources/file-routes
                                  {:file-root "./public"
                                   :fast? true
                                   :prefix "/"}))
        (hk/create-connector nil))))

;; For interactive development
(defonce *connector (atom nil))

(defn start [& {:as args}]
  (reset! *connector
          (conn/start! (create-connector args))))

(defn stop []
  (conn/stop! @*connector)
  (reset! *connector nil))

(defn restart []
  (stop)
  (start))

(defn -main [& {:as args}]
  (nrepl/start-server :port 7888)
  (start
   ;; convert map keys from strings into keywords
   ;; will probably fail if you put a bad format in program args
   (into {} (map (fn [[k v]] [(-> (rest k)
                                  (str/join)
                                  (keyword))
                              v]) args))))
