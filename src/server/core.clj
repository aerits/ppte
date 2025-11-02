(ns server.core
  (:require [io.pedestal.connector :as conn]
            [io.pedestal.interceptor :as ic]
            [io.pedestal.service.interceptors :as interceptors]
            [io.pedestal.http.ring-middlewares :as ring-middlewares]
            [io.pedestal.http.route :as route]
            [io.pedestal.service.resources :as resources]
            [io.pedestal.http.http-kit :as hk]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
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

(defn greet [request]
  (-> (ok (json/write-str {"bruh" :a}))
      (json)))

(defn game [request]
  (-> (ok "<!DOCTYPE html><head></head><body>
<iframe src='/index.html' height='800px' width='600px'></iframe>
</body></html>")
      (html)))

(defn index [request]
  (-> (ok "hi chat <br> <a href='game'>play</a>")
      (html)))

(defmacro create-routes
  {:clj-kondo/lint-as 'clojure.core/def}
  [name & routes]
  `(def ~name
     #{~@(-> (for [route routes]
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
                (fn [el] [(:name el) :get (:get el) :route-name (:route-name el)]) %)))}))

(create-routes routes
               {:fn greet}
               {:fn game}
               {:fn index})

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

(when @*connector (restart))

(defn -main [& {:as args}]
  (start
   (into {} (map (fn [[k v]] [(-> (rest k)
                                  (str/join)
                                  (keyword))
                              v]) args))))
