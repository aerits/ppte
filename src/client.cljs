(ns client
  (:require ["react" :as react]
            [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdomc]))

(defn app []
  (->> (concat [:div
                [:iframe {:id "frame1"
                          :src "index.html?play=true&server=0"
                          :height "700px"
                          :width "600px"}]]

               (for [i (range 5)]
                 [:iframe {:id "frame1"
                           :src "index.html?play=true&server=0"
                           :height "200px"
                           :width "100px"}]))
       (into [])))

(print (app))

(def functional-compiler (r/create-compiler {:function-components true}))

(defonce react-root (delay (rdomc/create-root (.getElementById js/document "app"))))

(defn ^:export ^:dev/after-load run []
  (rdomc/render @react-root [app] functional-compiler))
