(ns client
  (:require ["react" :as react]
            [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdomc]))

(defn app []
  [:div {:style {:display "flex"
                 :flex-direction "row"}}
   [:div {:style {:display "inline-block"}}
    ;; "player"]
    [:iframe {:id "frame1"
              :src "index.html?play=true&server=0"
              :height "700px"
              :width "500px"}]]
   (->> (concat [:div {:id "others"
                       :style {:height "800px"
                               :width "700px"
                               :border "1px solid #ccc"
                               :overflow "auto"
                               :display "grid"
                               :grid-template-columns "auto auto auto"}}]

                (for [i (range 5)]
                  ;; [:p "yay"]))
                  [:iframe {:id "frame1"
                            :src "index.html?play=false&server=0"
                            :height "100%"
                            :width "100%"}]))
        (into []))])
(print (app))

(def functional-compiler (r/create-compiler {:function-components true}))

(defonce react-root (delay (rdomc/create-root (.getElementById js/document "app"))))

(defn ^:export ^:dev/after-load run []
  (rdomc/render @react-root [app] functional-compiler))
