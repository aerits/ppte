(ns Particle)

(defn create-particle [f-draw f-update traits]
  (merge traits {:on-update f-update :on-draw f-draw} ))

(defn pdraw [particle & body]
  (try (apply (:on-draw particle) body)
       (catch js/Object e
         (println (str "couldn't run on-draw:" e)))))

(defn pupdate [particle & body]
  (apply (:on-update particle) body))
