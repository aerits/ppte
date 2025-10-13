(ns Particle)

(defn create-particle [f-draw f-update traits]
  (merge traits {:on-update f-update :on-draw f-draw} ))

(defn pdraw [particle & body]
  (apply (:on-draw particle) body))

(defn pupdate [particle & body]
  (apply (:on-update particle) body))
