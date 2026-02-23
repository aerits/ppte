(ns lib)

(defn create-queue []
  clojure.lang.PersistentQueue/EMPTY)

(defn floor [n]
  (Math/floor n))
