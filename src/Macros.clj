(ns Macros)

(defmacro assertHandlesAllTypes [handle enum-types error]
  `(doseq [var# ~enum-types]
     (when (not (contains? ~handle var#))
       (~error (str '~handle " didn't handle " var#)))))

(defmacro defenum [name & el]
  (let [el-map (into {} (map-indexed (fn [idx el]
                                           [(keyword el) idx])
                                         el))]
    `(def ~name ~el-map)))
