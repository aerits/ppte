(ns PuyoTypes
 (:require-macros [ Macros :as m ]) )

(def enum [:pt/empty :pt/red :pt/green :pt/blue :pt/yellow :pt/purple])


;; (def all-types [empty-cell red-puyo green-puyo blue-puyo yellow-puyo purple-puyo] )
;; (m/enumGenerator all-types)
;; (println (macroexpand-1 '(m/enumGenerator all-types)))

;; (defn puyos-assertHandlesAllTypes [handle]
;;   (m/assertHandlesAllTypes handle all-types (fn [e] (throw (js/Error e) ))))
