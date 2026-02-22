(ns bot
  (:require
   [Puyo :as p]))

(def state
  (p/create-globalstate clojure.lang.PersistentQueue/EMPTY))

(print state)
(p/state-update state (:state-enum state) {} 0 0)
