(ns bot
  (:require
   [Puyo :as p]))

(def state
  (p/create-globalstate))

(print state)
(p/state-update state (:state-enum state) {} 0 0)
