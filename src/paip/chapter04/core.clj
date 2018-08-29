(ns paip.chapter04.core
  (:require [clojure.set :as set]))

;;;; 4. GPS: The General problem Solver

(def ^:dynamic *state*
  "The current state: a list of conditions."
  #{})

(def ^:dynamic *ops*
  "A list of available operations."
  [])

(defrecord Op [action
               preconds
               add-list
               del-list])

(defn make-op [& {:keys [action preconds add-list del-list]
                  :or {action nil
                       preconds #{}
                       add-list #{}
                       del-list #{}}}]
  (->Op action preconds add-list del-list))

(defn appropriate?
  "An op is appropriate to a goal if it is in its add list."
  [goal op]
  (contains? (:add-list op) goal))

(declare achieve)

(defn apply-op
  "Print a message and update state if op is applicable."
  [op]
  (when (every? achieve (:preconds op))
    (println (list 'executing (:action op)))
    (set! *state* (set/difference *state* (:del-list op)))
    (set! *state* (set/union *state* (:add-list op)))
    true))

(defn achieve
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  [goal]
  (or (contains? *state* goal)
      (some apply-op
            (filter #(appropriate? goal %) *ops*))))

(defn GPS
  "General Problem Solver: achieve all goals using ops."
  [state goals ops]
  (binding [*state* state
            *ops* ops]
    (when (every? achieve goals) :solved)))

(def school-ops
  [(make-op :action 'drive-son-to-school
            :preconds '#{son-at-home car-works}
            :add-list '#{son-at-school}
            :del-list '#{son-at-home})
   (make-op :action 'shop-installs-battery
            :preconds '#{car-needs-battery shop-knows-problem shop-has-money}
            :add-list '#{car-works})
   (make-op :action 'tell-shop-problem
            :preconds '#{in-communication-with-shop}
            :add-list '#{shop-knows-problem})
   (make-op :action 'telephone-shop
            :preconds '#{know-phone-number}
            :add-list '#{in-communication-with-shop})
   (make-op :action 'look-up-number
            :preconds '#{have-phone-book}
            :add-list '#{know-phone-number})
   (make-op :action 'give-shop-money
            :preconds '#{have-money}
            :add-list '#{shop-has-money}
            :del-list '#{have-money})])
