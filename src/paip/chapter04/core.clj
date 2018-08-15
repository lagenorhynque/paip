(ns paip.chapter04.core
  (:require [clojure.set :as set]))

;;;; 4. GPS: The General problem Solver

(def state
  "The current state: a list of conditions."
  (atom #{}))

(def ops
  "A list of available operations."
  (atom #{}))

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
    (println "executing" (:action op))
    (swap! state set/difference (:del-list op))
    (swap! state set/union (:add-list op))
    true))

(defn achieve
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  [goal]
  (or (contains? @state goal)
      (some apply-op
            (filter #(appropriate? goal %) @ops))))

(defn GPS
  "General Problem Solver: achieve all goals using ops."
  [state goals ops]
  (when (every? achieve goals) :solved))
