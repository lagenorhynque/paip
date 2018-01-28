(ns paip.chapter01.core-test
  (:require [clojure.test :as t]
            [paip.chapter01.core :as sut]))

;;;; 1. Introduction to Lisp

;; Exercise 1.2
(t/deftest power-test
  (t/is (= 1 (sut/power 2 0)))
  (t/is (= 2 (sut/power 2 1)))
  (t/is (= 16 (sut/power 2 4)))
  (t/is (= 32 (sut/power 2 5))))
