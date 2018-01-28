(ns paip.chapter01.core-test
  (:require [clojure.test :as t]
            [paip.chapter01.core :as sut]))

;;;; 1. Introduction to Lisp

;; Exercise 1.1
;; TODO

;; Exercise 1.2
(t/deftest power-test
  (t/is (= 9 (sut/power 3 2)))
  (t/is (= 1 (sut/power 2 0)))
  (t/is (= 2 (sut/power 2 1)))
  (t/is (= 16 (sut/power 2 4)))
  (t/is (= 32 (sut/power 2 5))))

;; Exercise 1.3
(t/deftest count-atoms-test
  (t/is (= 3 (sut/count-atoms '(a (b) c))))
  (t/is (= 0 (sut/count-atoms ())))
  (t/is (= 2 (sut/count-atoms '(a () c))))
  (t/is (= 3 (sut/count-atoms '(a nil c)))))

;; Exercise 1.4
(t/deftest count-anywhere-test
  (t/is (= 3 (sut/count-anywhere 'a '(a ((a) b) a)))))

;; Exercise 1.5
(t/deftest dot-product-test
  (t/is (= 110 (sut/dot-product '(10 20) '(3 4)))))
