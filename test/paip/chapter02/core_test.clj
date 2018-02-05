(ns paip.chapter02.core-test
  (:require [clojure.test :as t]
            [paip.chapter02.core :as sut]))

;;;; 2. A Simple Lisp Program

;; Exercise 2.4
(t/deftest combine-all'-test
  (t/is (= '((a 1) (b 1) (a 2) (b 2)) (sut/combine-all' '((a) (b)) '((1) (2))))))

(t/deftest cross-product-test
  (t/is (= '(11 12 13 21 22 23 31 32 33)
           (sut/cross-product + '(1 2 3) '(10 20 30))))
  (t/is (= '((A 1) (B 1) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)
                   (A 2) (B 2) (C 2) (D 2) (E 2) (F 2) (G 2) (H 2)
                   (A 3) (B 3) (C 3) (D 3) (E 3) (F 3) (G 3) (H 3)
                   (A 4) (B 4) (C 4) (D 4) (E 4) (F 4) (G 4) (H 4)
                   (A 5) (B 5) (C 5) (D 5) (E 5) (F 5) (G 5) (H 5)
                   (A 6) (B 6) (C 6) (D 6) (E 6) (F 6) (G 6) (H 6)
                   (A 7) (B 7) (C 7) (D 7) (E 7) (F 7) (G 7) (H 7)
                   (A 8) (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8))
           (sut/cross-product list
                              '(A B C D E F G H)
                              '(1 2 3 4 5 6 7 8)))))
