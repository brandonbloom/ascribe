(ns ascribe.core-test
  (:use clojure.test)
  (:require [ascribe.core :as a :refer (defattr)]))

(deftest traversal-tests
  (testing "Children and Siblings"
    (let [t (a/tree [:x :y :z])]
      (is (= (a/children-count t) 3))
      (is (= (-> t a/first-child deref) :x))
      (is (= (-> t a/first-child a/right a/right deref) :z))
      (is (nil? (-> t a/first-child a/right a/right a/right)))
      (is (= (-> t a/last-child deref) :z))
      (is (= (-> t a/last-child a/left a/left deref) :x)))
    ))
