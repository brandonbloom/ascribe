(ns ascribe.cljs.attrs.edn
  (:refer-clojure :except [list?])
  (:require [ascribe.core :refer (defattr)]))

(defattr tag? [node]
  (contains? @node :tag))

(defattr list? [node]
  (= (:composite @node) :list))

(defattr composite? [node]
  (boolean (:composite @node)))
