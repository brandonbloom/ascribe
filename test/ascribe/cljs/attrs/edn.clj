(ns ascribe.cljs.attrs.edn
  (:refer-clojure :exclude [list? map? vector? set?])
  (:require [ascribe.core :as a :refer (defattr)]))

(defattr tag? [node]
  (contains? @node :tag))

(defattr scalar? [node]
  (contains? @node :value))

(defattr composite? [node]
  (contains? @node :composite))

(defattr list? [node]
  (= (:composite @node) :list))

(defattr map? [node]
  (= (:composite @node) :map))

(defattr vector? [node]
  (= (:composite @node) :vector))

(defattr set? [node]
  (= (:composite @node) :set))

(defattr items [node]
  (a/child node :items))

(defattr form [node]
  ;;TODO meta
  (cond
    ;;TODO tag?
    (scalar? node) (:value @node)
    (composite? node)
      (let [elements (map form (-> node items a/elements))
            composite (:composite @node)
            f (composite {:list list*
                          :map #(apply hash-map %)
                          :vector vec
                          :set set})]
        (f elements))))
