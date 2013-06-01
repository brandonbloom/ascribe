(ns ascribe.repmin
  (:require [ascribe.core :refer (defattr defsplice tree parent child root?)]))

(defattr value [node]
  (:value @node))

(defattr left [node]
  (child node :left))

(defattr right [node]
  (child node :right))

(defattr lmin [node]
  (or (value node)
      (min (-> node left lmin) (-> node right lmin))))

(defattr gmin [node]
  (if (root? node)
    (lmin node)
    (-> node parent gmin)))

(defattr ret [node]
  (if (value node)
    {:value (gmin node)}
    {:left (-> node left ret) :right (-> node right ret)}))

(defsplice ret2 [node]
  (if (value node)
    {:value (gmin node)}
    {:left @(-> node left ret2) :right @(-> node right ret2)}))

(comment

  (def t (tree {:left {:value 2}
                :right {:left {:value 1}
                        :right {:value 3}}}))

  (-> t)
  (-> t deref)

  (-> t ret)
  (-> t left)
  (-> t lmin)
  (-> t right ret)
  (-> t left lmin)

  (-> t ret2)
  (-> t ret2 right right deref)

  (clojure.pprint/pprint @(-cache bar))

  ;;TODO ideas from kiama:
  ;; circular attributes
  ;; higher-order trees <- sorta have this now, need forrest cache
  ;; attribute forwarding

)
