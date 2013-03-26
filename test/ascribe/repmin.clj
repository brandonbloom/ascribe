(ns ascribe.repmin
  (:require [ascribe.core :refer (defattr tree parent child root?)]))

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

(comment

  (def foo {:left {:value 2}
            :right {:left {:value 1}
                    :right {:value 3}}})

  (let [bar (tree foo)]
    (ret bar)
    (println "----------")
    (ret bar)
    ;(clojure.pprint/pprint @(-cache bar))
  )

  ;;TODO ideas from kiama:
  ;; circular attributes
  ;; higher-order trees
  ;; attribute forwarding
  ;; tree splicing

)
