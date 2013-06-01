(ns ascribe.cljs.attrs.clj
  (:require [ascribe.core :refer (defattr tree parent child
                                  node? root? child-node?)]
            [ascribe.cljs.attrs.edn :as edn]))

;(defmulti -children :op)
;
;(defattr children [node]
;  (-children node))

(defattr callee [call]
  ;;TODO return the node defining the callee or some description
  ;;TODO see also callee-name
  ;; hackily assume calls are always to symbol nodes right now
  {:name (-> @call :items first :value)})

(defattr callee-name [call]
  (let [c (callee call)]
    (if (node? c)
      (:name @c)
      (:name c))))

(defn macro? [callable]
  (contains? #{'a-macro} (:name callable)))

(declare quoted?)

(defattr quote? [node]
  (and (edn/list? node)
       (not (quoted? node))
       (= (callee-name node) 'quote)))

(defattr call? [node]
  (and (not (quoted? node))
       (edn/list? node)))

(defattr macro-call? [node]
  (and (call? node)
       (macro? (callee node))))

(defattr fn-call? [node]
  (and (call? node)
       (not (macro-call? node))))

(defattr quoter? [node]
  (or (quote? node)
      (macro-call? node)
      (edn/tag? node)))

(defattr quoted? [node]
  (and (child-node? node)
       (let [p (parent node)]
         (or (quoter? p)
             (quoted? p)))))

(defattr expanded? [node]
  (not (macro-call? node)))

;(defattr fully-expanded? [node]
;  (and (expanded? node)
;       (every? fully-expanded? (children node))))
;
;(defattr transformed [node pre post]
;
;  )
;
;(defattr macroexpanded [node]
;  (transformed [node
;  (if (? node)
;    node
;    (recur (expansion node))))


(comment

  (require '[ascribe.cljs.parse])

  (defn parse [form]
    (tree (ascribe.cljs.parse/parse-form form)))

  (->
    '(a-macro asdf)
    parse
    macro-call?
  )

)
