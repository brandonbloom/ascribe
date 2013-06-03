(ns ascribe.cljs.attrs.clj
  (:refer-clojure :exclude [namespace])
  (:require [ascribe.core :as a :refer (defattr defsplice)]
            [ascribe.cljs.parse :refer (parse-form)]
            [ascribe.cljs.attrs.edn :as edn]))

;(defmulti -children :op)
;
;(defattr children [node]
;  (-children node))

(defn macro? [x]
  ;;TODO consider .foo and Foo. symbols
  (and (var? x)
       (.isMacro ^clojure.lang.Var x)))

(defattr vars [node]
  ;;TODO defattr vars and vars', update through ns macro analysis
  (into {} (filter (comp macro? val) (ns-interns 'clojure.core))))

(defattr var-named [node sym]
  ((vars node) sym))

(defattr callee [call]
  ;; hackily assume calls are always to special or var symbols right now
  (let [x (-> @call :items first :value)]
    (if (special-symbol? x)
      x
      (var-named call x))))

(declare quoted?)

(defattr quote? [node]
  (and (edn/list? node)
       (not (quoted? node))
       (= (callee node) 'quote)))

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
  (and (a/child-node? node)
       (let [p (a/parent node)]
         (or (quoter? p)
             (quoted? p)))))

;;TODO find better name. ambiguous: symbol-namespace vs node-is-in-namespace
(defattr namespace [node]
  ;;TODO get this from the ns form
  (create-ns 'cljs.user))

(defattr env [node]
  ;;TODO stick vars & locals and whatnot in here
  {})

;;; Macro Expansion

(defsplice expanded1 [node]
  (let [mac (callee node)
        form (edn/form node)]
    (binding [*ns* (namespace node)]
      (parse-form (apply mac form env (rest form))))))

(declare expanded)

(defsplice expanded-children [node]
  (cond
    (quoted? node) node
    (macro-call? node) node
    (edn/composite? node) (-> (a/child node :items)
                              (a/map-children expanded)
                              a/parent)
    :else node))

(defsplice expanded [node]
  (loop [node node]
    (if (macro-call? node)
      (recur (expanded1 node))
      (expanded-children node))))

;;; Post-expansion operations

(declare op)

(defn list-op [node]
  (let [c (callee node)]
    (if (special-symbol? c)
      (keyword c)
      :invoke)))

(defn blah-op [node]
  (cond
    (edn/list? node) (list-op node)
    (edn/composite? node) (:composite @node)
    ;;TODO edn/tag?
    :else :constant))

(defmulti -children-ops #'op)

(defmethod -children-ops :let* [node]
  (let [[_ bindings & statements] (-> node (a/child :items) a/children)]
    (into {bindings :bindings}
          (map (juxt identity blah-op) statements))))

(defmethod -children-ops :default [node]
  ;;TODO probably want to delete this method
  (println "DEFAULT!!")
  (prn (op node)))

(defattr children-ops [node]
  (-children-ops node))

(defattr child-op [node child]
  ((children-ops node) child))

(defattr op [node]
  (if (a/root? node)
    (blah-op node)
    (child-op (a/parent node) node)))


(comment

  (defn parse [form]
    (a/tree (parse-form form)))

  (->
    '(let [x 1] (let [y 2] (* x y)))
    parse
    expanded
    op
    ;a/first-child
    ;edn/form
  )

  :return :expr :statement

)
