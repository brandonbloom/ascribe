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
      (var-named x))))

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

(defattr expanded? [node]
  (not (macro-call? node)))

;;TODO find better name. ambiguous: symbol-namespace vs node-is-in-namespace
(defattr namespace [node]
  ;;TODO get this from the ns form
  (create-ns 'cljs.user))

(defattr env [node]
  ;;TODO stick vars & locals and whatnot in here
  {})

(defsplice expanded1 [node]
  (let [mac (callee node)
        form (edn/form node)]
    (binding [*ns* (namespace node)]
      (parse-form (apply mac form env (rest form))))))

;(defsplice expanded [node]
;  (loop [node node]
;    (if (expanded? node)
;      (expand-children node)
;      (recur (expand-node node)))))

;(defattr fully-expanded? [node]
;  (and (expanded? node)
;       (every? fully-expanded? (children node))))
;
;(defattr macroexpanded [node]
;  (transformed [node
;  (if (? node)
;    node
;    (recur (expansion node))))


(comment

  (require '[ascribe.cljs.parse])

  (defn parse [form]
    (a/tree (ascribe.cljs.parse/parse-form form)))

  (->
    '(let [x 1] x)
    parse
    expanded1
    ;edn/form
  )

)
