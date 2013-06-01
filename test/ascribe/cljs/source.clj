(ns ascribe.cljs.source
  (:require [ascribe.core :refer (defattr tree parent child root? child-node?)]))

(def ^:dynamic *quoted* false)

(declare edn->source)

(defn composite->source [{:keys [composite items] :as edn}]
  {:op composite
   :items items})

(defmulti foo->source first)

;(defmethod foo->source 'if [[_ & args]]
;  )
;
;(defmethod foo->source 'throw [[_ & args]]
;  )
;
;(defmethod foo->source 'try* [[_ & args]]
;  )
;
;(defmethod foo->source 'def [[_ & args]]
;  )
;
;(defmethod foo->source 'fn* [[_ & args]]
;  )
;
;(defmethod foo->source 'letfn* [[_ & args]]
;  )
;
;(defmethod foo->source 'do [[_ & args]]
;  )
;
;(defmethod foo->source 'let* [[_ & args]]
;  )
;
;(defmethod foo->source 'loop* [[_ & args]]
;  )
;
;(defmethod foo->source 'recur [[_ & args]]
;  )

(defmethod foo->source 'quote [[_ & args]]
  (binding [*quoted* true]
    ;TODO ensure exactly one arg ?
    (edn->source (first args))))
;
;(defmethod foo->source 'new [[_ & args]]
;  )
;
;(defmethod foo->source 'set! [[_ & args]]
;  )
;
;(defmethod foo->source 'ns [[_ & args]]
;  )
;
;(defmethod foo->source 'deftype* [[_ & args]]
;  )
;
;(defmethod foo->source 'defrecord* [[_ & args]]
;  )
;
;(defmethod foo->source '. [[_ & args]]
;  )
;
;(defmethod foo->source 'js* [[_ & args]]
;  )

(defmethod application->source :default [[f & args]]
  {:op :application
   :f (edn->source f)
   :args (mapv edn->source args)})

(defmethod foo->source :default [list]
  (application->source list))

(defn list->source [{:keys [items] :as edn}]
  (cond
    (or *quoted* (empty? items)) (composite->source edn)
    (symbol? (first items)) (foo->source edn)
    :else (application->source edn)))

(defn scalar->source [{:keys [value] :as edn}]
  ;;TODO symbols, etc
  {:op :constant
   :value value})

(defn edn->source [edn]
  ;(if (:tag edn) TODO ...)
  (if-let [composite (:composite edn)]
    (if (= composite :list)
      (list->source edn)
      (composite->source edn))
    (scalar->source edn)))

(defn source-tree [edn]
  (tree (edn->source edn)))


;;;;;;;;;; HMMMM I'm not sure anything in this file makes sense
;;;;;;;;;; in theory, any unknown applicative potentially implies quoting...

;(defmulti -children :op)
;
;(defattr children [node]
;  (-children node))

(defattr callee [call]
  ;;TODO return the node defining the callee or some description
  ;;TODO see also callee-name
  ;; hackily assume calls are always to symbol nodes right now
  {:name (:value call)}
  )

(defattr callee-name [call]
  (let [c (callee call)]
    (if (node? c)
      (:name @c)
      (:name c))))

(defattr tag? [node]
  (contains? @node :tag))

(defattr quote? [node]
  (and (list-node? node)
       (= (callee-name node) 'quote)))

(defttr quoter? [node]
  (or (quote? node)
      (macro-call? node)
      (tag? node)))

(declare quoted?)

(defattr call? [node]
  (and (not (quoted? node))
       (list-node? node)))

(defattr macro-call? [node]
  (and (call? node)
       (macro? (callee node))))

(defattr fn-call? [node]
  (and (call? node)
       (not (macro-call? node))))

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
