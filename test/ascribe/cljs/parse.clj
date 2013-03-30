(ns ascribe.cljs
  (:require [ascribe.core :refer (defattr tree parent child root?)]))

;;TODO parse and unparse should be generated from a grammar
;;TODO (asset (= (unparse (parse form)) form))
;; I guess this is a "tree grammar", not a normal grammar

(declare parse)

(defmulti parse-seq first)

(defmethod parse-seq 'if
  [[_ test then else :as form]]
  {:op :if :form form
   :test (parse test)
   :then (parse then)
   :else (parse else)})

(defmethod parse-seq 'throw
  [[_ throw :as form]]
  {:op :throw :form form
   :throw (parse throw)})

(defmethod parse-seq 'try*
  [[_ & body :as form]]
  (let [body (vec body)
        tail (peek body)
        fblock (when (and (seq? tail) (= 'finally (first tail)))
                  (rest tail))
        finally (when fblock (parse `(do ~@fblock)))
        body (if finally (pop body) body)
        tail (peek body)
        cblock (when (and (seq? tail)
                          (= 'catch (first tail)))
                 (rest tail))
        name (first cblock)
        catch (when cblock (parse `(do ~@(rest cblock))))
        body (if name (pop body) body)
        try (parse `(do ~@body))]
    {:op :try* :form form
     :try try
     :finally finally
     :name name
     :catch catch}))

;;TODO parse-seq 'set!
;;TODO parse-seq 'ns
;;TODO parse-seq 'deftype*
;;TODO parse-seq 'defrecord*
;;TODO parse-seq '.
;;TODO parse-seq 'js*

(defmethod parse-seq 'def
  [form]
  (let [pfn (fn
              ([_ sym] {:sym sym})
              ([_ sym init] {:sym sym :init init})
              ([_ sym doc init] {:sym sym :doc doc :init init}))
        args (apply pfn form) ;TODO don't throw exception on invalid arity
        sym (:sym args)
        met (meta sym)]
    {:op :def :form form
     :sym sym
     :meta met  ;TODO doesn't match clojurescript at all
     :doc (:doc args (:doc sym))
     :init (parse (:init args))}))

(defn- parse-fn-method [form]
  (let [params (first form)
        variadic (boolean (some '#{&} params))
        params (vec (remove '#{&} params))
        body (next form)
        fixed-arity (count (if variadic (butlast params) params))
        expr (parse `(do ~@body))]
    {:form form
     :variadic variadic
     :params params
     :max-fixed-arity fixed-arity
     :expr expr}))

(defmethod parse-seq 'fn*
  [[_ & args :as form]]
  (let [[name meths] (if (symbol? (first args))
                       [(first args) (next args)]
                       [name (seq args)])
        ;;turn (fn [] ...) into (fn ([]...))  ;TODO should happen in fn macro
        meths (if (vector? (first meths)) (list meths) meths)
        met (meta form)
        methods (map parse-fn-method meths)]
    {:op :fn :form form
     :name name
     :methods (mapv parse-fn-method meths)}))

(defn- parse-bindings
  [[_ bindings & exprs :as form]]
  ;;TODO validate bindings even count etc
  (let [bes (into [] (for [[name init] (partition 2 bindings)]
                        {:name name :init (parse init)}))
        expr (parse `(do ~@exprs))]
    {:form form
     :bindings bes
     :expr expr}))

(defmethod parse-seq 'letfn*
  [form]
  (-> form
    parse-bindings
    (update-in [:bindings] #(into {} (map (juxt :name :init) %)))
    (assoc :op :letfn)))

(defmethod parse-seq 'let*
  [form]
  (assoc (parse-bindings form) :op :let))

(defmethod parse-seq 'let*
  [form]
  (assoc (parse-bindings form) :op :loop))

(defmethod parse-seq 'do
  [[_ & exprs :as form]]
  (let [statements (mapv parse (butlast exprs))
        ret (parse (last exprs))]
    {:op :do :form form
     :statements statements :ret ret}))

(defmethod parse-seq 'recur
  [[_ & exprs :as form]]
  {:op :recur :form form
   :exprs (mapv parse exprs)})

(defmethod parse-seq 'quote
  [[_ form]]
  {:op :constant :form form})

(defmethod parse-seq 'new
  [[_ ctor & args :as form]]
  {:op :new :form form
   :ctor (parse ctor)
   :args (mapv parse args)})

(declare parse-map parse-vector parse-set parse-keyword)

(defn parse-symbol [form]
  {:op :var :form form})

(defn parse-constant [form]
  {:op :constant :form form})

(defn parse [form]
  (let [form (if (instance? clojure.lang.LazySeq form)
               (or (seq form) ())   ;;TODO what's this about?
               form)
        parser (cond
                 (symbol? form) parse-symbol
                 (and (seq? form) (seq form)) parse-seq
                 (map? form) parse-map
                 (vector? form) parse-vector
                 (set? form) parse-set
                 (keyword? form) parse-keyword
                 :else parse-constant)]
    (parser form)))


(comment

  (clojure.pprint/pprint
    (parse '
      (letfn* [f (fn* f [x] 1)
               g (fn* g [x] 2)]
        3)
      ))

)
