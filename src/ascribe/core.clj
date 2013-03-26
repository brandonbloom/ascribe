(ns ascribe.core)

(def foo {:left {:value 2}
          :right {:left {:value 1}
                  :right {:value 3}}})

(defprotocol INodeRef
  (-root [this])
  (-path [this]))

;; Consider:
; references          the remote tree node
; absolute paths      full path to a node
; relative paths      relative to current node
; computed paths      something like a name
(deftype NodeRef [root path]
  clojure.lang.IDeref
  (deref [_]
    (get-in root path))
  INodeRef
  (-root [_] root)
  (-path [_] path))

(def ^:dynamic *depth* -1)

(def ^:dynamic *trace* true)

(defn attr-fn [kw f]
  (fn [node]
    (assert (instance? NodeRef node) "Attribute fn must be applied to a node")
    (binding [*depth* (inc *depth*)]
      (when *trace*
        (print (apply str (repeat (* 2 *depth*) \space)))
        (println kw "@" (-path node)))
      (f node))))

(defmacro defattr [name args & body]
  `(def ~name (attr-fn ~(keyword name) (fn ~name ~args ~@body))))

;;; intrinsics

(defattr parent [node]
  (let [p (-path node)]
    (when (seq p)
      (NodeRef. (-root node) (pop p)))))

(defattr root? [node]
  (nil? (parent node)))

;;; repmin tree

(defattr value [node]
  (:value @node))

(defattr left [node]
  (NodeRef. (-root node) (conj (-path node) :left)))

(defattr right [node]
  (NodeRef. (-root node) (conj (-path node) :right)))

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

#_(-> (NodeRef. foo [])
    ret)
