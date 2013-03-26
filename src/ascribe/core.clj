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

(def ^:dynamic *trace* false)

(defn attr-fn [kw f]
  (fn [node & args]
    (assert (instance? NodeRef node) "Attribute fn must be applied to a node")
    (binding [*depth* (inc *depth*)]
      (when *trace*
        (print (apply str (repeat (* 2 *depth*) \space)))
        (println kw "@" (-path node)))
      (apply f node args))))

(defmacro defattr [name args & body]
  `(def ~name (attr-fn ~(keyword name) (fn ~name ~args ~@body))))

;;; intrinsics

(defattr parent [node]
  (let [p (-path node)]
    (when (seq p)
      (NodeRef. (-root node) (pop p)))))

(defattr root? [node]
  (nil? (parent node)))

(defattr child [node key]
  (when (contains? @node key)
    (NodeRef. (-root node) (conj (-path node) key))))

;;; repmin tree

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

  (-> (NodeRef. foo [])
    (child :right)
    (child :right)
    (child :right)
    )

)
