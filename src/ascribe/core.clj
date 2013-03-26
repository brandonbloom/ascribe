(ns ascribe.core)

(defprotocol ITree
  (-root [this])
  (-cache [this]))

(defprotocol INode
  (-tree [this])
  (-path [this]))

(deftype Tree [root cache]
  clojure.lang.IDeref
  (deref [_] root)
  INode
  (-tree [this] this)
  (-path [_] [])
  ITree
  (-root [_] root)
  (-cache [_] cache))

(defn tree [root]
  (Tree. root (atom {})))

(defn root [node]
  (-root (-tree node)))

(deftype Node [tree path]
  clojure.lang.IDeref
  (deref [_]
    (get-in (-root tree) path))
  INode
  (-tree [_] tree)
  (-path [_] path))

(defn tree? [x]
  (satisfies? ITree x))

(defn node? [x]
  (satisfies? INode x))

(def ^:dynamic *depth* -1)

(def ^:dynamic *trace* false)

(defn attr-fn [kw f]
  (fn [node & args]
    (assert (node? node) "Attribute fn must be applied to a node")
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
      (Node. (-tree node) (pop p)))))

(defattr root? [node]
  (nil? (parent node)))

(defattr child [node key]
  (when (contains? @node key)
    (Node. (-tree node) (conj (-path node) key))))

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

  (def foo {:left {:value 2}
            :right {:left {:value 1}
                    :right {:value 3}}})

  (-> foo
      tree
      ret
    )

)
