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
(def ^:dynamic *cache* true)

(defn attr-fn [kw f]
  (fn [node & args]
    (assert (node? node) "Attribute fn must be applied to a node")
    (binding [*depth* (inc *depth*)]
      (let [p (-path node)
            cache (-cache (-tree node))
            cache-key [p kw (vec args)]
            cached (get @cache cache-key ::miss)
            cached? (not= cached ::miss)]
        (when *trace*
          (print (apply str (repeat (* 2 *depth*) \space)))
          (when cached?
            (print "CACHED! "))
          (println kw (vec args) "@" p))
        (if cached?
          cached
          (let [ret (apply f node args)]
            (when *cache*
              (swap! cache assoc cache-key ret))
            ret))))))

(defmacro defattr [name args & body]
  `(def ~name (attr-fn ~(keyword name) (fn ~name ~args ~@body))))

(defattr parent [node]
  (let [p (-path node)]
    (when (seq p) ;;TODO better implementation now that we have Trees ?
      (Node. (-tree node) (pop p)))))

(defattr root? [node]
  (nil? (parent node)))

(defattr child [node key]
  (when (contains? @node key)
    (Node. (-tree node) (conj (-path node) key))))
