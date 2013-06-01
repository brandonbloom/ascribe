(ns ascribe.core
  (:require [ascribe.protocols :as proto]))

(def ^:dynamic *depth* -1)

(def ^:dynamic *trace* false)
(def ^:dynamic *cache* false)

(deftype Tree [root cache]
  clojure.lang.IDeref
  (deref [_] root)
  proto/INode
  (-tree [this] this)
  (-path [_] [])
  proto/ITree
  (-root [_] root)
  (-cache [_] cache))

(deftype Node [tree path]
  clojure.lang.IDeref
  (deref [_]
    (get-in (proto/-root tree) path))
  proto/INode
  (-tree [_] tree)
  (-path [_] path))

(defn tree? [x]
  (satisfies? proto/ITree x))

(defn node? [x]
  (satisfies? proto/INode x))

(defn tree [root]
  (Tree. root (atom {})))

(defn root [node]
  (proto/-root (proto/-tree node)))

(defn attr-fn [name f]
  (fn [node & args]
    (assert (node? node) "Attribute fn must be applied to a node")
    (binding [*depth* (inc *depth*)]
      (let [p (proto/-path node)
            cache (proto/-cache (proto/-tree node))
            cache-key [p f (vec args)]
            cached (get @cache cache-key ::miss)
            cached? (not= cached ::miss)]
        (when *trace*
          (print (apply str (repeat (* 2 *depth*) \space)))
          (when cached?
            (print "CACHED! "))
          (println name (vec args) "@" p))
        (if cached?
          cached
          (let [ret (apply f node args)]
            (when *cache*
              (swap! cache assoc cache-key ret))
            ret))))))

(defmacro defattr [name args & body]
  `(def ~name (attr-fn ~name (fn ~name ~args ~@body))))

(defattr parent [node]
  (let [p (proto/-path node)]
    (when (seq p) ;;TODO better implementation now that we have Trees ?
      (Node. (proto/-tree node) (pop p)))))

(defattr root? [node]
  (nil? (parent node)))

(defattr child [node key]
  (when (contains? @node key)
    (Node. (proto/-tree node) (conj (proto/-path node) key))))

(defattr child-node? [node]
  (not (root? node)))

(defn splice [at x]
  (let [value (if (node? x) @x x)
        p (proto/-path at)]
    (if (empty? p)
      (tree value)
      (Node. (tree (assoc-in (root at) p value)) p))))

(defmacro defsplice [name args & body]
  `(defattr ~name ~args
     (splice ~(first args) (do ~@body))))
