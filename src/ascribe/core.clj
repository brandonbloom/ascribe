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

;(defn attr-fn [name f]
;  (fn [node]
;    (assert (instance? NodeRef node))
;    (f node)))
;
;(defmacro defattr [name args & body]
;  `(def ~name (attr-fn ~(keyword name) (fn ~name ~args ~body))))

;;; intrinsics

(defn parent [node]
  (let [p (-path node)]
    (when (seq p)
      (NodeRef. (-root node) (pop p)))))

(defn root? [node]
  (nil? (parent node)))

;;; repmin tree

(defn value [node]
  (:value @node))

(defn left [node]
  (NodeRef. (-root node) (conj (-path node) :left)))

(defn right [node]
  (NodeRef. (-root node) (conj (-path node) :right)))

(defn lmin [node]
  (or (value node)
      (min (-> node left lmin) (-> node right lmin))))

(defn gmin [node]
  (if (root? node)
    (lmin node)
    (-> node parent gmin)))

(defn ret [node]
  (if (value node)
    {:value (gmin node)}
    {:left (-> node left ret) :right (-> node right ret)}))

#_(-> (NodeRef. foo [])
    ret)
