(ns ascribe.core)



(def graph (atom {}))




(def attrs {

  :prototype
  (fn [node attr value]
    value)

  :container
  (fn [node attr value]
    value)

  :contains
  (fn [node attr value]
    (or value :children))

  :template
  (fn [node attr value]
    (or value (-> node :prototype :template)))

  :items
  (fn [node attr value]
    (or value (-> node :prototype :items) []))

  :item-template
  (fn [node attr value]
    (or value (-> node :prototype :item-template) identity))

  :item-index
  (fn [node attr value]
    value)

  :expanded
  (fn [node attr value]
    (assert (nil? value))
    (->
      (if-let [template (:template node)]
        (assoc (template node) :container node)
        (update-in node [:children] #(mapv :expanded %)))
      (dissoc :template :items :item-template :contains)))

  :children
  (fn [node attr value]
    (let [contains (:contains node)]
      (->>
        (case contains
          :children (or value (-> node :prototype :children))
          :items (let [{:keys [items item-template]} node]
                   (assert (nil? value))
                   (map-indexed
                     (fn [i item]
                       (-> (item-template item) (assoc :item-index i)))
                     items)))
        (map-indexed (fn [i expanded]
                       (assoc expanded :container node :child-index i)))
        vec)))

  :text
  (fn [node attr value]
    (or value (-> node :prototype :text) ""))

  :child-index
  (fn [node attr value]
    value)

  :previous-child
  (fn [node attr value]
    (assert (nil? value))
    (let [previous-index (dec (:child-index node))]
      (when (<= 0 previous-index)
        ((-> node :container :children) previous-index))))

  :next-child
  (fn [node attr value]
    (assert (nil? value))
    (let [next-index (inc (:child-index node))
          siblings (-> node :container :children)]
      (when (> (count siblings) next-index)
        (siblings next-index))))

  :layout
  (fn [node attr value]
    (or value (-> node :prototype :layout) :fill))

  :bounds
  (fn [node attr value]
    (or value
        (let [container (:container node)]
          (case (:layout container)
            :fill [(:left container) (:top container)
                   (:right container) (:bottom container)]
            :vertical [(:left container) (or (-> node :previous-child :bottom)
                                             (:top container))
                       (:right container) (:content-height node)]
            ))))

  :left
  (fn [node attr value]
    (or value (nth (:bounds node) 0)))

  :top
  (fn [node attr value]
    (or value (nth (:bounds node) 1)))

  :right
  (fn [node attr value]
    (or value (nth (:bounds node) 2)))

  :bottom
  (fn [node attr value]
    (or value (nth (:bounds node) 3)))

  :width
  (fn [node attr value]
    (or value (- (:right node) (:left node))))

  :height
  (fn [node attr value]
    (or value (- (:bottom node) (:top node))))

  :content-width
  (fn [node attr value]
    (if (contains? node :width)
      (:width node)
      (apply max 0 (map :width (:children node)))))

  :content-height
  (fn [node attr value]
    (if (contains? node :height)
      (:height node)
      (apply max 0 (map :height (:children node)))))

})



(deftype Node [graph attributes]

  clojure.lang.ILookup
  (valAt [this attr]
    (.valAt this attr nil))
  (valAt [this attr not-found]
    (let [f (get attrs attr)
          _ (assert f)
          v (.valAt attributes attr not-found)]
      (f this attr v)))

  clojure.lang.Associative
  (containsKey [_ k]
    (contains? attributes k))
  (entryAt [_ k]
    (find attributes k))

  clojure.lang.Seqable
  (seq [_]
    (seq attributes))

  clojure.lang.IPersistentMap
  (assoc [_ k v]
    (Node. graph (assoc attributes k v)))
  (without [_ k]
    (Node. graph (dissoc attributes k)))

  )


(comment

  (def root (Node. graph {:bounds [0 0 800 600]}))


  (def a (Node. graph {:text "Hey"}))
  (def b (Node. graph {:prototype a}))

  (:text b)

  (def x (Node. graph {:contains :items
                       :items ["foo" "bar"]
                       :item-template #(Node. graph {:text %})}))
  (fipp.edn/pprint (:expanded x))


  (def y (Node. graph {:layout :vertical
                       :children [(Node. graph {:text "A" :width 15 :height 3})
                                  (Node. graph {:text "B" :width 20 :height 7})]}))
  (-> (get (:children y) 1) :container :children)
  (:next-child (get (:children y) 0))


  (fipp.edn/pprint (:expanded (assoc y :container root)))
  (-> y
    (assoc :container root)
    :expanded
    :children
    second
    :top
    fipp.edn/pprint
    )



  ;{:layout :fill
  ; :arrangement (fn [this]

  ;
  ; (defmulti arrange :layout)
  ;
  ; (defmethod arrange :fill [node]
  ;   {:left (-> node :container :left)
  ;    :width (-> node :container :width)
  ;    :top (-> node :container :top)
  ;    :height (-> node :container :height)})
  ;
  ;
  ; (defn initial-arrangement
  ;
  ;
  ; {:width 100
  ;  :height 500}

  ; (->
  ;   (-> this :container :width))

  ; {:layout :whatever
  ;  :arrangement  (attr [this] (func of (:layout this)))
  ;  :width (attr [this] (-> this :arrangement :width))
  ;  :height (attr [this] (-> this :arrangement :height))
  ;  :left (attr [this] (-> this :arrangement :left))
  ;  :right (attr [this] (-> this :arrangement :right))}

)
