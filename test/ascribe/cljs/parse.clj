(ns ascribe.cljs.parse)

(declare parse-form)

(deftype TaggedLiteral [ast]
  clojure.lang.IMeta
  (meta [_] nil)
  clojure.lang.IObj
  (withMeta [_ meta]
    (TaggedLiteral. (assoc ast :meta meta))))

(defn parse-tagged [tag form]
  (TaggedLiteral. {:tag tag :literal (parse-form form)}))

(defn classify-composite [x]
  (cond
    (seq? x) :list
    (map? x) :map
    (set? x) :set
    (vector? x) :vector))

(defn parse-form [form]
  (if (instance? TaggedLiteral form)
    (.ast ^TaggedLiteral form)
    (cond->
      (if-let [composite (classify-composite form)]
        {:composite composite :items (mapv parse-form form)}
        {:value form})
      (meta form) (assoc :meta (meta form)))))

(defn parse-string [string]
  (binding [*data-readers* {}
            *default-data-reader-fn* parse-tagged]
    (-> string read-string parse-form)))

(comment

  (fipp.edn/pprint
    (parse-string "^:foo [{:x 'y} {:z #tagged w} 123]")
  )

  (fipp.edn/pprint
    (parse-string "^:foo #tag ^:bar []")
  )

  (fipp.edn/pprint
    (parse-string "'1")
  )

  ;TODO need a :file composite or something like that

)
