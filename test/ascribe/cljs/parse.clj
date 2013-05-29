(ns ascribe.cljs.parse
  (:require [ascribe.core :refer (defattr tree parent child root?)]))

(declare parse-form)

(defn parse-tagged [tag form]
  (reduced (assoc (parse-form form) :tag tag)))

(defn classify-composite [x]
  (cond
    (seq? x) :list
    (map? x) :map
    (set? x) :set
    (vector? x) :vector))

(defn parse-form [form]
  (if (reduced? form)
    @form
    (cond->
      (if-let [composite (classify-composite form)]
        {:composite composite :elements (mapv parse-form form)}
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

)
