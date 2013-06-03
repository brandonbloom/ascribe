(ns ascribe.cljs.js
  (:require [ascribe.cljs.attrs.clj :as clj]
            [clojure.data.json :as json]))

(defmulti emit clj/op)

(defmethod emit :constant [node]
  {:type "Literal" :value (:value @node)})


(comment

  (require '[ascribe.core :as a])
  (require '[ascribe.cljs.parse :refer (parse-form)])

  (->
    "omg"
    parse-form
    a/tree
    emit
    json/write-str
    println
  )

  ;; JSON schema for JS Ast:
  ; https://developer.mozilla.org/en-US/docs/SpiderMonkey/Parser_API

  ;; Code generator:
  ; https://github.com/Constellation/escodegen

)
