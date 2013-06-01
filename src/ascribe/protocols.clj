(ns ascribe.protocols)

(defprotocol ITree
  (-root [this])
  (-cache [this]))

(defprotocol INode
  (-tree [this])
  (-path [this]))
