(ns clojure-chess-engine.utils)

(defn take-while+
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns logical true, including the first item
  for which (pred item) returns logical false."
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn my-any?
  "Returns true if some element in coll satisfies predicate"
  [pred col]
  (not (not-any? pred col)))
