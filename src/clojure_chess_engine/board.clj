(ns clojure-chess-engine.board)

(def all-squares
  (for [x (range 8)
        y (range 8)]
    [x y]))

(defn get-piece [board square]
  (get-in board square))

(defn remove-piece [board from-sq]
  (assoc-in board from-sq :e))

(defn place-piece [board square piece]
  (assoc-in board square piece))

(defn move-piece [board from-sq to-sq]
  (let [piece (get-piece board from-sq)]
    (-> board
        (remove-piece from-sq)
        (place-piece to-sq piece))))

(defn square-empty? [board square]
  (= (get-piece board square) :e))

(def square-occupied? (complement square-empty?))

(defn occupied-squares [board]
  (filter #(square-occupied? board %) all-squares))

(defn square-on-board? [[rank file]]
  (and (<= 0 rank 7) (<= 0 file 7)))

(defn add-squares [sq1 sq2]
  (map + sq1 sq2))
