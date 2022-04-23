(ns clojure-chess-engine.pieces)

(def white-piece? #{:R :N :B :Q :K :P})
(def black-piece? #{:r :n :b :q :k :p})

(defn piece-color [p]
  (cond
    (white-piece? p) :white
    (black-piece? p) :black))

(defn same-piece-color? [p1 p2]
  (= (piece-color p1) (piece-color p2)))

(defmulti piece->imgsrc identity)

(defmethod piece->imgsrc :r [_] "resources/images/rook-black.png")
(defmethod piece->imgsrc :R [_] "resources/images/rook-white.png")
(defmethod piece->imgsrc :n [_] "resources/images/knight-black.png")
(defmethod piece->imgsrc :N [_] "resources/images/knight-white.png")
(defmethod piece->imgsrc :b [_] "resources/images/bishop-black.png")
(defmethod piece->imgsrc :B [_] "resources/images/bishop-white.png")
(defmethod piece->imgsrc :q [_] "resources/images/queen-black.png")
(defmethod piece->imgsrc :Q [_] "resources/images/queen-white.png")
(defmethod piece->imgsrc :k [_] "resources/images/king-black.png")
(defmethod piece->imgsrc :K [_] "resources/images/king-white.png")
(defmethod piece->imgsrc :p [_] "resources/images/pawn-black.png")
(defmethod piece->imgsrc :P [_] "resources/images/pawn-white.png")
(defmethod piece->imgsrc :e [_] "")

