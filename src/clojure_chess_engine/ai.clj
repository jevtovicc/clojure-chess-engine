(ns clojure-chess-engine.ai
  (:require [clojure-chess-engine.rules :as rules]
            [clojure-chess-engine.board :as board]))

(defmulti get-piece-value identity)

(defmethod get-piece-value :p [_] -10)
(defmethod get-piece-value :P [_] 10)
(defmethod get-piece-value :b [_] -30)
(defmethod get-piece-value :B [_] 30)
(defmethod get-piece-value :n [_] -30)
(defmethod get-piece-value :N [_] 30)
(defmethod get-piece-value :r [_] -50)
(defmethod get-piece-value :R [_] 50)
(defmethod get-piece-value :q [_] -90)
(defmethod get-piece-value :Q [_] 90)
(defmethod get-piece-value :k [_] -900)
(defmethod get-piece-value :K [_] 900)
(defmethod get-piece-value :e [_] 0)

(defn evaluate-board [board]
  (reduce
   (fn [total-score square] (+ total-score (get-piece-value (board/get-piece board square))))
   0
   board/all-squares))

(declare minimax-max)

(defn minimax-min [game-state depth]
  (if (zero? depth)
    [(evaluate-board (:board game-state)) nil]
    (loop [[move & moves] (rules/possible-moves game-state)
           best-score 9999
           best-move nil]
      (if (nil? move)
        [best-score best-move]
        (let [[from-sq to-sq] move
              [score _] (minimax-max (rules/make-move game-state from-sq to-sq) (dec depth))]
          (recur moves (min best-score score) (if (< score best-score) move best-move)))))))

(defn minimax-max [game-state depth]
  (if (zero? depth)
    [(evaluate-board (:board game-state)) nil]
    (loop [[move & moves] (rules/possible-moves game-state)
           best-score -9999
           best-move nil]
      (if (nil? move)
        [best-score best-move]
        (let [[from-sq to-sq] move
              [score _] (minimax-min (rules/make-move game-state from-sq to-sq) (dec depth))]
          (recur moves (max best-score score) (if (> score best-score) move best-move)))))))
