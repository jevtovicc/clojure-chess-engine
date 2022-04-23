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

(def num-of-eval-positions (atom 0))

(defn evaluate-board [board]
  (reduce
   (fn [total-score square] (+ total-score (get-piece-value (board/get-piece board square))))
   0
   board/all-squares))

(declare minimax-max)

(defn minimax-min
  ([game-state depth] (minimax-min game-state depth -9999 9999))
  ([game-state depth alpha beta]
   (if (zero? depth)
     [(evaluate-board (:board game-state)) nil]
     (loop [[move & moves] (rules/possible-moves game-state)
            best-score 9999
            best-move nil
            alpha alpha
            beta beta]
       (if (nil? move)
         [best-score best-move]
         (let [[from-sq to-sq] move
               [score _] (minimax-max (rules/make-move game-state from-sq to-sq) (dec depth) alpha beta)
               best-move (if (< score best-score) move best-move)
               best-score (min best-score score)]
           (if (<= best-score alpha)
             [best-score best-move]
             (recur moves best-score best-move alpha (min beta best-score)))))))))

(defn minimax-max [game-state depth alpha beta]
  (if (zero? depth)
    [(evaluate-board (:board game-state)) nil]
    (loop [[move & moves] (rules/possible-moves game-state)
           best-score -9999
           best-move nil
           alpha alpha
           beta beta]
      (if (nil? move)
        [best-score best-move]
        (let [[from-sq to-sq] move
              [score _] (minimax-min (rules/make-move game-state from-sq to-sq) (dec depth) alpha beta)
              best-move (if (> score best-score) move best-move)
              best-score (max score best-score)]
          (if (>= best-score beta)
            [best-score best-move]
            (recur moves best-score best-move (max best-score alpha) beta)))))))
