(ns clojure-chess-engine.ai
  (:require [clojure-chess-engine.rules :as rules]
            [clojure-chess-engine.board :as board]))

(def white-eval-table
  {:p [[0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0]
       [5.0  5.0  5.0  5.0  5.0  5.0  5.0  5.0]
       [1.0  1.0  2.0  3.0  3.0  2.0  1.0  1.0]
       [0.5  0.5  1.0  2.5  2.5  1.0  0.5  0.5]
       [0.0  0.0  0.0  2.0  2.0  0.0  0.0  0.0]
       [0.5 -0.5 -1.0  0.0  0.0 -1.0 -0.5  0.5]
       [0.5  1.0 1.0  -2.0 -2.0  1.0  1.0  0.5]
       [0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0]]

   :n [[-5.0 -4.0 -3.0 -3.0 -3.0 -3.0 -4.0 -5.0]
       [-4.0 -2.0  0.0  0.0  0.0  0.0 -2.0 -4.0]
       [-3.0  0.0  1.0  1.5  1.5  1.0  0.0 -3.0]
       [-3.0  0.5  1.5  2.0  2.0  1.5  0.5 -3.0]
       [-3.0  0.0  1.5  2.0  2.0  1.5  0.0 -3.0]
       [-3.0  0.5  1.0  1.5  1.5  1.0  0.5 -3.0]
       [-4.0 -2.0  0.0  0.5  0.5  0.0 -2.0 -4.0]
       [-5.0 -4.0 -3.0 -3.0 -3.0 -3.0 -4.0 -5.0]]

   :b [[-2.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -2.0]
       [-1.0  0.0  0.0  0.0  0.0  0.0  0.0 -1.0]
       [-1.0  0.0  0.5  1.0  1.0  0.5  0.0 -1.0]
       [-1.0  0.5  0.5  1.0  1.0  0.5  0.5 -1.0]
       [-1.0  0.0  1.0  1.0  1.0  1.0  0.0 -1.0]
       [-1.0  1.0  1.0  1.0  1.0  1.0  1.0 -1.0]
       [-1.0  0.5  0.0  0.0  0.0  0.0  0.5 -1.0]
       [-2.0 -1.0 -1.0 -1.0 -1.0 -1.0 -1.0 -2.0]]

   :r [[0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0]
       [0.5  1.0  1.0  1.0  1.0  1.0  1.0  0.5]
       [-0.5  0.0  0.0  0.0  0.0  0.0  0.0 -0.5]
       [-0.5  0.0  0.0  0.0  0.0  0.0  0.0 -0.5]
       [-0.5  0.0  0.0  0.0  0.0  0.0  0.0 -0.5]
       [-0.5  0.0  0.0  0.0  0.0  0.0  0.0 -0.5]
       [-0.5  0.0  0.0  0.0  0.0  0.0  0.0 -0.5]
       [0.0   0.0 0.0  0.5  0.5  0.0  0.0  0.0]]

   :q [[-2.0 -1.0 -1.0 -0.5 -0.5 -1.0 -1.0 -2.0]
       [-1.0  0.0  0.0  0.0  0.0  0.0  0.0 -1.0]
       [-1.0  0.0  0.5  0.5  0.5  0.5  0.0 -1.0]
       [-0.5  0.0  0.5  0.5  0.5  0.5  0.0 -0.5]
       [0.0  0.0  0.5  0.5  0.5  0.5  0.0 -0.5]
       [-1.0  0.5  0.5  0.5  0.5  0.5  0.0 -1.0]
       [-1.0  0.0  0.5  0.0  0.0  0.0  0.0 -1.0]
       [-2.0 -1.0 -1.0 -0.5 -0.5 -1.0 -1.0 -2.0]]

   :k [[-3.0 -4.0 -4.0 -5.0 -5.0 -4.0 -4.0 -3.0]
       [-3.0 -4.0 -4.0 -5.0 -5.0 -4.0 -4.0 -3.0]
       [-3.0 -4.0 -4.0 -5.0 -5.0 -4.0 -4.0 -3.0]
       [-3.0 -4.0 -4.0 -5.0 -5.0 -4.0 -4.0 -3.0]
       [-2.0 -3.0 -3.0 -4.0 -4.0 -3.0 -3.0 -2.0]
       [-1.0 -2.0 -2.0 -2.0 -2.0 -2.0 -2.0 -1.0]
       [2.0  2.0  0.0  0.0  0.0  0.0  2.0  2.0]
       [2.0  3.0  1.0  0.0  0.0  1.0  3.0  2.0]]})

(def black-eval-table
  (reduce-kv
   (fn [acc k v] (assoc acc k (->> v
                                   (mapv (fn [row] (mapv - row)))
                                   reverse
                                   vec)))
   {}
   white-eval-table))


(defmulti get-piece-value (fn [piece row tile] piece))

(get-in black-eval-table [:q 5 0])
(defmethod get-piece-value :p [_ row tile] (+ -10 (get-in black-eval-table [:p row tile])))
(defmethod get-piece-value :P [_ row tile] (+ 10 (get-in white-eval-table [:p row tile])))
(defmethod get-piece-value :b [_ row tile] (+ -30 (get-in black-eval-table [:b row tile])))
(defmethod get-piece-value :B [_ row tile] (+ 30 (get-in white-eval-table [:b row tile])))
(defmethod get-piece-value :n [_ row tile] (+ -30 (get-in black-eval-table [:n row tile])))
(defmethod get-piece-value :N [_ row tile] (+ 30 (get-in white-eval-table [:n row tile])))
(defmethod get-piece-value :r [_ row tile] (+ -50 (get-in black-eval-table [:r row tile])))
(defmethod get-piece-value :R [_ row tile] (+ 50 (get-in white-eval-table [:r row tile])))
(defmethod get-piece-value :q [_ row tile] (+ -90 (get-in black-eval-table [:q row tile])))
(defmethod get-piece-value :Q [_ row tile] (+ 90 (get-in white-eval-table [:q row tile])))
(defmethod get-piece-value :k [_ row tile] (+ -900 (get-in black-eval-table [:k row tile])))
(defmethod get-piece-value :K [_ row tile] (+ 900 (get-in white-eval-table [:k row tile])))
(defmethod get-piece-value :e [_ row tile] 0)


(defn evaluate-board [board]
  (reduce
   (fn [total-score [row tile :as square]] (+ total-score (get-piece-value (board/get-piece board square) row tile)))
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
