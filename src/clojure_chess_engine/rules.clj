(ns clojure-chess-engine.rules
  (:require [clojure-chess-engine.board :as board]
            [clojure-chess-engine.utils :as utils]
            [clojure-chess-engine.pieces :as pieces]))

;; directions
(def dir-up         [1 0])
(def dir-down       [-1 0])
(def dir-right      [0 1])
(def dir-left       [0 -1])
(def dir-up-right   [1 1])
(def dir-up-left    [1 -1])
(def dir-down-right [-1 1])
(def dir-down-left  [-1 -1])

(def rook-directions [dir-left dir-right dir-up dir-down])
(def bishop-directions [dir-up-left dir-up-right dir-down-left dir-down-right])
(def all-directions (concat rook-directions bishop-directions))
(def knight-directions [[2 1] [2 -1] [1 2] [1 -2]
                        [-2 1] [-2 -1] [-1 2] [-1 -2]])

(defn- get-players-king [player]
  (condp = player
    :white :K
    :black :k))

(defn flip-player [player]
  (condp = player
    :white :black
    :black :white))

(defmulti get-pseudolegal-destinations
  (fn [game-state from-sq] (board/get-piece (:board game-state) from-sq)))

(defmethod get-pseudolegal-destinations :n
  [{board :board :as game-state} from-sq]
  (->> knight-directions
       (map #(board/add-squares from-sq %))
       (filter board/square-on-board?)
       (remove #(pieces/same-piece-color? :n (board/get-piece board %)))
       #_(map #(let [board' (board/move-piece board from-sq %)
                   next-player (flip-player (:player-on-move game-state))]
               (assoc game-state :board board' :player-on-move next-player)))))

(defmethod get-pseudolegal-destinations :N
  [{board :board :as game-state} from-sq]
  (->> knight-directions
       (map #(board/add-squares from-sq %))
       (filter board/square-on-board?)
       (remove #(pieces/same-piece-color? :N (board/get-piece board %)))
       #_(map #(let [board' (board/move-piece board from-sq %)
                   next-player (flip-player (:player-on-move game-state))]
               (assoc game-state :board board' :player-on-move next-player)))))

(defn get-squares-in-direction [board from-sq dir]
  (let [piece (board/get-piece board from-sq)]
    (->> (board/add-squares from-sq dir)
         (iterate #(board/add-squares % dir))
         (utils/take-while+ #(board/square-empty? board %))
         (filter board/square-on-board?)
         (remove #(pieces/same-piece-color? piece (board/get-piece board %))))))

(defn get-squares-in-directions [board from-sq dirs]
  (mapcat #(get-squares-in-direction board from-sq %) dirs))

(defmethod get-pseudolegal-destinations :r
  [{board :board :as game-state} from-sq]
  (let [legal-squares (get-squares-in-directions board from-sq rook-directions)]
    legal-squares
    #_(map #(let [board' (board/move-piece board from-sq %)
                next-player (flip-player (:player-on-move game-state))]
            (assoc game-state
                   :board board'
                   :player-on-move next-player
                   :black-can-castle-ks? (if (or (= from-sq [0 7])
                                                 (= (:black-can-castle-ks? game-state) false))
                                           false
                                           true)
                   :black-can-castle-qs? (if (or (= from-sq [0 0])
                                                 (= (:black-can-castle-qs? game-state) false))
                                           false
                                           true)))
         legal-squares)))

(defmethod get-pseudolegal-destinations :R
  [{board :board :as game-state} from-sq]
  (let [legal-squares (get-squares-in-directions board from-sq rook-directions)]
    legal-squares
    #_(map #(let [board' (board/move-piece board from-sq %)
                next-player (flip-player (:player-on-move game-state))]
            (assoc game-state
                   :board board'
                   :player-on-move next-player
                   :white-can-castle-ks? (if (or (= from-sq [7 7])
                                                 (= (:white-can-castle-ks? game-state) false))
                                           false
                                           true)
                   :white-can-castle-qs? (if (or (= from-sq [7 0])
                                                 (= (:white-can-castle-qs? game-state) false))
                                           false
                                           true)))
         legal-squares)))

(defmethod get-pseudolegal-destinations :b
  [{board :board :as game-state} from-sq]
  (let [legal-squares (get-squares-in-directions board from-sq bishop-directions)]
    legal-squares
    #_(map #(let [board' (board/move-piece board from-sq %)
                next-player (flip-player (:player-on-move game-state))]
            (assoc game-state :board board' :player-on-move next-player))
         legal-squares)))

(defmethod get-pseudolegal-destinations :B
  [{board :board :as game-state} from-sq]
  (let [legal-squares (get-squares-in-directions board from-sq bishop-directions)]
    legal-squares
    #_(map #(let [board' (board/move-piece board from-sq %)
                next-player (flip-player (:player-on-move game-state))]
            (assoc game-state :board board' :player-on-move next-player))
         legal-squares)))

(defmethod get-pseudolegal-destinations :q
  [{board :board :as game-state} from-sq]
  (let [legal-squares (get-squares-in-directions board from-sq all-directions)]
    legal-squares
    #_(map #(let [board' (board/move-piece board from-sq %)
                next-player (flip-player (:player-on-move game-state))]
            (assoc game-state :board board' :player-on-move next-player))
         legal-squares)))

(defmethod get-pseudolegal-destinations :Q
  [{board :board :as game-state} from-sq]
  (let [legal-squares (get-squares-in-directions board from-sq all-directions)]
    legal-squares
    #_(map #(let [board' (board/move-piece board from-sq %)
                next-player (flip-player (:player-on-move game-state))]
            (assoc game-state :board board' :player-on-move next-player))
         legal-squares)))

(defmethod get-pseudolegal-destinations :p
  [{board :board :as game-state} from-sq]
  (let [not-moved? (= (first from-sq) 1)
        one-up (board/add-squares dir-up from-sq)
        two-up (board/add-squares dir-up one-up)
        one-up-right (board/add-squares dir-up-right from-sq)
        one-up-left (board/add-squares dir-up-left from-sq)]
    (cond-> []
      (board/square-empty? board one-up) (conj one-up)
      (and not-moved?
           (board/square-empty? board one-up)
           (board/square-empty? board two-up)) (conj two-up)
      (and (not (board/square-empty? board one-up-left))
           (not (pieces/same-piece-color? :p (board/get-piece board one-up-left)))) (conj one-up-left)
      (and (not (board/square-empty? board one-up-right))
           (not (pieces/same-piece-color? :p (board/get-piece board one-up-right)))) (conj one-up-right)
      :always (->> (filter board/square-on-board?)
                   #_(map #(let [board' (board/move-piece board from-sq %)
                               next-player (flip-player (:player-on-move game-state))]
                           (assoc game-state :board board' :player-on-move next-player)))))))

(defmethod get-pseudolegal-destinations :P
  [{board :board :as game-state} from-sq]
  (let [not-moved? (= (first from-sq) 6)
        one-down (board/add-squares dir-down from-sq)
        two-down (board/add-squares dir-down one-down)
        one-down-right (board/add-squares dir-down-right from-sq)
        one-down-left (board/add-squares dir-down-left from-sq)]
    (cond-> []
      (board/square-empty? board one-down) (conj one-down)
      (and not-moved?
           (board/square-empty? board one-down)
           (board/square-empty? board two-down)) (conj two-down)
      (and (not (board/square-empty? board one-down-left))
           (not (pieces/same-piece-color? :P (board/get-piece board one-down-left)))) (conj one-down-left)
      (and (not (board/square-empty? board one-down-right))
           (not (pieces/same-piece-color? :P (board/get-piece board one-down-right)))) (conj one-down-right)
      :always (->> (filter board/square-on-board?)
                   #_(map #(let [board' (board/move-piece board from-sq %)
                               next-player (flip-player (:player-on-move game-state))]
                           (assoc game-state :board board' :player-on-move next-player)))))))

(declare squares-attacked-by-player)
(declare in-check?)

(defn castling-squares [side]
  (condp = side
    :k #{[0 5] [0 6]}
    :q #{[0 1] [0 2] [0 3]}
    :K #{[7 5] [7 6]}
    :Q #{[7 1] [7 2] [7 3]}))

(defn castling-squares-empty? [board side]
  (->> side
       castling-squares
       (every? #(board/square-empty? board %))))

(defn castling-squares-attacked? [{player-on-move :player-on-move :as game-state} side]
  (let [attacked-squares (squares-attacked-by-player game-state (flip-player player-on-move))
        castling-squares (castling-squares side)]
    (utils/my-any? castling-squares attacked-squares)))

(defn castling-possible? [game-state side]
  (and (castling-squares-empty? (:board game-state) side)
       (not (castling-squares-attacked? game-state side))
       (not (in-check? game-state))))

(defmethod get-pseudolegal-destinations :k
  [{:keys [board black-can-castle-ks? black-can-castle-qs?] :as game-state} from-sq]
  (cond-> []
    (and black-can-castle-ks? (castling-possible? game-state :k)) (conj [0 6])
    (and black-can-castle-qs? (castling-possible? game-state :q)) (conj [0 1])
    :always (into
             (->> all-directions
                  (map #(board/add-squares from-sq %))
                  (filter board/square-on-board?)
                  (remove #(pieces/same-piece-color? :k (board/get-piece board %)))
                  #_(map #(let [board' (board/move-piece board from-sq %)
                              next-player (flip-player (:player-on-move game-state))]
                          (assoc game-state
                                 :board board'
                                 :player-on-move next-player
                                 :black-can-castle-ks? false
                                 :black-can-castle-qs? false)))))))

(defmethod get-pseudolegal-destinations :K
  [{:keys [board white-can-castle-ks? white-can-castle-qs?] :as game-state} from-sq]
  (cond-> []
    (and white-can-castle-ks? (castling-possible? game-state :K)) (conj [7 6])
    (and white-can-castle-qs? (castling-possible? game-state :Q)) (conj [7 1])
    :always (into
             (->> all-directions
                  (map #(board/add-squares from-sq %))
                  (filter board/square-on-board?)
                  (remove #(pieces/same-piece-color? :K (board/get-piece board %)))
                  #_(map #(let [board' (board/move-piece board from-sq %)
                              next-player (flip-player (:player-on-move game-state))]
                          (assoc game-state
                                 :board board'
                                 :player-on-move next-player
                                 :white-can-castle-ks? false
                                 :white-can-castle-qs? false)))))))

(defn squares-attacked-by-player [{board :board :as game-state} player]
  (->> board
       board/occupied-squares
       (filter #(= player (pieces/piece-color (board/get-piece board %))))
       (mapcat #(get-pseudolegal-destinations game-state %))
       distinct))

(defn in-check? [{:keys [board player-on-move] :as game-state}]
  (let [attacked-king (get-players-king player-on-move)
        opponent (flip-player player-on-move)]
    (as-> (squares-attacked-by-player game-state opponent) xs
      (map #(board/get-piece board %) xs)
      (set xs)
      (contains? xs attacked-king))))

(defn in-check-after-move? [{board :board :as game-state} from-sq to-sq]
  (in-check? (assoc game-state :board (board/move-piece board from-sq to-sq))))

(defn get-legal-destinations [game-state from-sq]
  (->> (get-pseudolegal-destinations game-state from-sq)
       (remove #(in-check-after-move? game-state from-sq %))))

(defn check-mate? [{:keys [board player-on-move] :as game-state}]
  (->> board
       board/occupied-squares
       (filter #(= (pieces/piece-color (board/get-piece board %)) player-on-move))
       (mapcat #(get-legal-destinations game-state %))
       distinct
       empty?))


(defn possible-moves [{:keys [board player-on-move] :as game-state}]
  (->> board/all-squares
       (filter #(= (pieces/piece-color (board/get-piece board %)) player-on-move))
       (mapcat (fn [from-sq] (map (fn [to-sq] [from-sq to-sq]) (get-legal-destinations game-state from-sq))))))

(defn make-move [{:keys [board player-on-move] :as game-state} from-sq to-sq]
  (let [board' (board/move-piece board from-sq to-sq)
        next-player (flip-player player-on-move)]
    (assoc game-state
           :board (cond
                    (and (= (board/get-piece board from-sq) :k)
                         (= from-sq [0 4])
                         (= to-sq [0 1])) (board/move-piece board' [0 0] [0 2])
                    (and (= (board/get-piece board from-sq) :k)
                         (= from-sq [0 4])
                         (= to-sq [0 6])) (board/move-piece board' [0 7] [0 5])
                    (and (= (board/get-piece board from-sq) :K)
                         (= from-sq [7 4])
                         (= to-sq [7 1])) (board/move-piece board' [7 0] [7 2])
                    (and (= (board/get-piece board from-sq) :K)
                         (= from-sq [7 4])
                         (= to-sq [7 6])) (board/move-piece board' [7 7] [7 5])
                    (and (= (board/get-piece board from-sq) :p)
                         (= (first to-sq) 7)) (board/place-piece board' to-sq :q)
                    (and (= (board/get-piece board from-sq) :P)
                         (= (first to-sq) 0)) (board/place-piece board' to-sq :Q)
                    :else board')
           :player-on-move next-player
           :white-can-castle-ks? (if (or (= (:white-can-castle-ks? game-state) false)
                                         (= from-sq [7 4])
                                         (= from-sq [7 7])
                                         (= to-sq [7 7]))
                                   false
                                   true)
           :white-can-castle-qs? (if (or (= (:white-can-castle-qs? game-state) false)
                                         (= from-sq [7 4])
                                         (= from-sq [7 0])
                                         (= to-sq [7 0]))
                                   false
                                   true)
           :black-can-castle-ks? (if (or (= (:black-can-castle-ks? game-state) false)
                                         (= from-sq [0 4])
                                         (= from-sq [0 7])
                                         (= to-sq [0 7]))
                                   false
                                   true)
           :black-can-castle-qs? (if (or (= (:black-can-castle-qs? game-state) false)
                                         (= from-sq [0 4])
                                         (= from-sq [0 0])
                                         (= to-sq [0 0]))
                                   false
                                   true))))
