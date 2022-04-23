(ns clojure-chess-engine.fen-parser
  (:require [clojure.string :as str]))

(defn parse-row
  "Given a string representing one row, returns a vector of keywords representing pieces"
  [row]
  (reduce
   (fn [acc ch]
     (if (Character/isDigit ch)
       (let [num (Character/digit ch 10)]
         (into acc (repeat num :e)))
       (conj acc (keyword (str ch)))))
   []
   row))

(defn parse->board
  "Given a FEN, returns a board represented as a 2D vector of keywords representing pieces"
  [fen]
  (mapv parse-row (-> fen
                      (str/split #" ")
                      first
                      (str/split #"/"))))

(defn parse-player
  "Given a FEN, returns which player is on move"
  [fen]
  (let [player (second (str/split fen #" "))]
    (condp = player
      "w" :white
      "b" :black)))

(defn parse-castling
  "Given a FEN, returns castling possibilities"
  [fen]
  (let [castling-info (last (str/split fen #" "))]
    {:white-can-castle-ks? (str/includes? castling-info "K")
     :white-can-castle-qs? (str/includes? castling-info "Q")
     :black-can-castle-ks? (str/includes? castling-info "k")
     :black-can-castle-qs? (str/includes? castling-info "q")}))

(defn fen->game-state [fen]
  (let [board (parse->board fen)
        player (parse-player fen)
        castling-possibilities (parse-castling fen)]
    (merge {:board board :player-on-move player} castling-possibilities)))
