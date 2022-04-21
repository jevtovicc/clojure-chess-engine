(ns spacemacs-clojure.core
  (:require [seesaw.core :as seesaw]
            [seesaw.border :as ssborder]
            [clojure.string :as str])
  (:import
   (javax.swing UIManager ImageIcon)))

(defn parse-row [row]
  (reduce
   (fn [acc ch]
     (if (Character/isDigit ch)
       (let [num (Character/digit ch 10)]
         (into acc (repeat num :e)))
       (conj acc (keyword (str ch)))))
   []
   row))

(defn parse->board [fen]
  (-> fen
      (str/split #" ")
      first
      (str/split #"/")
      (as-> res
          (mapv parse-row res))))

(defn parse-player [fen]
  (let [player (second (str/split fen #" "))]
    (condp = player
      "w" :white
      "b" :black)))

(defn fen->game-state [fen]
  (let [board (parse->board fen)
        player (parse-player fen)]
    {:board board
     :player-on-move player}))

(def starting-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq")
(def starting-game-state (fen->game-state starting-fen))

(def game-state (atom starting-game-state))

(def all-squares
  (for [x (range 8)
        y (range 8)]
    [x y]))

(def white-piece? #{:R :N :B :Q :K :P})
(def black-piece? #{:r :n :b :q :k :p})

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

(def white-color "#efe7e1")
(def brown-color "#A27C5B")

(defn rank-file->square-color [rank file]
  (if (zero? (mod (+ rank file) 2))
    white-color
    brown-color))

(defn get-piece [board square]
  (get-in board square))

(defn remove-piece [board from-sq]
  (assoc-in board from-sq :e))

(defn place-piece [board dest-square piece]
  (assoc-in board dest-square piece))

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

(defn piece-color [p]
  (cond
    (white-piece? p) :white
    (black-piece? p) :black))

(defn same-piece-color? [p1 p2]
  (= (piece-color p1) (piece-color p2)))

(defn get-opponent [player]
  (condp = player
    :white :black
    :black :white))

(defn get-players-king [player]
  (condp = player
    :white :K
    :black :k))

(defn id->square [s]
  (map #(Integer/parseInt %) (str/split s #"-")))

(defmulti get-pseudolegal-destinations (fn [board from-sq] (get-piece board from-sq)))

(defmethod get-pseudolegal-destinations :n
  [board from-sq]
  (->> knight-directions
       (map #(add-squares from-sq %))
       (filter square-on-board?)
       (remove #(same-piece-color? :n (get-piece board %)))
       #_set))

(defmethod get-pseudolegal-destinations :N
  [board from-sq]
  (->> knight-directions
       (map #(add-squares from-sq %))
       (filter square-on-board?)
       (remove #(same-piece-color? :N (get-piece board %)))
       #_set))

(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn get-squares-in-direction [board from-sq dir]
  (let [piece (get-piece board from-sq)]
    (->> (add-squares from-sq dir)
         (iterate #(add-squares % dir))
         (take-while+ #(square-empty? board %))
         (filter square-on-board?)
         (remove #(same-piece-color? piece (get-piece board %))))))

(defn get-squares-in-directions [board from-sq dirs]
  (mapcat #(get-squares-in-direction board from-sq %) dirs))

(defmethod get-pseudolegal-destinations :r
  [board from-sq]
  (get-squares-in-directions board from-sq rook-directions))

(defmethod get-pseudolegal-destinations :R
  [board from-sq]
  (get-squares-in-directions board from-sq rook-directions))

(defmethod get-pseudolegal-destinations :b
  [board from-sq]
  (get-squares-in-directions board from-sq bishop-directions))

(defmethod get-pseudolegal-destinations :B
  [board from-sq]
  (get-squares-in-directions board from-sq bishop-directions))

(defmethod get-pseudolegal-destinations :q
  [board from-sq]
  (get-squares-in-directions board from-sq all-directions))

(defmethod get-pseudolegal-destinations :Q
  [board from-sq]
  (get-squares-in-directions board from-sq all-directions))

(defmethod get-pseudolegal-destinations :p
  [board from-sq]
  (let [not-moved? (= (first from-sq) 1)
        one-up (add-squares dir-up from-sq)
        two-up (add-squares dir-up one-up)
        one-up-right (add-squares dir-up-right from-sq)
        one-up-left (add-squares dir-up-left from-sq)]
    (cond-> []
      (square-empty? board one-up) (conj one-up)
      (and not-moved?
           (square-empty? board one-up)
           (square-empty? board two-up)) (conj two-up)
      (and (not (square-empty? board one-up-left))
           (not (same-piece-color? :p (get-piece board one-up-left)))) (conj one-up-left)
      (and (not (square-empty? board one-up-right))
           (not (same-piece-color? :p (get-piece board one-up-right)))) (conj one-up-right)
      :always (->> (filter square-on-board?)
                   set))))

(defmethod get-pseudolegal-destinations :P
  [board from-sq]
  (let [not-moved? (= (first from-sq) 6)
        one-down (add-squares dir-down from-sq)
        two-down (add-squares dir-down one-down)
        one-down-right (add-squares dir-down-right from-sq)
        one-down-left (add-squares dir-down-left from-sq)]
    (cond-> []
      (square-empty? board one-down) (conj one-down)
      (and not-moved?
           (square-empty? board one-down)
           (square-empty? board two-down)) (conj two-down)
      (and (not (square-empty? board one-down-left))
           (not (same-piece-color? :P (get-piece board one-down-left)))) (conj one-down-left)
      (and (not (square-empty? board one-down-right))
           (not (same-piece-color? :P (get-piece board one-down-right)))) (conj one-down-right)
      :always (->> (filter square-on-board?)))))

(defmethod get-pseudolegal-destinations :k
  [board from-sq]
  (->> all-directions
       (map #(add-squares from-sq %))
       (filter square-on-board?)
       (remove #(same-piece-color? :k (get-piece board %)))))

(defmethod get-pseudolegal-destinations :K
  [board from-sq]
  (->> all-directions
       (map #(add-squares from-sq %))
       (filter square-on-board?)
       (remove #(same-piece-color? :K (get-piece board %)))))

(defn squares-attacked-by-player [board player]
  (->> board
       occupied-squares
       (filter #(= player (piece-color (get-piece board %))))
       (remove #(contains? #{:k :K} (get-piece board %)))
       (mapcat #(get-pseudolegal-destinations board %))
       distinct))

(defn in-check? [board player]
  (let [attacked-king (get-players-king player)
        opponent (get-opponent player)]
    (as-> (squares-attacked-by-player board opponent) xs
      (map #(get-piece board %) xs)
      (set xs)
      (contains? xs attacked-king))))

(defn in-check-after-move? [board player from-sq to-sq]
  (in-check? (move-piece board from-sq to-sq) player))

(defn get-legal-destinations [board player from-sq]
  (->> (get-pseudolegal-destinations board from-sq)
       (remove #(in-check-after-move? board player from-sq %))))

(defn check-mate? [board player]
  (->> board
       occupied-squares
       (filter #(= (piece-color (get-piece board %)) player))
       (mapcat #(get-legal-destinations board player %))
       distinct
       empty?))

(declare handle-click)

(def buttons
  (for [i (range 8)
        j (range 8)
        :let [piece (get-piece (:board @game-state) [i j])]]
    (seesaw/button
     :id (str i "-" j)
     :background (rank-file->square-color i j)
     :icon (ImageIcon. (piece->imgsrc piece))
     :listen [:action handle-click])))

(def board-pane (seesaw/grid-panel
                 :rows 8
                 :columns 8
                 :items buttons))

(def my-frame (seesaw/frame
               :title "Chess Game"
               :content board-pane
               :width 1000
               :height 700))

(defn tag-legal-squares [legal-moves]
  (->> legal-moves
       (map
        (fn [square]
          (let [id (str "#" (str/join "-" square))]
            (seesaw/select board-pane [(keyword id)]))))
       (map #(seesaw/config! % :class :square-legal :border (ssborder/line-border :thickness 3 :color :red)))
       dorun))

(defn untag-legal-squares []
  (let [legal-squares (seesaw/select board-pane [:.square-legal])]
    (dorun (map #(seesaw/config! % :class :square-illegal :border nil) legal-squares))))

(defn untag-selected-square []
  (let [selected-square (first (seesaw/select board-pane [:.square-selected]))]
    (seesaw/config! selected-square :class :square-illegal :border nil)))

(defn switch-player [player]
  (condp = player
    :white :black
    :black :white))

;; Slucajevi
;; 1) Ako nije selektovana figura - selektuj figuru (stavi joj klasu :square-selected)
;; 2) Ako je figura selektovana i kliknuta je figura iste boje - prebaci selektovanu figuru na novu selekciju
;; 3) Ako je figura selektovana i kliknuto je polje koje nije legalno - ukloni selekciju figure
;; 4) Ako je figura selektovana i kliknuto je polje koje je legalno - prebaci figuru na to polje
(defn handle-click [e]
  (let [id (seesaw/config e :id)
        square (id->square (name id))]
    (cond
      (contains? (seesaw/config e :class) "square-legal") (let [selected-square (first (seesaw/select board-pane [:.square-selected]))]
                                                            (seesaw/config! e :icon (seesaw/config selected-square :icon))
                                                            (seesaw/config! selected-square :icon nil)
                                                            (untag-legal-squares)
                                                            (untag-selected-square)
                                                            (swap! game-state #(assoc % :board (move-piece (:board %) (id->square (name (seesaw/config selected-square :id))) square) :player-on-move (switch-player (:player-on-move %))))
                                                            (when (check-mate? (:board @game-state) (:player-on-move @game-state))
                                                              (println "Check mate")
                                                              (seesaw/alert "Game over!")))
      (square-empty? (:board @game-state) square) (do (untag-legal-squares)
                                                      (untag-selected-square))
      :else (let [legal-moves (get-legal-destinations (:board @game-state) (:player-on-move @game-state) square)]
              (untag-legal-squares)
              (untag-selected-square)
              (seesaw/config! e :class :square-selected :border (ssborder/line-border :thickness 4 :color :green))
              (tag-legal-squares legal-moves)))))

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (seesaw/show! my-frame))

(main)
