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

(defn parse-castling [fen]
  (last (str/split fen #" ")))

(defn fen->game-state [fen]
  (let [board (parse->board fen)
        player (parse-player fen)
        castling-info (parse-castling fen)]
    {:board board
     :player-on-move player
     :white-can-castle-ks? (str/includes? castling-info "K")
     :white-can-castle-qs? (str/includes? castling-info "Q")
     :black-can-castle-ks? (str/includes? castling-info "k")
     :black-can-castle-qs? (str/includes? castling-info "q")}))

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

(defmulti get-pseudolegal-destinations (fn [game-state from-sq] (get-piece (:board game-state) from-sq)))

(defmethod get-pseudolegal-destinations :n
  [{board :board} from-sq]
  (->> knight-directions
       (map #(add-squares from-sq %))
       (filter square-on-board?)
       (remove #(same-piece-color? :n (get-piece board %)))))

(defmethod get-pseudolegal-destinations :N
  [{board :board} from-sq]
  (->> knight-directions
       (map #(add-squares from-sq %))
       (filter square-on-board?)
       (remove #(same-piece-color? :N (get-piece board %)))))

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
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq rook-directions))

(defmethod get-pseudolegal-destinations :R
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq rook-directions))

(defmethod get-pseudolegal-destinations :b
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq bishop-directions))

(defmethod get-pseudolegal-destinations :B
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq bishop-directions))

(defmethod get-pseudolegal-destinations :q
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq all-directions))

(defmethod get-pseudolegal-destinations :Q
  [{board :board} from-sq]
  (get-squares-in-directions board from-sq all-directions))

(defmethod get-pseudolegal-destinations :p
  [{board :board} from-sq]
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
  [{board :board} from-sq]
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
       (every? #(square-empty? board %))))

(defn my-any?
  "Returns true if some element in coll satisfies predicate"
  [pred col]
  (not (not-any? pred col)))

(defn castling-squares-attacked? [{player-on-move :player-on-move :as game-state} side]
  (let [attacked-squares (squares-attacked-by-player game-state (get-opponent player-on-move))
        castling-squares (castling-squares side)]
    (my-any? castling-squares attacked-squares)))

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
                  (map #(add-squares from-sq %))
                  (filter square-on-board?)
                  (remove #(same-piece-color? :k (get-piece board %)))))))

(defmethod get-pseudolegal-destinations :K
  [{:keys [board white-can-castle-ks? white-can-castle-qs?] :as game-state} from-sq]
  (cond-> []
    (and white-can-castle-ks? (castling-possible? game-state :K)) (conj [7 6])
    (and white-can-castle-qs? (castling-possible? game-state :Q)) (conj [7 1])
    :always (into
             (->> all-directions
                  (map #(add-squares from-sq %))
                  (filter square-on-board?)
                  (remove #(same-piece-color? :K (get-piece board %)))))))

(defn squares-attacked-by-player [{board :board :as game-state} player]
  (->> board
       occupied-squares
       (filter #(= player (piece-color (get-piece board %))))
       (remove #(contains? #{:k :K} (get-piece board %)))
       (mapcat #(get-pseudolegal-destinations game-state %))
       distinct))

(defn in-check? [{:keys [board player-on-move] :as game-state}]
  (let [attacked-king (get-players-king player-on-move)
        opponent (get-opponent player-on-move)]
    (as-> (squares-attacked-by-player game-state opponent) xs
      (map #(get-piece board %) xs)
      (set xs)
      (contains? xs attacked-king))))

(defn in-check-after-move? [{board :board :as game-state} from-sq to-sq]
  (in-check? (assoc game-state :board (move-piece board from-sq to-sq))))

(defn get-legal-destinations [game-state from-sq]
  (->> (get-pseudolegal-destinations game-state from-sq)
       (remove #(in-check-after-move? game-state from-sq %))))

(defn check-mate? [{:keys [board player-on-move] :as game-state}]
  (->> board
       occupied-squares
       (filter #(= (piece-color (get-piece board %)) player-on-move))
       (mapcat #(get-legal-destinations game-state %))
       distinct
       empty?))

(declare handle-click)

(defn generate-starting-buttons []
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
                 :items (generate-starting-buttons)))

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

(defn reset-game-state []
  (reset! game-state starting-game-state))

(defn reset-game []
  (reset-game-state)
  (seesaw/config! board-pane :items (generate-starting-buttons)))

(defn possible-moves [{:keys [board player-on-move] :as game-state}]
  (->> all-squares
       (filter #(= (piece-color (get-piece board %)) player-on-move))
       (reduce
        (fn [acc square]
          (assoc acc square (get-legal-destinations game-state square)))
        {})
       (remove (fn [move] (empty? (val move))))
       (into {})))

(defn disable-buttons []
  (dorun (map #(seesaw/config! % :enabled? false) (seesaw/select board-pane [:JButton]))))

(defn enable-buttons []
  (dorun (map #(seesaw/config! % :enabled? true) (seesaw/select board-pane [:JButton]))))

(defn square->seesawid [square]
  (->> square
       (str/join "-")
       (str "#")
       keyword))

(defn generate-random-move []
  (let [moves (possible-moves @game-state)
        from-sq (rand-nth (keys moves))
        to-sq (rand-nth (moves from-sq))
        src-button (seesaw/select board-pane [(square->seesawid from-sq)])
        dest-button (seesaw/select board-pane [(square->seesawid to-sq)])]
    (seesaw/config! dest-button :icon (seesaw/config src-button :icon))
    (seesaw/config! src-button :icon nil)
    (swap! game-state #(assoc % :board (move-piece (:board %) from-sq to-sq) :player-on-move (get-opponent (:player-on-move %))))))

;; Slucajevi
;; 1) Ako nije selektovana figura - selektuj figuru (stavi joj klasu :square-selected)
;; 2) Ako je figura selektovana i kliknuta je figura iste boje - prebaci selektovanu figuru na novu selekciju
;; 3) Ako je figura selektovana i kliknuto je polje koje nije legalno - ukloni selekciju figure
;; 4) Ako je figura selektovana i kliknuto je polje koje je legalno - prebaci figuru na to polje
(defn handle-click [e]
  (let [id (seesaw/config e :id)
        square (id->square (name id))]
    (cond
      (contains? (seesaw/config e :class) "square-legal") (let [selected-square (first (seesaw/select board-pane [:.square-selected]))
                                                                from-sq (id->square (name (seesaw/config selected-square :id)))
                                                                piece (get-piece (:board @game-state) from-sq)]
                                                            (seesaw/config! e :icon (seesaw/config selected-square :icon))
                                                            (seesaw/config! selected-square :icon nil)
                                                            (untag-legal-squares)
                                                            (untag-selected-square)
                                                            (swap! game-state #(assoc % :board (move-piece (:board %) from-sq square) :player-on-move (get-opponent (:player-on-move %))))
                                                            (if (and (= piece :k)
                                                                     (= from-sq [0 4])
                                                                     (= square [0 1]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (move-piece (:board %) [0 0] [0 2])))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-2]) :icon (seesaw/config (seesaw/select board-pane [:#0-0]) :icon))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-0]) :icon nil)))
                                                            (if (and (= piece :k)
                                                                     (= from-sq [0 4])
                                                                     (= square [0 6]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (move-piece (:board %) [0 7] [0 5])))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-5]) :icon (seesaw/config (seesaw/select board-pane [:#0-7]) :icon))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-7]) :icon nil)))
                                                            (if (= piece :k)
                                                              (swap! game-state #(assoc % :black-can-castle-ks? false :black-can-castle-qs? false)))
                                                            (if (and (= piece :K)
                                                                     (= from-sq [7 4])
                                                                     (= square [7 1]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (move-piece (:board %) [7 0] [7 2])))
                                                                (seesaw/config! (seesaw/select board-pane [:#7-2]) :icon (seesaw/config (seesaw/select board-pane [:#7-0]) :icon))
                                                                (seesaw/config! (seesaw/select board-pane [:#7-0]) :icon nil)))
                                                            (if (and (= piece :K)
                                                                     (= from-sq [7 4])
                                                                     (= square [7 6]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (move-piece (:board %) [7 7] [7 5])))
                                                                (seesaw/config! (seesaw/select board-pane [:#7-5]) :icon (seesaw/config (seesaw/select board-pane [:#7-7]) :icon))
                                                                (seesaw/config! (seesaw/select board-pane [:#7-7]) :icon nil)))
                                                            (if (= piece :K)
                                                              (swap! game-state #(assoc % :white-can-castle-ks? false :white-can-castle-qs? false)))
                                                            (if (= from-sq [0 0])
                                                              (swap! game-state #(assoc % :black-can-castle-qs? false)))
                                                            (if (= from-sq [0 7])
                                                              (swap! game-state #(assoc % :black-can-castle-ks? false)))
                                                            (if (= from-sq [7 0])
                                                              (swap! game-state #(assoc % :white-can-castle-qs? false)))
                                                            (if (= from-sq [7 7])
                                                              (swap! game-state #(assoc % :white-can-castle-ks? false)))
                                                            (if (check-mate? @game-state)
                                                              (do
                                                                (seesaw/alert "Game over!")
                                                                (reset-game))
                                                              (generate-random-move)))
      (square-empty? (:board @game-state) square) (do (untag-legal-squares)
                                                      (untag-selected-square))
      :else (let [legal-moves (get-legal-destinations @game-state square)]
              (untag-legal-squares)
              (untag-selected-square)
              (seesaw/config! e :class :square-selected :border (ssborder/line-border :thickness 4 :color :green))
              (tag-legal-squares legal-moves)))))

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (seesaw/show! my-frame))

(main)
