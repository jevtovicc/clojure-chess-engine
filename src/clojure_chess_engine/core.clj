(ns clojure-chess-engine.core
  (:require [seesaw.core :as seesaw]
            [seesaw.border :as ssborder]
            [clojure.string :as str]
            [clojure-chess-engine.fen-parser :as parser]
            [clojure-chess-engine.board :as board]
            [clojure-chess-engine.pieces :as pieces]
            [clojure-chess-engine.rules :as rules]
            [clojure-chess-engine.ai :as ai])
  (:import
   (javax.swing UIManager ImageIcon)))

(def starting-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq")
;; (def testing-fen "rnbq1bnr/pppp2pp/4ppk1/8/4P1K1/8/PPPP1PPP/RNBQ1BNR w -")
(def starting-game-state (parser/fen->game-state starting-fen))

(def game-state (atom starting-game-state))

(def white-color "#efe7e1")
(def brown-color "#A27C5B")

(defn rank-file->square-color [rank file]
  (if (zero? (mod (+ rank file) 2))
    white-color
    brown-color))

(defn id->square
  "Given a id in form of 'rank-file' returns a two element vector
   in form of [rank file]"
  [id]
  (map #(Integer/parseInt %) (str/split id #"-")))

(defn square->seesawid
  "Given a square in form of [rank file] returns
   an id appropriate for selecting elements using seesaw/select function"
  [square]
  (->> square
       (str/join "-")
       (str "#")
       keyword))

(declare handle-click)

(defn generate-buttons [board]
  (for [i (range 8)
        j (range 8)
        :let [piece (board/get-piece board [i j])]]
    (seesaw/button
     :id (str i "-" j)
     :background (rank-file->square-color i j)
     :icon (ImageIcon. (pieces/piece->imgsrc piece))
     :listen [:action (fn [e] (handle-click e))])))

(def board-pane (seesaw/grid-panel
                 :rows 8
                 :columns 8
                 :items (generate-buttons (:board @game-state))))

(def my-frame (seesaw/frame
               :title "Chess Game"
               :content board-pane
               :width 1000
               :height 700))

(defn draw-board [board]
  (seesaw/config! board-pane :items (generate-buttons board)))

(defn tag-legal-squares [legal-squares]
  (->> legal-squares
       (map
        (fn [square]
          (let [id (square->seesawid square)]
            (seesaw/select board-pane [id]))))
       (run! #(seesaw/config! % :class :square-legal :border (ssborder/line-border :thickness 3 :color :red)))))

(defn untag-legal-squares []
  (let [legal-squares (seesaw/select board-pane [:.square-legal])]
    (run! #(seesaw/config! % :class :square-illegal :border nil) legal-squares)))

(defn untag-selected-square []
  (let [selected-square (first (seesaw/select board-pane [:.square-selected]))]
    (seesaw/config! selected-square :class :square-illegal :border nil)))

(defn reset-game-state []
  (reset! game-state starting-game-state))

(defn reset-game []
  (reset-game-state)
  (seesaw/config! board-pane :items (generate-buttons (:board @game-state))))

(defn disable-buttons []
  (run! #(seesaw/config! % :enabled? false) (seesaw/select board-pane [:JButton])))

(defn enable-buttons []
  (run! #(seesaw/config! % :enabled? true) (seesaw/select board-pane [:JButton])))

(defn make-random-move [game-state]
  (let [moves (rules/possible-moves game-state)
        [from-sq to-sq] (rand-nth moves)]
    (rules/make-move game-state from-sq to-sq)))

(defn make-ai-move [game-state]
  (let [[_ best-move] (ai/minimax-min game-state 4)
        [from-sq to-sq] best-move]
    (rules/make-move game-state from-sq to-sq)))

;; Slucajevi
;; 1) Ako nije selektovana figura - selektuj figuru (stavi joj klasu :square-selected)
;; 2) Ako je figura selektovana i kliknuta je figura iste boje - prebaci selektovanu figuru na novu selekciju
;; 3) Ako je figura selektovana i kliknuto je polje koje nije legalno - ukloni selekciju figure
;; 4) Ako je figura selektovana i kliknuto je polje koje je legalno - prebaci figuru na to polje
(defn handle-click [e]
  (let [id (seesaw/config e :id)
        square (id->square (name id))]
    (when (= (:player-on-move @game-state) :white)
      (cond
      ;; Case 4: If clicked on a legal square, move the selected piece.
        (contains? (seesaw/config e :class) "square-legal")
        (let [selected-square (first (seesaw/select board-pane [:.square-selected]))
              from-sq (id->square (name (seesaw/config selected-square :id)))]
        ;; Make the player's move and immediately update the board.
          (swap! game-state #(rules/make-move % from-sq square))
          (draw-board (:board @game-state))

        ;; After player's move, check for checkmate before AI moves.
          (if (rules/check-mate? @game-state)
            (do
              (seesaw/alert "Game over!")
              (reset-game))
          ;; Start AI move in a background thread.
            (future
              (swap! game-state make-ai-move)
              (seesaw/invoke-later
               (draw-board (:board @game-state))
               (when (rules/check-mate? @game-state)
                 (seesaw/alert "Game over!")
                 (reset-game))))))

      ;; Case 3: If clicked on a non-legal square, unselect the piece.
        (board/square-empty? (:board @game-state) square)
        (do
          (untag-legal-squares)
          (untag-selected-square))

      ;; Case 1 & 2: Select or re-select a piece and tag its legal moves.
        :else
        (let [legal-moves (rules/get-legal-destinations @game-state square)]
          (untag-legal-squares)
          (untag-selected-square)
          (seesaw/config! e :class :square-selected :border (ssborder/line-border :thickness 4 :color :green))
          (tag-legal-squares legal-moves))))))

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (seesaw/show! my-frame))

(main)


@game-state