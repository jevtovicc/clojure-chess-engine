(ns clojure-chess-engine.core
  (:require [seesaw.core :as seesaw]
            [seesaw.border :as ssborder]
            [clojure.string :as str]
            [clojure-chess-engine.fen-parser :as parser]
            [clojure-chess-engine.board :as board]
            [clojure-chess-engine.pieces :as pieces]
            [clojure-chess-engine.rules :as rules])
  (:import
   (javax.swing UIManager ImageIcon)))

(def starting-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq")
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
  [s]
  (map #(Integer/parseInt %) (str/split s #"-")))

(declare handle-click)

(defn generate-starting-buttons []
  (for [i (range 8)
        j (range 8)
        :let [piece (board/get-piece (:board @game-state) [i j])]]
    (seesaw/button
     :id (str i "-" j)
     :background (rank-file->square-color i j)
     :icon (ImageIcon. (pieces/piece->imgsrc piece))
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
  (->> board/all-squares
       (filter #(= (pieces/piece-color (board/get-piece board %)) player-on-move))
       (reduce
        (fn [acc square]
          (assoc acc square (rules/get-legal-destinations game-state square)))
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
    (swap! game-state #(assoc % :board (board/move-piece (:board %) from-sq to-sq) :player-on-move (rules/flip-player (:player-on-move %))))))

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
                                                                piece (board/get-piece (:board @game-state) from-sq)]
                                                            (seesaw/config! e :icon (seesaw/config selected-square :icon))
                                                            (seesaw/config! selected-square :icon nil)
                                                            (untag-legal-squares)
                                                            (untag-selected-square)
                                                            (swap! game-state #(assoc % :board (board/move-piece (:board %) from-sq square) :player-on-move (rules/flip-player (:player-on-move %))))
                                                            (if (and (= piece :k)
                                                                     (= from-sq [0 4])
                                                                     (= square [0 1]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (board/move-piece (:board %) [0 0] [0 2])))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-2]) :icon (seesaw/config (seesaw/select board-pane [:#0-0]) :icon))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-0]) :icon nil)))
                                                            (if (and (= piece :k)
                                                                     (= from-sq [0 4])
                                                                     (= square [0 6]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (board/move-piece (:board %) [0 7] [0 5])))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-5]) :icon (seesaw/config (seesaw/select board-pane [:#0-7]) :icon))
                                                                (seesaw/config! (seesaw/select board-pane [:#0-7]) :icon nil)))
                                                            (if (= piece :k)
                                                              (swap! game-state #(assoc % :black-can-castle-ks? false :black-can-castle-qs? false)))
                                                            (if (and (= piece :K)
                                                                     (= from-sq [7 4])
                                                                     (= square [7 1]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (board/move-piece (:board %) [7 0] [7 2])))
                                                                (seesaw/config! (seesaw/select board-pane [:#7-2]) :icon (seesaw/config (seesaw/select board-pane [:#7-0]) :icon))
                                                                (seesaw/config! (seesaw/select board-pane [:#7-0]) :icon nil)))
                                                            (if (and (= piece :K)
                                                                     (= from-sq [7 4])
                                                                     (= square [7 6]))
                                                              (do
                                                                (swap! game-state #(assoc % :board (board/move-piece (:board %) [7 7] [7 5])))
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
                                                            (if (rules/check-mate? @game-state)
                                                              (do
                                                                (seesaw/alert "Game over!")
                                                                (reset-game))
                                                              (generate-random-move)))
      (board/square-empty? (:board @game-state) square) (do (untag-legal-squares)
                                                      (untag-selected-square))
      :else (let [legal-moves (rules/get-legal-destinations @game-state square)]
              (untag-legal-squares)
              (untag-selected-square)
              (seesaw/config! e :class :square-selected :border (ssborder/line-border :thickness 4 :color :green))
              (tag-legal-squares legal-moves)))))

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (seesaw/show! my-frame))

(main)
