(ns spacemacs-clojure.core
  (:require [seesaw.core :as seesaw]
            [seesaw.border :as ssborder]
            [clojure.string :as str])
  (:import
   (javax.swing UIManager ImageIcon)))

(def initial-board [[:r :n :b :q :k :b :n :r]
                    [:p :p :p :p :p :p :p :p]
                    [:e :e :e :e :e :e :e :e]
                    [:e :e :e :e :e :e :e :e]
                    [:e :e :e :e :e :e :e :e]
                    [:e :e :e :e :e :e :e :e]
                    [:P :P :P :P :P :P :P :P]
                    [:R :N :B :Q :K :B :N :R]])

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

(defn square-empty? [board square]
  (= (get-piece board square) :e))

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
         (split-with #(square-empty? board %))
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

(declare handle-click)

(def buttons
  (for [i (range 8)
        j (range 8)
        :let [piece (get-piece initial-board [i j])]]
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

;; Slucajevi
;; 1) Ako nije selektovana figura - selektuj figuru (stavi joj klasu :square-selected)
;; 2) Ako je figura selektovana i kliknuta je figura iste boje - prebaci selektovanu figuru na novu selekciju
;; 3) Ako je figura selektovana i kliknuto je polje koje nije legalno - ukloni selekciju figure
;; 4) Ako je figura selektovana i kliknuto je polje koje je legalno - prebaci figuru na to polje
(defn handle-click [e]
  (let [id (seesaw/config e :id)
        square (id->square (name id))]
    (cond
      (and (square-empty? initial-board square)
           (not= (seesaw/config e :class) :square-legal)) (do (untag-legal-squares)
                                                              (untag-selected-square))
      :else (let [legal-moves (get-pseudolegal-destinations initial-board square)]
              (untag-legal-squares)
              (untag-selected-square)
              (seesaw/config! e :class :square-selected :border (ssborder/line-border :thickness 4 :color :green))
              (tag-legal-squares legal-moves)))))

(seesaw/select board-pane [:.square-selected])

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (seesaw/show! my-frame))

(main)
