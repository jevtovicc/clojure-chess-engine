(ns clojure-chess-engine.core
  (:require [clojure.string :as str])
  (:import
   (java.awt Dimension GridLayout Color Image)
   (java.awt.event ActionListener)
   (javax.swing JFrame JButton JPanel UIManager ImageIcon)
   (javax.swing.border LineBorder)))

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

(def brown-color (Color. 0x493323))
(def white-color (Color. 0xE1BC91))
(def green-color (Color. 0x7FFFD4))

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

(defn str->square [s]
  (map #(Integer/parseInt %) (str/split s #"-")))

(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

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

(def last-clicked-button (atom nil))
(def available-positions (atom #{}))

(declare update-board-buttons)

(defn draw-board [board-pane board-buttons]
  (doseq [i (range 8)
          j (range 8)
          :let [button (board-buttons [i j])]]
    (.add board-pane button)))

(defn handle-click [e]
  (let [button (.getSource e)
        square (str->square (.getName button))]
    (if (nil? @last-clicked-button)
      (let [previous-avaialbe-positions @available-positions]
        (reset! available-positions (get-pseudolegal-destinations initial-board square))
        (reset! last-clicked-button button)
        (.setBorder @last-clicked-button (LineBorder. green-color 4))
        (update-board-buttons previous-avaialbe-positions @available-positions))
      (if (square-empty? initial-board square)
        (do
          (.setBorder @last-clicked-button (LineBorder. nil))
          (reset! last-clicked-button nil))
        (let [previous-avaialbe-positions @available-positions]
          (reset! available-positions (get-pseudolegal-destinations initial-board square))
          (.setBorder @last-clicked-button (LineBorder. nil))
          (reset! last-clicked-button button)
          (.setBorder @last-clicked-button (LineBorder. green-color 4))
          (update-board-buttons previous-avaialbe-positions @available-positions))))))

(def buttons
  (into {} (for [i (range 8)
                 j (range 8)
                 :let [square-color (rank-file->square-color i j)]]
             [[i j] (doto (JButton.)
                      (.setName (str i "-" j))
                      (.setBackground square-color)
                      (.addActionListener (reify ActionListener
                                            (actionPerformed [this e] (handle-click e)))))])))

(defn remove-circles [positions]
  (doseq [square positions
          :let [button (buttons square)]]
    (.setIcon button nil)))

(defn add-circles [positions]
  (doseq [square positions
          :let [button (buttons square)]]
    (.setIcon button
              (ImageIcon. (.getScaledInstance
                           (.getImage
                            (ImageIcon. "resources/images/full-red-circle.png"))
                           30 30 Image/SCALE_DEFAULT)))))

(defn update-board-buttons [previous-available-positions available-positions]
  (remove-circles previous-available-positions)
  (add-circles available-positions))

(defn proba-tabla [board available-positions]
  (doseq [entry buttons
          :let [[i j] (key entry)
                button (val entry)
                piece (get-piece board [i j])]]
    (.setIcon button (ImageIcon.
                      (if (contains? available-positions [i j])
                        (.getScaledInstance
                         (.getImage
                          (ImageIcon. "resources/images/full-red-circle.png"))
                         30 30 Image/SCALE_DEFAULT)
                        (piece->imgsrc piece))))))

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (let [my-frame (doto (JFrame. "Chess Game")
                   (.setLocationRelativeTo nil)
                   (.setSize 1000 700))
        board-pane (doto (JPanel.)
                     (.setLayout (GridLayout. 8 8))
                     (.setPreferredSize (Dimension. 1000 600))
                     (.setMaximumSize (Dimension. 1000 600)))]
    (proba-tabla initial-board #{})
    (draw-board board-pane buttons)
    (.add my-frame board-pane)
    (.setVisible my-frame true)))

(main)
;; @last-clicked-button
