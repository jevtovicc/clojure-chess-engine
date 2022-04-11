(ns clojure-chess-engine.core
  (:import
   (java.awt Dimension GridLayout Color)
   (javax.swing JFrame JButton JPanel UIManager ImageIcon)))

(def initial-fen [[:r :n :b :q :k :b :n :r]
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

(defn rank-file->field-color [rank file]
  (if (zero? (mod (+ rank file) 2))
    white-color
    brown-color))

(defn get-piece [board rank file]
  (get-in board [rank file]))

(defn place-pieces [board-pane]
  (doseq [i (range 8)
          j (range 8)
          :let [field-color (rank-file->field-color i j)
                piece (get-piece initial-fen i j)]]
    (.add board-pane (doto (JButton.)
                       (.setBackground field-color)
                       (.setIcon (ImageIcon. (piece->imgsrc piece)))))))

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (let [my-frame (doto (JFrame. "Chess Game")
                   (.setLocationRelativeTo nil))
        board-pane (doto (JPanel.)
                     (.setLayout (GridLayout. 8 8))
                     (.setPreferredSize (Dimension. 1000 600))
                     (.setMaximumSize (Dimension. 1000 600)))]
    (place-pieces board-pane)
    (.add my-frame board-pane)
    (.setSize my-frame 1000 700)
    (.setVisible my-frame true)))

(main)








