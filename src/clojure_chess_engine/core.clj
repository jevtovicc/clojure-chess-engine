(ns clojure-chess-engine.core
  (:import
   (java.awt Dimension GridLayout Color)
   (javax.swing JFrame JButton JPanel UIManager)))

(def brown-color (Color. 0x493323))
(def white-color (Color. 0xE1BC91))

(defn file-rank->field-color [file rank]
  (if (zero? (mod (+ file rank) 2))
    white-color
    brown-color))

(defn- main []
  (UIManager/setLookAndFeel (UIManager/getCrossPlatformLookAndFeelClassName))
  (let [my-frame (doto (JFrame. "Chess Game")
                   (.setLocationRelativeTo nil))
        board-pane (doto (JPanel.)
                     (.setLayout (GridLayout. 8 8))
                     (.setPreferredSize (Dimension. 1000 600))
                     (.setMaximumSize (Dimension. 1000 600)))]
    (doseq [i (range 8)
            j (range 8)
            :let [field-color (file-rank->field-color i j)]]
      (.add board-pane (doto (JButton.)
                         (.setBackground field-color))))
    (.add my-frame board-pane)
    (.setSize my-frame 1200 700)
    (.setVisible my-frame true)))

(main)








