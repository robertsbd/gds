(ns trajectory.core
  (:gen-class)
  (:require  [trajectory.data-model :as dm]
             [seesaw.core :as sc]
             [seesaw.graphics :as sg]
             [seesaw.color :as scol]))

(defn paint
  "Our paint function"
  [c g]
  (doseq [d (@dm/data-to-plot :vertices)]
    (let [x ((second d) :x)
          y ((second d) :y)
          width (* ((second d) :state) 10)
          label (first d)]
      (sg/draw g
               (sg/ellipse x y width width) (sg/style :foreground (scol/color :black) :background (scol/color :white))
               (sg/string-shape x (- y 5) label))
      )))

(def main-canvas
  "The canvas"
  (sc/canvas :id :maincanvas
             :background (scol/color :white)
             :paint paint))

(def main-window
  "create the window to render the frames in"
  (sc/frame :title "Graph Dynamical System"
            :content main-canvas))

(defn create-window!
  "Create a window"
  []
  (-> main-window
      (sc/show!)
      (sc/config! :size [800 :by 800])))

(defn plot-data-model
  []
  (let [data (dm/read-input-file-return-data)
        trajectories (dm/all-trajectories data)]
    (do
      (dm/system-to-plot data dm/data-to-plot)
      (create-window!))))
