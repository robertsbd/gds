(ns trajectory.swing.plotter
  (:gen-class)
  (:require [seesaw.core :as sc]
            [seesaw.graphics :as sg]
            [seesaw.color :as scol]))

(defn paint
  "Our paint function"
  [c g]
        (sg/draw g
                 (sg/rect 0 0 10 10)
                 (sg/style :background (scol/color :red))))

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
;;      (sc/full-screen!)
      (sc/show!)
      (sc/config! :size [200 :by 200]))


































