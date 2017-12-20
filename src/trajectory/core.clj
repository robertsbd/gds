(ns trajectory.core
  (:gen-class)
  (:require  [trajectory.data-model :as dm]
;;             [trajectory.plotter :as pl]
             [seesaw.core :as sc]
             [seesaw.graphics :as sg]
             [seesaw.color :as scol]
             ))

(def circle-style-base
  (sg/style :foreground (scol/color :lightblue) :background (scol/color :lightblue)))

(def circle-style-foreground
  (sg/style :foreground (scol/color :darkblue) :background (scol/color :darkblue)))


(defn centred-circle-with-label
  "a circle centred on the coordinates given with an optional label"
  [g x y radius circ-style & {:keys [label] :or {label ""}}]
  (let [width (* 2 radius)]
    (sg/draw g
             (sg/ellipse (- x radius) (- y radius) width width) circ-style
             (sg/string-shape x (+ y radius 15) label))))

(defn arrow
  "an arrow"
  [g x1 y1 x2 y2 edge-width arrow-width vertex-radius col]
  (let ;; bunch of trig to make the line intersect with the vertex circle
      [angle-a (Math/atan2 (- y2 y1) (- x2 x1))
       angle-b (- (/ Math/PI 2) angle-a)
       y-adjustment (/ (* vertex-radius (Math/sin angle-a)) (Math/sin (/ Math/PI 2)))
       x-adjustment (Math/sqrt (- (* vertex-radius vertex-radius) (* y-adjustment y-adjustment)))
       x-edge-of-vertex (if (> x2 x1) (- x2 x-adjustment) (+ x2 x-adjustment)) 
       y-edge-of-vertex (- y2 y-adjustment)]
    ;; Draw the arrow head
    (sg/push g
             (sg/translate g x-edge-of-vertex y-edge-of-vertex)
             (sg/rotate g (Math/toDegrees (- angle-a (/ Math/PI 2))))
             (sg/draw g
                      (sg/polygon [0 0] [(- arrow-width) (- (* 2 arrow-width))] [arrow-width (- (* 2 arrow-width))])                      
;;                      (sg/polygon [0 arrow-width] [(- arrow-width) (- arrow-width)] [arrow-width (- arrow-width)])
                      (sg/style :foreground (scol/color col) :background (scol/color col))))
    ;; Draw the arrows line
    (sg/draw g
             (sg/line x1 y1 x-edge-of-vertex y-edge-of-vertex) (sg/style :foreground (scol/color col) :stroke edge-width))))

(defn draw-all-edges
  "draw all the edges of a data model"
  [g data-model arrow-colour]
  (doseq [e (@data-model :edges)] ;; second draw all the edges
    (let [edge-width (e :width)
          x1 ((second (e :start)) :x)
          y1 ((second (e :start)) :y)
          x2 ((second (e :end)) :x)
          y2 ((second (e :end)) :y)
          size-of-end-vertex (* 10 (((@data-model :vertices) (first (e :end))) :state))]
      (arrow g x1 y1 x2 y2 edge-width 7 size-of-end-vertex arrow-colour))))

(defn draw-all-vertices
  "draw all the vertices of a data model"
  [g data-model circle-style show-label]
  (do
    (doseq [d (@data-model :vertices)] ;; first draw all the vertices
      (let [circle-radius ((second d) :state)
            x ((second d) :x)            ;; x and y define the centre of the circle
            y ((second d) :y)
            label (first d)]
        (if show-label 
          (centred-circle-with-label g x y (* 10 circle-radius) circle-style :label label)
          (centred-circle-with-label g x y (* 10 circle-radius) circle-style))))))

(defn paint
  "Our paint function"
  [c g]
  
  ;; paint the basemodel
  (draw-all-edges g dm/data-to-plot :lightgrey)
  (draw-all-vertices g dm/data-to-plot circle-style-base true)

  ;; paint the overlay  
  (draw-all-edges g dm/overlay :black)
  (draw-all-vertices g dm/overlay circle-style-foreground false))

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

(defn plot-model
  []
  (let [data (dm/read-input-file-return-data)]
    (do
      (dm/system-to-plot data dm/data-to-plot "-1")
      (dm/system-to-plot data dm/overlay "-1")
      (create-window!))))

(defn plot-model-PhD
  []
  (let [data (dm/read-input-file-return-data)]
    (do
      (dm/system-to-plot data dm/data-to-plot "-1")
      (dm/system-to-plot data dm/overlay #(some (fn [a] (= a "PhD")) (% :trajectory)))
      (sc/repaint! (sc/select main-window [:#maincanvas])))))

(defn plot-model-filtered
  [filter-list]
  (let [data (dm/read-input-file-return-data)]
    (do
      (dm/system-to-plot data dm/data-to-plot "-1")      
      (dm/system-to-plot data dm/overlay #(clojure.set/subset? (set filter-list) (set (% :trajectory))))
      (sc/repaint! (sc/select main-window [:#maincanvas])))))

