(ns trajectory.plotter
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

(defn new-end-points
  "modify the length of a line and provide new x y coords to avoid overlapping the circle around a point to which we are plotting"
  [size-of-circle x1 y1 x2 y2]
  (let
      [length (math/sqrt (+ (* (- y1 y2) (- y1 y2)) (* (- x1 x2) (- x1 x2))))
       radius-of-cirle (/ size-of-circle 2)
       new-length (- length radius-of-circle 5)]
    
    
