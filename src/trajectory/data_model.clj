(ns trajectory.data-model
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.matrix.stats :refer [mean]]))

;; a blank space that is very thin is being inserted into the first item when the file is read 

(def UTF8_BOM "\uFEFF") ;; Byte Order Marker needs to be removed

(def input-file "resources/input.csv") ;; input file 

(def locations {"Line 0" {:x 100 :y 400}
                "Line 1" {:x 300 :y 600}
                "Line 2" {:x 400 :y 200}
                "Line 3" {:x 700 :y 200}}) ;; this is the locations of the nodes

(def data-to-plot (atom {:vertices {} :edges {}}))

;; file reading functions

(defn debomify
  "Will remove the BOM char at the start of a string"
  [^String line]
  (let [bom UTF8_BOM]
    (if (.startsWith line bom)
      (.substring line 1)
      line)))

(defn debommed-line-seq
  "Produces a lazy line-seq that has removed BOM char at start of first line, 
  if it exists"
  [^java.io.BufferedReader rdr]
  (when-let [line (.readLine rdr)]
    (cons (debomify line) (lazy-seq (line-seq rdr)))))

(defn read-file
  "Read in the file and use the process-line function to process each line"
  [file line-func line-acc]
  (with-open [rdr (io/reader file)]
    (reduce line-func line-acc (debommed-line-seq rdr))))

(defn process-line
  "Process a line from the file, and then write it to an accumulator variable to to write out. This function should contain any preprocessing"
  [acc line]
  (let [split-line (str/split line #",")
        processed-line (list
                        (nth split-line 0)
                        (read-string (nth split-line 1))
                        (nth split-line 2))
        add-keys (zipmap [:object :step :vertex] processed-line)]
    (conj acc add-keys)))

(defn read-input-file-return-data
  "A wrapper function to read the input-file with the process-line function and return the data"
  []
  (read-file input-file process-line []))

;; Analysis functions

(defn distinct-objects
  "Returns a map of all distinct objects"
  [data]
  (distinct (map #(:object %1) data)))

(defn distinct-vertices
  "Returns a map of all distinct objects"
  [data]
  (distinct (map #(:vertex %1) data)))

(defn filter-object
  "Returns all rows of an object"
  [object data]
  (filter #(= object (:object %)) data))

(defn sort-by-step
  "sort all rows by step"
  [data]
  (sort-by :step data))

(defn object-trajectory
  "compute trajectory of a given object"
  [object data]
  (let [obj (sort-by-step (filter-object object data))
        all-states (map #(%1 :vertex) obj)
        all-steps (map #(%1 :step) obj)
        trajectory (reduce (fn [a b] (conj a (second b)))
                           []
                           (sort-by first (zipmap all-steps all-states)))]
    (conj
     {:object object}
     {:trajectory trajectory})))

(defn all-trajectories
  "compute all trajectories"
  [data]
  (map #(object-trajectory %1 data) (distinct-objects data)))

(defn mean-position-in-sequence
  "Where is the average position of a state amongst all the sequence"
  [state data]
  (mean
   (map
    #(:step %1)
    (filter #(= (:vertex %1) state) data))))

;; we can compute the vertex state far more easilt using frequencies - implement this
(defn compute-vertex-state
  "computer vertex state. State of vertex is the number of occurences of the vertex"
  [vertex-name data]
  (count (filter #(= (:vertex %1) vertex-name) data)))

(defn system-vertices-state
  "compute the state of all vertices"
  [data]
  (apply merge
         (map #(hash-map %1 (hash-map :state (compute-vertex-state %1 data))) (distinct-vertices data))))

(defn edges-of-trajectory
 "Leave as is as the moment but change to a matrix. produce a matrix of two vectors. First vector is the start and sseond the end"
  [trajectory]
  (let
      [start-vertices (take (- (count trajectory) 1) trajectory)
       end-vertices (rest trajectory)]
    (map vector start-vertices end-vertices)))

(defn all-edges
  "Get all the edges from the data"
  [gross-filter data]
  (let
      [filtered-trajectories
       (if (not (= gross-filter "-1"))
         (filter #(some (fn [a] (= gross-filter a)) (% :trajectory)) (all-trajectories data))
         (all-trajectories data))]
    (mapcat #(edges-of-trajectory (% :trajectory)) filtered-trajectories)))

(defn system-edges-weights
  "compute the weight of all the edges by doing a distinct-count of all the edges"
  [edges]
  (let [edge-state (frequencies edges)]
    (map (fn [[[a b] c]]
           (hash-map
            :start (first (filter #(= (first %) a) locations))
            :end (first (filter #(= (first %) b) locations))
            :width c))
         edge-state)))
    
  ;; then we need to get the x and y location of the start and the ends for the purpose of plotting and we are good to go
  
;; calculate the overall system to plot 
(defn system-to-plot
  "contains the data structure to plot, data is the output of the file read. Need to redefine this parameter"
  [data data-model-atom gross-filter]
  (reset! data-model-atom
          (conj
           {}
           {:vertices (merge-with merge locations (system-vertices-state data))}
           {:edges (system-edges-weights (all-edges gross-filter data))})))
