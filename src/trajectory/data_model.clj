(ns trajectory.data-model
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.matrix.stats :refer [mean]]))

;; a blank space that is very thin is being inserted into the first item when the file is read 

(def UTF8_BOM "\uFEFF") ;; Byte Order Marker needs to be removed

(def input-file "resources/input.csv") ;; input file 

(def locations {"EXTERNAL" {:x 100 :y 200}
                "UG" {:x 100 :y 500}
                "PGT" {:x 300 :y 600}
                "PGR" {:x 400 :y 100}
                "PhD" {:x 700 :y 350}}) ;; locations of the nodes

;;base data to plot and the overlay data to plot
(def data-to-plot (atom {:vertices {} :edges {}}))
(def overlay (atom {:vertices {} :edges {}}))

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

(defn filter-object
  "Returns all rows of an object"
  [object data]
  (filter #(= object (:object %)) data))

(defn object-trajectory
  "compute trajectory of a given object"
  [object data]
  (let [obj (sort-by :step (filter-object object data))
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

(defn system-vertices-state
  "compute the state of all vertices"
  [flt data]
  (let
      [filtered-trajectories
       (if (not (= flt "-1"))
         (filter flt (all-trajectories data))
         (all-trajectories data))
       freq-states (frequencies (apply concat (map #(%1 :trajectory) filtered-trajectories)))]
    (apply merge (map #(hash-map (first %) (hash-map :state (second %))) freq-states))))

(defn edges-of-trajectory
 "Leave as is as the moment but change to a matrix. produce a matrix of two vectors. First vector is the start and sseond the end"
  [trajectory]
  (let
      [start-vertices (take (- (count trajectory) 1) trajectory)
       end-vertices (rest trajectory)]
    (map vector start-vertices end-vertices)))

(defn all-edges
  "Get all the edges from the data"
  [flt data]
  (let
      [filtered-trajectories
       (if (not (= flt "-1"))
         (filter flt (all-trajectories data))
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
    
(defn system-to-plot
  "Data structure to plot"
  [data data-model-atom flt]
  (let
      [vertices
       (->> data
           (system-vertices-state flt)
           (merge-with merge locations)
           (filter #(not (= nil ((second %) :state))))
           (conj {}))
       edges
       (->> data
           (all-edges flt)
           (system-edges-weights))]
    (reset! data-model-atom
          (conj
           {}
           {:vertices vertices}
           {:edges edges}))))
