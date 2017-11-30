(ns trajectory.core
  (:gen-class)
  (:require [trajectory.swing.plotter :as plotter]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.matrix.stats :as stats]))

;; a blank space that is very thin is being inserted into the first item when the file is read 

(def UTF8_BOM "\uFEFF") ;; Byte Order Marker needs to be removed

(def input-file "resources/input.csv") ;; input file 

(def locations [{"Line 0" [:x 10 :y 10]}
                {"Line 1" [:x 50 :y 50]}
                {"Line 2" [:x 70 :y 70]}
                {"Line 3" [:x 100 :y 100]}]) ;; this is the locations of the nodes

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
  "Process a line from the file, process the line and then write it to an 
  accumulator variable to to write out. This function should contain any 
  preprocessing"
  [acc line]
  (let [split-line (str/split line #",")
        processed-line (list
                        (nth split-line 0)
                        (read-string (nth split-line 1))
                        (nth split-line 2))
        add-keys (zipmap [:object :step :state] processed-line)]
    (conj acc add-keys)))

(defn distinct-objects
  "Returns a map of all distinct objects"
  [data]
  (distinct (map #(:object %1) data)))

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
        all-states (map #(%1 :state) obj)
        all-steps (map #(%1 :step) obj)
        trajectory (apply str (reduce concat all-states))] 
  (merge
   {:object object}
   (zipmap all-steps all-states)
   {:trajectory-string trajectory})))

(defn all-trajectories
  "compute all trajectories"
  [data]
  (map #(object-trajectory %1 data) (distinct-objects data)))

(defn mean-position-in-sequence
  "Where is the average position of a state amongst all the sequence"
  [state data]
  (stats/mean
   (map
    #(:step %1)
    (filter #(= (:state %1) state) data))))

