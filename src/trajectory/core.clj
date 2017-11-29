(ns trajectory.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; a blank space that is very thin is being inserted into the first item when the file is read 

(def UTF8_BOM "\uFEFF") ;; Byte Order Marker needs to be removed

(def input-file "resources/input.csv")

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

(defn object-trajectory
  "compute all distinct trajectories and a count of each"
  [object data]
  (let [obj (filter-object object data)] 
  (merge
   {:object object}
   (zipmap (map #(%1 :step) obj) (map #(%1 :state) obj)))))
