(ns trajectory.data-model
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.matrix.stats :refer [mean]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file reading functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn debomify
  "Will remove the BOM char at the start of a string"
  [^String line]
    (if (.startsWith line "\uFEFF") (.substring line 1) line))

(defn debommed-line-seq
  "Produces a lazy line-seq that has removed BOM char at start of first line, if it exists"
  [^java.io.BufferedReader rdr]
  (when-let [line (.readLine rdr)]
    (cons (debomify line) (lazy-seq (line-seq rdr)))))

(defn read-file
  "Read in the file. Processing each line, and write it to an accumulator"
  [process-line-fn file]
    (with-open [rdr (io/reader file)]
      (reduce process-line-fn [] (debommed-line-seq rdr))))

(defn process-line-data
  "process the lines of the file that specifies the input data"
  [accumulator input-line]
  (let [split-line (str/split input-line #",")
        processed-line {:object (nth split-line 0)
                        :step (read-string (nth split-line 1))
                        :vertex (nth split-line 2)}]
    (conj accumulator processed-line)))

(defn process-line-vertices
  "process the lines of a file that specifies the vertices file"
  [accumulator input-line]
  (let [split-line (str/split input-line #",")
        processed-line {(nth split-line 0)
                        {:x (read-string (nth split-line 1))
                         :y (read-string (nth split-line 2))}}]
    (merge (into {} accumulator) processed-line)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Analysis functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn object-trajectory
  "compute trajectory of a given object"
  [data object]
  (let [data-filtered-for-object
        (->> data
             (filter #(= object (% :object)))
             (sort-by :step))
        steps (map #(%1 :step) data-filtered-for-object)
        states (map #(%1 :vertex) data-filtered-for-object)
        trajectory
        (->> (zipmap steps states)
             (sort-by first)
             (vals))]
    (conj
     {:object object}
     {:trajectory trajectory})))

(defn all-trajectories
  "compute all trajectories"
  [data]
  (->> data
       (map #(%1 :object))
       (distinct)
       (map #(object-trajectory data %1))))

(defn system-vertices-state
  "compute the state of all vertices"
  [filter-fn data]
  (->> data
       (all-trajectories)
       (filter filter-fn)
       (map #(%1 :trajectory))
       (apply concat)
       (frequencies)
       (map #(hash-map (first %) (hash-map :state (second %))))
       (apply merge)))

(defn edges-of-trajectory
 "A vector of all the edges of a trajectory"
  [trajectory]
  (let
      [start-vertices (take (- (count trajectory) 1) trajectory)
       end-vertices (rest trajectory)]
    (map vector start-vertices end-vertices)))

(defn all-edges
  "Get all the edges from the data"
  [data filter-fn]
  (let
      [filtered-trajectories
       (if (not (= filter-fn "-1"))
         (filter filter-fn (all-trajectories data))
         (all-trajectories data))]
    (mapcat #(edges-of-trajectory (% :trajectory)) filtered-trajectories)))

(defn system-edges-weights
  "take the frequencies of each edge and then map this against the location of all the edges"
  [edges location-info]
    (map (fn [[[a b] c]]
            {:start (first (filter #(= (first %) a) location-info))
            :end (first (filter #(= (first %) b) location-info))
            :width c})
           (frequencies edges)))
    
(defn system-to-plot
  "Data structure to plot"
  [data vertices-locations data-model-atom filter-fn]
  (let
      [vertices
       (->> data
            (system-vertices-state filter-fn)
            (merge-with merge vertices-locations)
            (filter #(not (= nil ((second %) :state))))
            (conj {}))
       edges
       (-> data
           (all-edges filter-fn)
           (system-edges-weights locations))]
    (reset! data-model-atom
          (conj
           {}
           {:vertices vertices}
           {:edges edges}))))
