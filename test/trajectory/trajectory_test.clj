(ns trajectory.trajectory-test
  (:require [clojure.test :refer :all]
            [trajectory.data-model :refer :all]))


(def test-data (read-file input-file process-line []))

;; ********************************** FILE INPUT TESTS

(deftest file-input
  "Tests of file input functions"
  (testing "debomify to remove BOM at start of file"
      (is 
       (=
        ((first test-data) :object)
        "B")))
  
  (testing "read-file 5th line of output should correspond to the 5th line of the test-data"
    (is
     (=
      (nth test-data 5)
      {:object "B", :step 2, :vertex "Line 3"})))

  (testing "read-input-file-return-data wrapper should equal test-data"
    (is
     (=
      (read-input-file-return-data)
      test-data))))


;; ********************************** TRAJECTORIES TESTS
  
(deftest trajectories
  "Tests of functions computing trajectories"
  (testing "object-trajectory creates trajectory of test-data"
    (is
     (=
      (object-trajectory "B" test-data)
      {:object "B", :trajectory ["Line 1", "Line 3", "Line 1", "Line 3", "Line 1"]})))

  (testing "all-trajectories creates all the trajectories of the test data"
    (is
     (=
      (all-trajectories test-data)
      (list {:object "B", 1 "Line 1", 2 "Line 3", 3 "Line 1", 4 "Line 3", 5 "Line 1", :trajectory-string "Line 1Line 3Line 1Line 3Line 1"}
            {:object "A", 1 "Line 0", 2 "Line 2", 3 "Line 1", :trajectory-string "Line 0Line 2Line 1"}
            {:object "C", 1 "Line 2", 2 "Line 2", 3 "Line 1", 4 "Line 2", :trajectory-string "Line 2Line 2Line 1Line 2"}))))

  (testing "mean-position-in-sequence returns expected means of test-data for Line 2"
    (is
     (=
      (mean-position-in-sequence "Line 2" test-data)
      9/4))))

;; ********************************** VERTICES TESTS

(deftest vertices
  "Tests of functions for computations relating to vertices"
  (testing "distinct-vertices should return the distinct vertex names from test-data"
    (is
     (=
      (list "Line 0" "Line 1" "Line 2" "Line 3")
      (sort (distinct-vertices test-data)))))
  
  (testing "compute-vertex-state counts the appearance of a vertex in the test-data should return 5 for 'Line 2'" 
    (is
     (=
      (compute-vertex-state "Line 2" test-data)
      4)))

  (testing "system-vertices-state computes state of all vertices in the system"
    (is
     (=
      (system-vertices-state test-data)
      {"Line 1" {:state 5}, "Line 0" {:state 1}, "Line 3" {:state 2}, "Line 2" {:state 4}}))))

;; ********************************** PLOTTING MODEL TESTS

(deftest plotting-model
  "Test of the data model to be plotted"
  (testing "system-to-plot. Should update data-to-plot from empty to a value when called, the second item of, is specified in the test and the atom should be of the length of distinct objects after the update"
    (do
      (system-to-plot test-data data-to-plot)
      (is
       (=
        ["Line 1" {:x 150, :y 500, :state 5}]
        (second (@data-to-plot :vertices))
        )))))

;; ********************************** MISC FUNCTION TESTS

(deftest misc
  "Tests of misc functions"
  (testing "distinct-objects should return the distinct objects from test-data"
    (is
     (=
      (list "B" "A" "C")
      (distinct-objects test-data))))
  
  (testing "filter-object should all rows for object A in test-data"
    (is
     (=
      (filter-object "A" test-data)
      (list {:object "A", :step 1, :vertex "Line 0"}
       {:object "A", :step 2, :vertex "Line 2"}
       {:object "A", :step 3, :vertex "Line 1"})
      )))

  (testing "sort-by-step sorts data by steps"
    (is
     (=
      (sort-by-step test-data)
      (list {:object "A", :step 1, :vertex "Line 0"}
            {:object "B", :step 1, :vertex "Line 1"}
            {:object "C", :step 1, :vertex "Line 2"}
            {:object "A", :step 2, :vertex "Line 2"}
            {:object "B", :step 2, :vertex "Line 3"}
            {:object "C", :step 2, :vertex "Line 2"}
            {:object "A", :step 3, :vertex "Line 1"}
            {:object "B", :step 3, :vertex "Line 1"}
            {:object "C", :step 3, :vertex "Line 1"}
            {:object "B", :step 4, :vertex "Line 3"}
            {:object "C", :step 4, :vertex "Line 2"}
            {:object "B", :step 5, :vertex "Line 1"})))))





















