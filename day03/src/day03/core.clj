(ns day03.core
  "AOC 2020 Day 3"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn get-data
  "read tree pattern"
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    data))

(defn check-tree
  "check for tree in column x for a pattern"
  [pattern n]
  (let [width (count pattern)]
    (if (= \# (nth pattern (rem n width)))
      1
      0)))

(defn walk-slope
  "walk through forest along slope right 3, down 1"
  [pos forest direction]
  (if (empty? forest)
    0
    (+ (check-tree (first forest) pos) (walk-slope (+ pos direction) (rest forest) direction))))

(defn part1
  [s]
  (walk-slope 0 s 3))

(defn walk-slope-set
  [forest]
  (let [slopes [[1,1] [3,1] [5,1] [7,1] [1,2]]]
    (map (fn [slope] (walk-slope 0 (take-nth (second slope) forest) (first slope))) slopes)))

(defn part2
  [f]
  (reduce * 1 (walk-slope-set f)))

(defn -main
  "AOC Day 3 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
