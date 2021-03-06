(ns day06.core
  "AOC 2020 Day 6"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn seperate-answers
  "seperate answers of one group"
  [s]
  (string/split s #"\n"))

(defn get-data
  "read custom forms answers"
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n\s")))]
    (map seperate-answers data)))

(defn count-answers
  "count answers from one group"
  [answers]
  (count (distinct (apply concat answers))))

(defn remove-answers
  "remove answers from first form that the second form does not have"
  [form other]
  (reduce (fn [x y] (if (some #(= y %) other)
                      (str x y)
                      x))
          ""
          form))

(defn part1
  [data]
  (reduce (fn [x y] (+ x (count-answers y))) 0 data))

(defn part2
  [data]
  (reduce (fn [x y] (+ x (count (reduce remove-answers (map distinct y))))) 0 data))

(defn -main
  "AOC Day 6 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
