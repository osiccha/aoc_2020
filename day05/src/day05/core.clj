(ns day05.core
  "AOC 2020 Day 5"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn get-data
  "read passports"
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    data))

(defn read-digit
  "convert letter to corresponding number"
  [digit]
  (if (or (= digit \B) (= digit \R))
    1
    0))

(defn conv-pass
  "convert pass of length n to number"
  [pos s]
  (let [value (read-digit (first s))]
    (if (= pos 0)
      value
      (+ (* (exp 2 pos) value) (conv-pass (- pos 1) (rest s))))))

(defn pass-id
  "calculcate ID of boarding pass"
  [s]
  (let [row (conv-pass 6 s)
        col (conv-pass 2 (drop 7 s))]
    (+ (* 8 row) col)))

(defn get-board-id
  "gets your boarding ID"
  [passes]
  (let [x (first passes)
        xs (second passes)]
    (if (nil? xs)
      x
      (if (= x (- xs 2))
        (+ x 1)
        (get-board-id (rest passes))))))

(defn part1
  [data]
  (apply max (map pass-id data)))

(defn part2
  [data]
  (get-board-id (sort <= (map pass-id data))))


(defn -main
  "AOC Day 5 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
