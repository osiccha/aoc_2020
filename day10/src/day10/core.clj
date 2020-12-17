(ns day10.core
  "AOC 2020 Day 10"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn get-data
  "read custom forms answers"
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    (map #(Integer/parseInt %) data)))

(defn solve-subproblem
  [adapter known-solutions]
  (reduce #(+ %1
              (if (nil? (get known-solutions %2))
                0
                (get known-solutions %2)))
          0
          (range (+ 1 adapter) (+ adapter 4))))

(defn part1
  [data]
  (loop [prev 0
         adapters (sort <= data)
         values [0 0]]
    (if (= 0 (count adapters))
      (* (first values) (+ 1 (second values)))
      (recur (first adapters)
             (rest adapters) 
             (case (- (first adapters) prev)
               1 [(+ 1 (first values)) (second values)]
               2 values
               3 [ (first values) (+ 1 (second values))])))))

(defn part2
  [data]
  (let [entries (sort >= (conj data 0))
        highest-jolt (apply max entries)]
    (loop [adapters entries
           solutions {(+ 3 highest-jolt) 1}]
      (if (= 0 (count  adapters))
        (get solutions 0)
        (recur (rest adapters)
               (assoc solutions 
                      (first adapters) 
                      (solve-subproblem (first adapters) solutions)))))))

(defn -main
  "AOC Day 10 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
