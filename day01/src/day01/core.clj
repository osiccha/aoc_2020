(ns day01.core
  "AOC 2020 Day 1"
  (:require
    [clojure.java.io :refer [reader]])
  (:gen-class))

(defn input-int-list 
  "read file s to seq of integers"
  [s]
  (map #(Integer/parseInt %)
       (with-open [r (reader s)]
         (doall (line-seq r)))))


(defn search-first-of-goal-sum
  "search entries for first entry such that another entry exist which sums to goal"
  [entries goal]
  (let [x (first entries)
        entries (rest entries)]
    (if (or (nil? x)
            (some #(= (- goal x) %)  entries))
      x
      (search-first-of-goal-sum entries goal))))

(defn part1 
  [intlist]
  (let [x (search-first-of-goal-sum intlist 2020)]
    (* x (- 2020 x))))

(defn part2
  [intlist]
  (reduce *
          (map first 
               (filter (fn [y] 
                         (not (nil? (second y))))
                       (map (fn [x] 
                              [ x (search-first-of-goal-sum intlist (- 2020 x)) ] )
                            intlist)))))

(defn -main
  "AOC Day 1 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         input-int-list
         ;;part1
         part2
         println)))

