(ns day13.core
  "AOC 2020 Day 13"
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
                 (#(string/split % #"\n|,")))]
    data)

(defn part1 
  [table]
  (let [timestamp (Integer/parseInt (first table))
        schedule (filter #(not (= "x" %)) (rest table))
        after-timestamp (map (fn [x] 
                               (+ (- timestamp 
                                     (rem timestamp (Integer/parseInt x)))
                                  (Integer/parseInt x)))
                             schedule)
        wait (- (apply min after-timestamp) timestamp)]
    (* (Integer/parseInt (first (filter (fn [y] (= 0 (mod (+ timestamp
                                                             wait)
                                                          (Integer/parseInt y))))
                                        schedule))) wait )))

(defn -main
  "AOC Day 13 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         part1
         println
         )))
