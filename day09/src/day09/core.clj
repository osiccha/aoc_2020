(ns day09.core
  "AOC 2020 Day 9"
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
    (map #(Long/parseLong %) data)))

(defn check-xmas
  [current previous]
  (if (< (count previous) 25)
     true
     (loop [to-check previous]
       (if (nil? to-check)
         false
         (if (some #(= current (+' (first to-check) %)) (next to-check) )
           true
           (recur (next to-check)))))))

(defn contigous-set
  [numbers]
  (reduce #(conj %1
                 (+' %2 (first %1)))
          (seq [(+ (first numbers) (second numbers))])
          (next (next numbers))))

(defn part1
  [data]
  (loop [checked '()
         to-check data]
    (if (empty? to-check)
      nil
      (if (check-xmas (first to-check) (take 25 checked))
        (recur (conj checked
                     (first to-check))
               (next to-check))
        (first to-check)))))

(defn part2
  [data]
  (let [error (part1 data)]
    (loop [numbers data]
      (if (empty? numbers)
        0
        (if (some #(= % error)
                  (contigous-set numbers))
          (let [found-range (take (loop [x (contigous-set numbers)]
                                    (if (= (first x) error)
                                      (+ (count x) 2)
                                      (recur  (next x)))) numbers)]
            (+ (apply min found-range) (apply max found-range)))
          (recur (next numbers)))))))

(defn -main
  "AOC Day 9 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
