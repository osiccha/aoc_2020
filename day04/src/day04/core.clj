(ns day04.core
  "AOC 2020 Day 4"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn get-data
  "read passports"
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n\s")))]
    data))

(defn construct-passport
  "constructs map of passport entries"
  [s]
  (into {}
        (for [[k v] (map #(string/split % #"\:")
                         (string/split s #"\s|\n"))]
          [(keyword k) v])))

(defn validate-passport
  "check for all mandatory entries"
  [passport]
  (let [fields (string/split "byr iyr eyr hgt hcl ecl pid" #"\s")]
    (reduce (fn [a b] (and a b)) true (map (fn [x] (contains? passport (keyword x))) fields))))

(defn part1
  [data]
  (count (filter validate-passport (map construct-passport data))))

(defn -main
  "AOC Day 4 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         part1
         println)))
