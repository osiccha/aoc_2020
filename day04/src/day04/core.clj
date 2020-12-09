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

(defn append-height
  "adds entry with unit as key"
  [passport]
  (let [entry (string/split (get passport :hgt) #"(?=i)|(?=c)")]
    (assoc passport (keyword (second entry)) (first entry))))

(defn validate-passport
  "check for all mandatory entries"
  [passport]
  (let [fields (string/split "byr iyr eyr hgt hcl ecl pid" #"\s")]
    (not (some false? (map (fn [x] (contains? passport (keyword x))) fields)))))

(defn validate-pattern
  "for a map of rules applies regex of rule to field of passport"
  [passport field rules ]
  (if (contains? passport field)
    (not (nil?    (re-matches (re-pattern (get rules field)) (get passport field))))
    false))

(defn validate-passport-entries
  "check if all passport entries satisfy regex for validation"
  [passport]
  (let [rules {:byr "\\d{4}",
               :iyr "\\d{4}",
               :eyr "\\d{4}",
               :hgt "\\d+(in|cm)",
               :hcl "#[0-9a-f]{6}",
               :ecl "amb|blu|brn|gry|grn|hzl|oth",
               :pid "\\d{9}"
               }]
    (not (some false? (map (fn [field] (validate-pattern passport field rules)) (keys rules))))))

(defn compare-range
  "check if entry of passport to field is in the interval or rule"
  [passport field rule]
  (let [x (get passport field)]
    (if (nil? x)
      true
      (<=  (first rule) (Integer/parseInt x)  (second rule)))))

(defn check-range
  "checks that all passport entries with numbers are in valid range"
  [passport]
  (let [rules {:byr [1920 2002],
               :iyr [2010 2020],
               :eyr [2020 2030],
               :in [59 76],
               :cm [150 193]}
        passport (append-height passport)]
    (not (some false?
               (map (fn [x] (compare-range passport x (get rules x)))
                    (keys rules))))))

(defn part1
  [data]
  (count (filter validate-passport (map construct-passport data))))

(defn part2
  [data]
  (count (filter check-range
                 (filter validate-passport-entries
                         (filter validate-passport
                                 (map construct-passport
                                      data))))))

(defn -main
  "AOC Day 4 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
