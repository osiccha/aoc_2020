(ns day02.core
  "AOC 2020 Day 2"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn input-password 
  "read file s to seq of strings"
  [s]
  (with-open [r (reader s)]
    (doall (line-seq r))))

(defn get-range
  "gets first and second value out of string seperated by -"
  [s]
  (map #(Integer/parseInt %) (string/split s #"-")))

(defn password-policy
  "checks if given string fulfills password policy"
  [s]
  (let [components (string/split s #"\s")]
    (let [rang (get-range (first components))
          character (first (second components))]
      (let [counter (count (filter #(= character %) (second (next components))))]
        (if (and (<= (first rang) counter ) (<= counter (second rang)))
          counter
          nil)))))

(defn part1
  [entries]
  (count (filter password-policy entries)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         input-password
         part1
         println)))

