(ns day19.core
  "AOC 2020 Day 19"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn get-data
  "read rules and messages"
  [f]
  (let [data (->> f
                 slurp
                 string/trim
                 (#(string/split % #"\n\s"))
                 (map (fn [x] (string/split x #"\n"))))]
    data))

(defn translate-rule
  "convert rulestring into appropiate map entry"
  [s]
  (let [key-value (string/split s #":\s")
        values (second key-value)]
    [(Integer/parseInt (first key-value))
     (map (fn [x]
            (map #(if (= (first %) \") 
                    (second %)
                    (Integer/parseInt %))
                 (string/split x #"\s")))
          (string/split values #"\s\|\s"))]))

(defn create-rules-map
  "read rules and constructs a map with those rules"
  [data]
  (into {} (map translate-rule data)))

(defn match-expr-rules 
  "checks if the string expr can be matched according to rules map"
  [expr to-match rules]
  (loop [ex expr match to-match]
    (if (empty? match)
      (empty? ex)
      (if (< (count ex) (count match))
        false
        (if (= (first ex) (first match))
          (recur (rest ex) (rest match) )
          (if (char? (first match))
            false
            (let [rule-subs (map #(concat % (rest match))
                                 (get rules (first match)))]
              (some true? (map #(match-expr-rules ex % rules) rule-subs)))))))))

(defn part1
  [data]
  (let [rules (create-rules-map (first data))
        messages (second data)]
    (count (filter true? (map #(match-expr-rules  (seq %) [0] rules) messages)))))

(defn part2
  [data]
  (let [rules (assoc (create-rules-map (first data))
                     8 [[42] [42 8]]
                     11 [[42 31] [42 11 31]])
        messages (second data)]
    (count (filter true? (map #(match-expr-rules  (seq %) [0] rules) messages)))))

(defn -main
  "AOC Day 19 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
