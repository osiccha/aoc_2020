(ns day18.core
  "AOC 2020 Day 18"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn get-data
  "read arithmetic expressions"
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n")))]
    (map (fn [x]
           (map #(case %
                   "+" "+"
                   "*" "*"
                   "(" "("
                   ")" ")"
                   (Integer/parseInt %))
                (string/split x #"\s|(?<=\()|(?=\))")))
         data)))

(defn set-brackets
  "converts bracket tokens into corresponding list"
  [buffer & xs]
  (if (empty? xs)
    buffer
    (let [current (first xs)
          others (next xs)]
      (case current
        "("  (let [res (apply set-brackets 
                              (conj others
                                    []))]
               (apply set-brackets (conj (second res) (conj buffer (first res)))))
        ")" [buffer others]
        (apply set-brackets (seq (conj others (conj buffer current))))))))

(defn eval-expr
  "evaluates a list from left to right"
  [expr]
  (if (int? expr)
    expr
    (let [left (eval-expr (first expr))
          oper (second expr)
          right (next (next expr))]
      (case oper
        "+" (eval-expr (conj (next right) (+ left (eval-expr (first right)))))
        "*" (eval-expr (conj (next right) (* left (eval-expr (first right)))))
        left))))

(defn part1
  [data]
  (reduce + (map #(eval-expr (apply set-brackets (conj  % []))) data)))

(defn -main
  "AOC Day 18 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         part1
         println)))
