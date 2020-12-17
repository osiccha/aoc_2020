(ns day08.core
  "AOC 2020 Day 8"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn get-data
  "read boot code"
  [f]
  (let [data (->> f
                  slurp
                  string/trim
                  (#(string/split % #"\n"))
                  (#(map (fn [x] (string/split x #"\s")) %))            
                  (#(map (fn [x] [(keyword (first x)) (Integer/parseInt (second x))]  ) %)))]
    data))

(defn construct-game-map
  "make map corresponding to boot code"
  [lines]
  (loop [instruct lines size 0 game {}]
    (if (= 0 (count instruct))
      game
      (recur (rest instruct) (+ size 1) 
             (into game
                   [[size (first instruct) ]])))))

(defn in?
  [coll elem]
  (some #(= elem %) coll))

(defn do-line
  "reads an instruction and executes it"
  [line accum game]
  (let [instruct (get game line)
        value (second instruct)]
    (case (first instruct)
      :acc [(+ line 1) (+ accum value) game]
      :jmp [(+ line value) accum game]
      :nop [(+ line 1) accum game]
      nil [line accum game])))

(defn search-loop
  [game]
  (loop [line 0 
         accum 0
         visited []]
    (if (in? visited
             line)
      [accum line]
      (let [state (do-line line accum game)]
        (recur (first state)
               (second state)
               (conj visited line))))))

(defn switch-inst
  "switches specified line around"
  [game line]
  (let [instruction (get game line)]
    (case (first instruction)
      :jmp (assoc game line [:nop (second instruction)])
      :nop (assoc game line [:jmp (second instruction)])
      nil)))

(defn part1
  [data]
  (search-loop (construct-game-map data)))

(defn part2
  [data]
  (let [origin (construct-game-map data)
        all-games (for [x (range 0 (count origin))]
                    (switch-inst origin x))]
    (filter #(= (second %) (count origin))
            (map search-loop
                 (filter (fn [y] (not (nil? y)))
                         all-games)))))

(defn -main
  "AOC Day 8 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
