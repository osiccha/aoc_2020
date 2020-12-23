(ns day23.core
  "AOC 2020 Day 23"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn find-destination-and-removed-cups
  "finds the destination cup to current cup and the three cups clockwise to current cup"
  [permutation current order]
  (let [three-cups  (loop [x current
                           removed-cups []]
                      (if (< (count removed-cups) 3 )
                        (let [to-remove (nth permutation x)]
                          (recur to-remove
                                 (conj removed-cups to-remove) ))
                        removed-cups))]
    (loop [x current]
      (let [dest (if (= 1 x)
                   order
                   (- x 1 ))]
        (if (some #(= % dest) three-cups)
          (recur dest)
          [dest three-cups])))))

(defn apply-operation
  "updates permutation according to crab rule"
  [permutation current dest-and-removed-cups]
  (let [dest (first dest-and-removed-cups)
        first-removed-cup (first (second dest-and-removed-cups))
        last-removed-cup (nth (second dest-and-removed-cups) 2)
        after-dest (nth permutation dest)
        after-removed-cups (nth permutation last-removed-cup)]
    [(assoc permutation
            current after-removed-cups
            dest first-removed-cup
            last-removed-cup after-dest)
     after-removed-cups]))

(defn loop-operation
  "applies crab operation on permutation with start loop-count times. Order of corresponding sym. group has to be given"
  [permutation start loop-count order]
  (loop [perm permutation
         current start
         step 0]
    (if (= step loop-count)
      perm
      (let [next-results (apply-operation perm
                                          current
                                          (find-destination-and-removed-cups perm
                                                                             current
                                                                             order))]
        (recur (first next-results)
               (second next-results)
               (+ step 1))))))

(defn convert-cycle
  "conv function into cycle"
  [perm]
  (loop [cyc [1] current 1]
    (if (= (nth perm current) 1)
      cyc
      (recur (conj cyc
                   (nth perm current))
             (nth perm current)))))

(defn part1
  []
  (println (loop-operation [nil 8 3 1 6 7 5 2 9 4] 3 100 9)))

(defn part2
  []
  (let [id-perm (range 11 1000002)
        cup-perm (assoc (vec (concat [nil 8 10 1 6 7 5 2 9 4]
                                     id-perm))
                        1000000
                        3)
        res (loop-operation cup-perm 3 10000000 1000000)
        first-fac (nth res 1)]
    (println (* first-fac (nth res first-fac)))))

(defn -main
  "AOC Day 23 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         println)))
