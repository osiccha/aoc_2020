(ns day13.core
  "AOC 2020 Day 13"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn get-data
  "read bus data"
  [f]
  (let [data (-> f
                 slurp
                 string/trim
                 (#(string/split % #"\n|,")))]
    data))

(defn parse-input
  "parse input into list of integers and nil if the entry is x"
  [table]
  (map (fn [x] (if (= x "x")
                 nil
                 (Integer/parseInt x))) (rest table)))

(defn xgcd
  "Extended Euclidean Algorithm. Returns [gcd(a,b) x y] where ax + by = gcd(a,b)."
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g x y] (xgcd (mod b a) a)]
      [g (-' y (*' (quot b a) x)) x])))

(defn construct-orthogonal-integers
  "constructs integer x with x=1 (mod factor) and x=0 (mod product/factor)"
  [factor product]
  (-' 1 
      (*' factor 
          (second (xgcd factor 
                        (quot product factor))))))

(defn construct-equation-coefficients-and-basis
  "construct list of congruencies x = a (mod bus-ID) as list of tupels [a b] with b an integer b=1 (mod bus-ID) and  b=0 modulo all other factors of primeproduct"
  [table primeproduct]
  (second (reduce (fn [x bus-ID]
                    (let [step (first x)
                          res (second x)]
                      (if (nil? bus-ID)
                        [(+ 1 step)
                         res]
                        [(+ 1 step)
                         (conj res [  (mod (* -1 step) bus-ID)
                                    (construct-orthogonal-integers bus-ID primeproduct) ])])))
                  [0 []]
                  table)))

(defn apply-natural-epi
  "calculate CRT-epi for inverse imagine of canonical basis"
  [data]
  (let [primes (filter #(not (nil? %)) data)
        product (reduce *' 1 primes)
        entries (construct-equation-coefficients-and-basis data product)]
    (map (fn [x]  (map #( mod (second x)  % ) primes)   ) entries)))

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
    (* (Integer/parseInt (first (filter (fn [y]
                                          (= 0
                                             (mod (+ timestamp
                                                     wait)
                                                  (Integer/parseInt y))))
                                        schedule)))
       wait )))

(defn part2
  [data]
  (let [primes (filter #(not (nil? %)) data)
        product (reduce *' 1 primes)
        result (mod (reduce (fn [x y] (+' x (apply *' y)))
                            0
                            (construct-equation-coefficients-and-basis data product))
                    product)]
    result))

(defn -main
  "AOC Day 13 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
     (->> args
         first
         get-data
         ;;part1
         parse-input
         part2
         println)))


