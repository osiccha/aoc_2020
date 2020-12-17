(ns day16.core
  "AOC 2020 Day 16"
  (:require
    [clojure.java.io :refer [reader]])
  (:require
    [clojure.string :as string])
  (:gen-class))

(defn parse-ticketblock
  "reads ticket block and returns list of tickets"
  [s]
  (map (fn [x] 
         (map #(Integer/parseInt %)
              (string/split x #",")))
       (next (string/split s #"\n"))))

(defn conv-range
  "convert range string into 2 pairs"
  [s]
  (let [both-range (map #(Integer/parseInt %) s)
        second-range (next (next both-range))]
    [[(first both-range) (second both-range)]  [(first second-range) (second second-range)]]))

(defn parse-rule
  "convert rulestring into ruleslist"
  [s]
  (let [lines (string/split s #"\n")
        key-value (map #(string/split % #":\s")
                       lines)
        values (map (fn [x]
                      [(first x)
                       (conv-range (string/split (second x) #"(\sor\s)|-|\s"))])
                    key-value)] 
    values))

(defn in-range
  "check for range condition"
  [value ranges]
    (reduce #(or %1 (<= (first %2) value (second %2))) false ranges))

(defn get-data
  "read tickets"
  [f]
  (let [data (->> f
                  slurp
                  string/trim
                  (#(string/split % #"\n\s")))
        rules (first data)
        my-ticket (second data)
        other-tickets (second (next data))]
    [(parse-rule rules)
     (first (parse-ticketblock my-ticket))
     (parse-ticketblock other-tickets)]))

(defn filter-valid-tickets
  "filter all tickets that have an invalid field"
  [data]
  (let [ranges (map #(second %) (first data))
        other-tickets (second (next data))]
    (filter (fn [ticket] 
              (not (some nil?
                         (map (fn [entry]
                                (some #(in-range entry %)
                                      ranges) )
                              ticket))))
            other-tickets)))

(defn conv-ticket-entries-into-possible-field-name
  "goes over all valid tickets, and writes at each position all rules, for which the entry satisfies the range"
  [data]
  (let [other-tickets (filter-valid-tickets data)
        rules (first data)]
    (map (fn [ticket] 
           (map (fn [entry] 
                  (map first
                       (filter #(in-range entry
                                          (second %))
                               rules)))
                ticket))
         other-tickets)))

(defn intersect-string-list
  "returns list of strings that occur in both collections"
  [x y]
  (filter (fn [elem] 
            (some #(= % elem) 
                  y))
          x))

(defn intersect-field-names
  "for two list of entries with possible fields, returns a list with entries, which are valid for both"
  [left-ticket right-ticket]
  (if (= (count left-ticket) 1) 
    [(intersect-string-list (first left-ticket) (first right-ticket))]
    (concat [(intersect-string-list (first left-ticket) (first right-ticket))]
            (intersect-field-names (rest left-ticket) (rest right-ticket)))))

(defn set-identified-fields
  "checks for fields whose labels are uniquely identified and removes these labels from all other fields"
  [all-fields]
  (let [unique-fields (map first (filter #(= (count %) 1) all-fields))]
    (map ( fn [field]
           (if (= (count field) 1)
             field
             (filter (fn [elem] 
                       (nil? (some (fn [in] (= elem in))
                                   unique-fields)))
                     field))) 
         all-fields)))

(defn part1
  [data]
  (let [ranges (map #(second %) (first data))
        other-tickets (second (next data))
        values (flatten other-tickets)]
    (reduce + (filter (fn [x] (not (some #(in-range x %) ranges))) values))))

(defn part2
  [data]
  (let [tickets (filter-valid-tickets data)
        field-names (reduce intersect-field-names (conv-ticket-entries-into-possible-field-name data))
        ticket-translation (loop [current field-names]
                             (if (empty? (filter #(not (= (count %) 1)) current  ))
                               current
                               (recur (set-identified-fields current))))]
      (loop [my-ticket (second data) translation ticket-translation res 1]
        (if (empty? my-ticket)
          res
            (recur (rest my-ticket) 
                   (rest translation) 
                   (if (empty? (re-find #"departure" (first (first translation))))
                     res
                     (* res (first my-ticket))))))))

(defn -main
  "AOC Day 16 entrypoint"
  [& args]
  (if (empty? args)
    (println "Expected a path to the input file")
    (->> args
         first
         get-data
         ;;part1
         part2
         println)))
