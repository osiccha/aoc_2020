(defproject day01 "0.1.0-SNAPSHOT"
  :description "AOC2020: Day 1"
  :url "https://adventofcode.com/2020/day/1"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot day01.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
