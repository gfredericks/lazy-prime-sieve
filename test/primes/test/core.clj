(ns primes.test.core
  (:use clojure.test)
  (:use primes.core))

(deftest bump-divisors-map-test
  (let [mm {9 [3]}]
    (is (= (bump-divisors-map mm 9)
           {15 [3]}))))

(deftest primes-test
  (is (= (take 10 (all-primes))
         [2 3 5 7 11 13 17 19 23 29]))
  (is (= 7919 (nth (all-primes) 999))))
