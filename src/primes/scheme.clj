(ns primes.scheme
  "A more traditional implementation included for comparison.")

(defn sieve
  [[n & ns]]
  (cons n
    (lazy-seq
      (sieve (filter #(> (rem % n) 0) ns)))))

(defn all-primes
  []
  (sieve (iterate inc 2)))
