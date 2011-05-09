(ns primes.core
  (:gen-class))

(defn bump-divisors-map
  [divisors z]
  (reduce
    (fn [m p]
      (let [y (+ z (* 2 p))]
        (update-in m [y] conj p)))
    (dissoc divisors z)
    (divisors z)))

(defn bump-divisors
  [ob z]
  (update-in ob [:divisors] bump-divisors-map z))

;; Initial sieve object. Starts at 3 instead of 2, since 2 is excluded
;; algorithmically. The :divisors map is a map from an upcoming composite
;; number to all of its prime divisors. The :square value is used to detect
;; when a new prime needs to be added to the :divisors map. The :primes
;; key points to a nested structure for keeping track of the next prime to
;; add to the :divisors map. The :p key keeps track of the current number
;; being tested. This number is always prime in the object returned from
;; (next-prime).
(def initial
  {:p         3,
   :divisors  {9 '(3)},
   :square 9,
   :primes nil})

;; We need to declare next-prime ahead of time because of the circular
;; dependency between next-prime and bump-square
(declare next-prime)

(defn bump-square
  "Finds the next prime in the nested structure and updates the :square key
  with it."
  [{:keys [divisors primes] :as ob}]
  (let [{:keys [p] :as new-primes} (next-prime (or primes initial)),
        sq (* p p)]
    (assoc ob :primes new-primes,
              :square sq,
              :divisors (assoc divisors sq (list p)))))

(defn next-prime
  [{:keys [divisors p square] :as ob}]
  (let [z (+ 2 p)]
    (if (divisors z)
      ;; then composite
      (->
        (if (= z square)
          (bump-square ob)
          ob)
        (bump-divisors z)
        (assoc :p z)
        (recur))
      ;; otherwise prime
      (assoc ob :p z))))

(defn all-primes
  []
  (cons 2
    (map :p
      (iterate next-prime initial))))

(defn -main
  []
  (doseq [p (all-primes)]
    (println p)))
