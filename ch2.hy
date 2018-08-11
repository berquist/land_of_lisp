(setv *small* 1)
(setv *big* 100)

(defn ash [n shift]
  (cond [(< shift 0) (>> n (abs shift))]
        [(> shift 0) (<< n shift)]
        [True n]))

(assert (= (ash 11 1) 22))
(assert (= (ash 11 0) 11))
(assert (= (ash 11 -1) 5))

(defn guess-my-number []
  (ash (+ *small* *big*) -1))

(defn smaller []
  (global *big*)
  (setv *big* (- (guess-my-number) 1))
  (guess-my-number))

(defn bigger []
  (global *small*)
  (setv *small* (+ (guess-my-number) 1))
  (guess-my-number))

(defn start-over []
  (global *big*)
  (global *small*)
  (setv *small* 1)
  (setv *big* 100)
  (guess-my-number))
