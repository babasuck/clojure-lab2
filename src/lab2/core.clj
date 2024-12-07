(ns lab2.core)

 (defn integr [func h]
   "Trapezoidal method for numerical integration."
   (fn [a b]
     (let [n (int (/ (- b a) h))
           x-values (map #(+ a (* % h)) (range (inc n)))]
       (* h
          (/ (+ (func a) (func b)
                (* 2 (reduce + (map func (rest (butlast x-values))))))
             2)))))

(defn memoized-integr [func h]
  "Returns a memoized integral function."
  (memoize (integr func h)))

(defn trapezoidal-stream [func h]
  (let [partial-integr (integr func h)]

    (defn next-point [start current-integral]
      (let [next-point (+ start h)
            next-integral (+ current-integral (partial-integr start next-point))]
        [next-point next-integral]))

    (defn generate-stream [start current-integral]
      (lazy-seq
       (let [[next current] (next-point start current-integral)]
         (cons [next current]
               (generate-stream next current)))))

    (cons [0 0] (generate-stream 0 0))))

(defn -main
  [& args]
  (let [f (fn [x] (* x x))

        stream-f (trapezoidal-stream f 0.1)

        memo-integrate-f (memoized-integr f 0.1)

        direct-integrate-f (integr f 0.1)]
    
    (println "\n--- Stream-based Integration ---")
    (println "Stream-based integration (f, [0, 100]):")
    (time (println "Result:" (let [[_ result] (last (take 1001 stream-f))]
                               result)))

    (println "\nStream-based integration (f, [50, 150]):")
    (time (println "Result:" (let [[_ result] (last (take 1501 stream-f))]
                               result)))

    (println "\n--- Memoized Integration ---")
    (println "Memoized integration (f, [0, 100]):")
    (time (println "Result:" (memo-integrate-f 0 100)))

    (println "\nMemoized integration (f, [50, 150]):")
    (time (println "Result:" (memo-integrate-f 50 150)))

    (println "\n--- Direct Integration ---")
    (println "Direct integration (f, [0, 100]):")
    (time (println "Result:" (direct-integrate-f 0 100)))

    (println "\nDirect integration (f, [50, 150]):")
    (time (println "Result:" (direct-integrate-f 50 150)))))

