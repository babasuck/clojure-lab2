(ns lab2.core-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]))

(deftest test-integr
  (testing "Integration of x^2"
    (let [f (fn [x] (* x x))
          integrate-f (integr f 0.01)]
      (is (< (Math/abs (- (integrate-f 0 1) (/ 1 3))) 0.0001))))

  (testing "Integration of 2x"
    (let [g (fn [x] (* 2 x))
          integrate-g (integr g 0.01)]
      (is (< (Math/abs (- (integrate-g 0 1) 1.0)) 0.0001))))

  (testing "Integration of x+1"
    (let [h (fn [x] (+ x 1))
          integrate-h (integr h 0.01)]
      (is (< (Math/abs (- (integrate-h 0 1) 1.5)) 0.0001)))))

(deftest test-memoized-integr
  (testing "Memoized integration of x^2"
    (let [f (fn [x] (* x x))
          memo-integrate-f (memoized-integr f 0.01)]
      (is (< (Math/abs (- (memo-integrate-f 0 1) (/ 1 3))) 0.0001))
      (is (< (Math/abs (- (memo-integrate-f 0 2) (/ 8 3))) 0.0001)))))

(deftest test-trapezoidal-stream
  (testing "Stream-based integration of x^2"
    (let [f (fn [x] (* x x))
          stream-f (trapezoidal-stream f 0.01)]
      (is (< (Math/abs (- (let [[_ sum] (last (take 101 stream-f))]
                            sum)
                          (/ 1 3))) 0.0001))
      (is (< (Math/abs (- (let [[_ sum] (last (take 201 stream-f))]
                            sum)
                          (/ 8 3))) 0.0001)))) 
  
  (testing "Stream-based integration of x+1"
    (let [h (fn [x] (+ x 1))
          stream-h (trapezoidal-stream h 0.01)]
      (is (< (Math/abs (- (let [[_ sum] (last (take 101 stream-h))]
                            sum)
                          1.5)) 0.0001))
      (is (< (Math/abs (- (let [[_ sum] (last (take 201 stream-h))]
                            sum)
                          4.0)) 0.0001)))))
