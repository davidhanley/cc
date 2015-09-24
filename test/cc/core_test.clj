(ns cc.core-test
  (:require [clojure.test :refer :all]
            [cc.core :refer :all]))

;(deftest basic-piece-test
 ; (let [p (make-piece "p" 1 1)]
  ;     (testing "make sure make-piece works."  	 
;		(is (= (:glyph p) "p"))
;		(is (= (:side p) 1))
;		(is (= (:value p) 1)))))

(deftest axis-coord
  (testing "axis-ok" 
	   (is (= (axis-ok? -1) false))
	   (is (= (axis-ok? 0) true))
	   (is (= (axis-ok? 7) true))
	   (is (= (axis-ok? 8) false)))
  (testing "coord-ok" 
	   (is (= (coord-ok? [-1 0]) false))
	   (is (= (coord-ok? [8 7]) false))
	   (is (= (coord-ok? [1 1]) true)))
  (testing "square-ok"
	   (is (= (map square-ok? [-1 0 8 63 64]) [false true true true false])))
  (testing "squares, square-string"
	   (is (= e1 60)
	       (= a8 0)))
  (testing "add-coords"
	   (is (= (add-coords [2 2][2 2]) [4 4]))
	   (is (= (add-coords [4 4][1 -1]) [5 3])))
	       
  )
	       
	   
	   

