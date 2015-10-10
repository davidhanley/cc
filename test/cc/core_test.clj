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
	 
(deftest pieces-test
  (testing "from-string"
	   (is (= white-pawn (piece-from-string "P")))
	   ))


(deftest movgen-stuff
  (testing "gen-hopper"
	   (is (= (gen-hopper [[-1 -1]] [1 1]) (list {:from 9, :to 0}))))
  (testing "gen-ray"
	   (is (= (gen-ray [-1 -1] [3 4]) (list {:from 28, :to 19} {:from 28, :to 10} {:from 28, :to 1}))))
  
  (testing "white-knight-table" 
	   (is (= (make-move-table white-knight e4)
		  (list {:from 36, :to 21} {:from 36, :to 19} {:from 36, :to 26} {:from 36, :to 30} {:from 36, :to 42} {:from 36, :to 46} {:from 36, :to 51} {:from 36, :to 53}))))

  (testing "white-bishop-table" 
	  (is (= (make-move-table white-bishop g2)
		 (list (list {:from 54, :to 45} {:from 54, :to 36} {:from 54, :to 27} {:from 54, :to 18} {:from 54, :to 9} {:from 54, :to 0}) (list {:from 54, :to 47}) (list {:from 54, :to 61}) (list {:from 54, :to 63})))))

  )
  
	   
	   
(deftest hashing 
  (testing "nonpawn hashes right"
	   (is (= (hash-piece {:hash-code 0 :pawn-hash 0} {:value 200 :hashes (long-array [55])}  0) {:hash-code 55 :pawn-hash 0}))
	   (is (= (hash-piece {:hash-code 0 :pawn-hash 0} {:value 100 :hashes (long-array [55])}  0) {:hash-code 55 :pawn-hash 55}))
	   )
  )

(deftest playing 
  (let [board (from-fen start-fen)]
       (testing "playing"
		(let [board2 (play-move board {:from e2 :to e4})]
		     (is (= (board2 e4) white-pawn))
		     (is (= (board2 e2) nil))
		     (is (= (board2 :material) 0))
		     (is (= (board2 :white-pieces-at) #{62 59 58 60 55 54 48 50 56 36 61 51 57 53 63 49}))
		     )
		)
       )
  )