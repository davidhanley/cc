(ns cc.core
  (:gen-class))

(defn l[] (use 'cc.core :reload))

(defn axis-ok?[axis](and (>= axis 0) (< axis 8)))
(defn coord-ok?[[r f]](and (axis-ok? r) (axis-ok? f)))
(defn square-ok?[square](and (>= square 0) (<= square 63)))

(defn square-to-coord[square][(quot square 8)  (mod square 8)])
(defn coord-to-string[[r f]](str (get "abcdefgh" f) (get "87654321" r)))
(defn square-to-string[sq](assert (square-ok? sq)) (coord-to-string (square-to-coord sq)))
(defn coord-to-square[[r f]](+ f (* r 8)))

(def squares (range 0 64))
(doseq [sq squares](eval (list 'def (symbol (square-to-string sq)) sq)))

(defn add-coords[[r1 f1] [r2 f2]] [(+ r1 r2) (+ f1 f2) ])

(def rand-source (new java.util.Random))
(defn hashes[] (vec (take 64 (repeatedly #(.nextLong rand-source)))))

(defmacro piece[n g s v](list 'def n {:Piece (list 'quote (symbol (clojure.string/upper-case g))) :glyph g :side s :value v :hashes (long-array (hashes))
	  }))

(defmacro defpieces[& pcs]
  (let [defs (partition 4 pcs) pnames (map first defs)]
       (concat ['do] (map (fn[pd](concat ['piece] pd)) defs)
	    (list (list 'def 'pieces (vec pnames))))))					   
			    
(defpieces white-pawn "P" 1 100
           white-knight "N" 1 325
           white-bishop "B" 1 325
           white-rook "R" 1 500
           white-queen "Q" 1 500
           white-king "K" 1 10000

	   black-pawn "p" -1 100
           black-knight "n" -1 325
           black-bishop "b" -1 325
           black-rook "r" -1 500
           black-queen "q" -1 500
           black-king "k" -1 10000)

(def start-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defn piece-from-string[str](first (filter #(= str (:glyph %1)) pieces)))
  
(defn ray[start delta](take-while coord-ok? (iterate (partial add-coords delta) start)))

(def king-deltas   [[-1 -1][0 -1][1 -1][-1 0][1 0][-1 1][0 1][1 1]])
(def knight-deltas [[-2 1][-2 -1][-1 -2][-1 2][1 -2][1 2][2 -1][2 1]])

(def bishop-slides [[-1 -1][-1 1][1 -1][1 1]])
(def rook-slides   [[1 0][-1 0][0 1][0 -1]])
(def queen-slides  (concat bishop-slides rook-slides))

(defn move=[move1 move2](and (= (:from move1) (:from move2)) (= (:to move1) (:to move2))))

(defn coords-to-move[ c1 c2 ]{:from (coord-to-square c1) :to (coord-to-square c2)})

;; to-do: refactor the following two to remove common code 
(defn gen-hopper[deltas coord]
  (map (partial coords-to-move coord)(filter coord-ok? (map (partial add-coords coord) deltas))))

(defn gen-ray[delta coord]
  (map (partial coords-to-move coord)(take-while coord-ok? (rest (iterate (partial add-coords delta) coord)))))

(defn gen-rays[rays coord]
  (map #(gen-ray %1 coord) rays))

(defmulti move-tables :Piece)

(defmethod move-tables 'P [p sq] (if (= (:side p) 1) "wp" "bp"))

(defmethod move-tables 'K [p sq] (gen-hopper king-deltas sq))
(defmethod move-tables 'N [p sq] (gen-hopper knight-deltas sq))

(defmethod move-tables 'R [p sq] (gen-rays rook-slides sq))
(defmethod move-tables 'B [p sq] (gen-rays bishop-slides sq))
(defmethod move-tables 'Q [p sq] (gen-rays queen-slides sq))

(defn make-move-table[piece square](move-tables piece (square-to-coord square)))

(defn get-side-fields[piece]
  (if (= (:side piece) 1) [:white-pieces-at :white-material :white-pawns]
                          [:black-pieces-at :black-material :black-pawns] ) )

(defn remove-piece-at[board square](dissoc board square))

(defn add-piece-at[board piece square]
  (let [[piece-side side-material side-pawns] (get-side-fields piece)]
       (-> board
	   (assoc square piece)
	   (assoc piece-side (conj (piece-side board) square))
	   (assoc :material (+ (:material board) (* (:value piece) (:side piece))))
	   (assoc side-material (+ (side-material board) (:value piece)))
	   (assoc :hash-code (bit-xor (:hash-code board) (aget (:hashes piece) square)))
	   )))

(defn print-board[board]
  (let [hr #(print "+---+---+---+---+---+---+---+---+\n")
        ts #(if % % " ")
        pp (fn[sq]
	    (when (zero? (mod sq 8)) (print "\n")(hr)(print "|"))
	    (print " ")
	    (print (ts (:glyph (board sq))))
	    (print " |")
	    )
	]
	(doseq [sq squares] (pp sq))
	(print "\n")(hr)
))
	  
(defn base-board[]
  {:white-pieces-at (set [])
   :black-pieces-at (set [])
   :white-material 0
   :black-material 0 
   :material 0 
   :white-pawns 0
   :black-pawns 0
   :hash-code 0 
   }
   )
   

(defn char-to-pieces[cs]
  (let [p (piece-from-string cs)]
       (if p [p] (repeat (Integer/parseInt cs) nil))))
	      
(defn from-fen[str]
  (let [[board to-move ep-square halfmove_clock full-moves] (clojure.string/split str #" ")
        squares (remove #{"/"} (clojure.string/split board #""))
	slist (mapcat char-to-pieces squares)
	indexed (map (fn[a b][a b]) slist (range))
	board (reduce (fn[b [p sq]](print [b p sq])(if (nil? p) b (add-piece-at b p sq))) (base-board) indexed)
       ]
       board
  ))

(defn slide-piece[board piece from to]
     (let [b2 (remove-piece-at board to)]
	  (add-piece-at b2 (board from) to)))


