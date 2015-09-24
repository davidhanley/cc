(ns cc.core
  (:gen-class))

(defn l[] (use 'cc.core :reload))

(defn axis-ok?[axis](and (>= axis 0) (< axis 8)))
(defn coord-ok?[[r f]](and (axis-ok? r) (axis-ok? f)))
(defn square-ok?[square](and (>= square 0) (<= square 63)))

(defn square-to-coord[square][(quot square 8)  (mod square 8)])
(defn coord-to-string[[r f]](str (get "abcdefgh" f) (get "87654321" r)))
(defn square-to-string[sq](assert (square-ok? sq)) (coord-to-string (square-to-coord sq)))

(def squares (range 0 63))
(doseq [sq squares](eval (list 'def (symbol (square-to-string sq)) sq)))

(defn add-coords[[r1 f1] [r2 f2]] [(+ r1 r2) (+ f1 f2) ])


(defmacro piece[n g s v](list 'def n {:glyph g :side s :value v}))
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

(defn remove-piece-at[board square](dissoc board square))

(defn add-piece-at[board piece square](assoc board square piece))

(defn slide-piece[board piece from to]
     (let [b2 (remove-piece-at board to)]
	  (add-piece-at b2 (get board from) to)))

    
   


