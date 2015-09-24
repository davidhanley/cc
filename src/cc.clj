; clojure chess.  Now i get the language better

(defn make-piece[glyph side value] 
      {:glyph glyph :side side :value value})
 
(defn string-to-board
      "Turn a FEN string to a board representation" 
      [str]
      

