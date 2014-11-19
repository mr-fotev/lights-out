(ns lightsout.core (:use [seesaw core color graphics behave]))

(defn solvable-board []
  (let [v (vec (map rand-int (repeat 12 2)))] 
    (letfn [(mrg [v1 v2] (if (= v2 []) v1 (mrg (conj v1 (first v2)) (rest v2))))] (mrg (conj v (rand-int 2)) (vec (reverse v))))))

(defn move [v n]
  (letfn [(flip [y] (-> y v inc (rem 2)))]
    (cond 
        (= n 0) (assoc v 0 (flip 0), 1 (flip 1), 5 (flip 5))
	(= n 4) (assoc v 4 (flip 4), 3 (flip 3), 9 (flip 9))
	(= n 20) (assoc v 20 (flip 20), 15 (flip 15), 21 (flip 21))
	(= n 24) (assoc v 24 (flip 24), 23 (flip 23), 19 (flip 19)) 
	(= (quot n 5) 0) (assoc v n (flip n), (dec n) (flip (dec n)), (inc n) (flip (inc n)), (+ n 5) (flip (+ n 5)))
        (= (quot n 5) 4) (assoc v n (flip n), (dec n) (flip (dec n)), (inc n) (flip (inc n)), (- n 5) (flip (- n 5)))
        (= (rem n 5) 0) (assoc v n (flip n), (- n 5) (flip (- n 5)), (+ n 5) (flip (+ n 5)), (inc n) (flip (inc n)))
        (= (rem n 5) 4) (assoc v n (flip n), (- n 5) (flip (- n 5)), (+ n 5) (flip (+ n 5)), (dec n) (flip (dec n)))
        :else (assoc v n (flip n), (- n 5) (flip (- n 5)), (+ n 5) (flip (+ n 5)), (dec n) (flip (dec n)), (inc n) (flip (inc n))))))
	   
(def x (atom (solvable-board)))

(def game-window (frame :title "game", :height 400, :width 600, :on-close :exit, :resizable? false))

(defn init []
  (xyz-panel :items 
    (conj
	(vec (for [i (range 5), j (range 5), :let [d (+ (* 5 i) j)]]
		(button :background (if (= (@x d) 0) (color 250 250 50) (color 100 100 255))
			:bounds [(+ 45 (* j 61)) (+ 35 (* i 61)) 60 60]
	 		:listen [:action (fn [e] (do (swap! x move d) 
		 		                     (config! game-window :content (init))
                                                     (when (= (vec (repeat 25 0)) @x) 
							(alert "You win!") 
							(swap! x (fn [y] (solvable-board)))
                                                        (config! game-window :content (init)))))])))
	
	  	(button :text "New game" :foreground :white :font "ARIAL-BOLD-18"
			:background :blue
			:bounds [400 100 150 60]
			:listen [:action (fn [e] (do (swap! x (fn [y] (solvable-board))) (config! game-window :content (init))))]))))

(config! game-window :content (init))

(defn -main [& args] (show! game-window))
