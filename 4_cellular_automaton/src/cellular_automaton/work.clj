(ns cellular-automaton.work
  (:use quil.core))



;;; Your task is to implement cellular automaton.
;;; The most famous example of cellular automaton is Conway's Game of Life.
;;; Unlike previous tasks now you have to implement visualization and bots. So you need to implement everything :)
;;; I suggest to use quil library for animation (it was used in all previous tasks): https://github.com/quil/quil
;;; But of course you can use whatever you want.
;;; Keep in mind that is should be simple to run your simulator with different automata (Game of Life is only 1 example).


;;; Implement and run Brian's Brain automaton in your simulator: http://en.wikipedia.org/wiki/Brian%27s_Brain


;;; Implement Wireworld automaton: http://en.wikipedia.org/wiki/Wireworld


;;; Add Wireworld implementation to Rosetta Code (it's not present here yet): http://rosettacode.org/wiki/Wireworld


;;; Implement Von Neumann cellular automaton: http://en.wikipedia.org/wiki/Von_Neumann_cellular_automata


;;; Implement Langton's ant: http://en.wikipedia.org/wiki/Langton%27s_ant


;;; Add ability to change cells' states by mouse click, to restart and pause simulation.


(def m {0 [0 0 0] 1 [0 255 0] 2 [255 0 0] 3 [0 0 255]})


(defn Convey [state neighbours]
  (let [numb (reduce #(if (> %2 0) (inc %1) %1) 0 neighbours)]
    (if (> state 0)
      (if (or (= numb 2) (= numb 3))
        1
        0)
      (if (= numb 3)
        1
        0)
)))

(defn Brian-Brayn [state neighbours]
  (let [numb (reduce #(if (= %2 1) (inc %1) %1) 0 neighbours)]
    (if (= state 1)
      0
      (if (= state 0)
        2
        (if (= numb 2)
          1
          2)))))

(defn Wireworld [state neighbours]
  (let [numb (reduce #(if (= %2 1) (inc %1) %1) 0 neighbours)]
    (if (= state 0)
      0
      (if (= state 1)
        2
        (if (= state 2)
          3
          (if (or (= numb 2) (= numb 1))
            1
            3))))))

(defn update-cell [update-fn state neighbours]
  (update-fn state neighbours
))

(defn find-neighb [ [x y] field]
	(let [dir [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
		(into []
              (map #(nth (nth field (+ x (first %))) (+ y (second %))) dir
))))

(defn update-gen [update-fn field]
	(for [x (range (count field))]
      (for [y (range (count (first field)))]
        (if (and (> x 0) (< x (dec (count field))))
          (if (and (> y 0) (< y (dec (count (first field)))))
            (update-cell update-fn (nth (nth field x) y) (find-neighb [x y] field))
            0)
          0
))))

(defn update [field update-fn]
	(let [new (update-gen update-fn field)]
      	(into [] 
              (map #(into[] %) new)
)))

(defn pre-create [height width n]
	(for [x (range (+ height 2))]
      (for [y (range (+ width 2))]
        (if (and (> x 0) (< x (inc height)))
          (if (and (> y 0) (< y (inc width)))
            (rand-int n)
            0)
          0
))))

(defn create[height width n]
	(let [start (pre-create height width n)]
      (into [](map #(into [] %) start)
)))


(defn draw-sells[field scale]
	(doseq [x (range 1 (dec (count field)))
          y (range 1 (dec (count (first field))))
          :let [elem (nth (nth field x) y)]
          :let [color (m elem)]]
          (fill (first color) (second color) (last color))
          (rect (* scale (dec x)) (* scale (dec y)) scale 
                           scale)))


(defn setup [num-states]
  (smooth)
  (frame-rate 10)
  (background 200)
  (def states (atom (create 50 50 num-states))))

(def states (atom (create 50 50 4)))

(defn draw[update-fn]
	(background 200)
	(draw-sells @states 12)
	(reset! states (update @states update-fn)))

(defn run [update-fn num-states]
  (defsketch automat
       	:title "Hello, BSU" 
 		:setup (fn setup1[](setup num-states)) 
 		:draw (fn draw1[](draw update-fn))
	 	:size [600 600] 
))


;;;It takes approximately 10 second to produce initial state of game
;;;Please, for each game correctly write the number of possible states

(run Wireworld 4)
;;;(run Convey 2)
;;;(run Brian-Brayn 3)