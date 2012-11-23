(ns snake.work
  (:use [snake.core :only (run-not-grow run-grow run-many-apples run-with-walls)]))

;;; You're writing a bot for playing snake.
;;; So, you are a snake and your goal is to collect apples.
;;; Field sizes: 40 x 30
;;; Every turn you move to one of the neighbours cell.
;;; Your function must take 2 arguments: snake's position and apple's position and decide which direction to move.
;;; Directions are: :up, :down, :left, :right (they are keywords). Your function must return one of these directions.
;;; Position (snake's or apple's) is a vector of 2 elements: x and y.
;;; In this task snake is not growing after it ate an apple so there is no danger of snake hitting itself.
;;; Note: upper left corner cell is (0, 0).

;;;(defn my_run-not-grow [v1 v2]
;;;    (let [x1 (first v1) y1 (last v1) x2 (first v2) y2(last v2)]
;;;     (if (= x1 x2) 
;;;            (if (< y1 y2) :down :up)
;;;            (if (< x1 x2) :right :left)
;;;)))

;;; Uncomment and substitute your solution
;;;(run-not-grow my_run-not-grow)



;;; Snake grows now (each time snake eats an apple, it's length increases).
;;; You need to write similar function as in previous task.
;;; It takes 2 arguments.
;;; First argument is snake's body - collection of cells, each cell is a vector of x and y. First cell is snake's head.
;;; Second argument is apple's position - vector of x and y.
;;; It should return direction: :up, :down, :left or :right.
;;; Note that you cannot change direction to the opposite in 1 move: snake will hit it's tail if length is 2 or more.
;;; Wait, you can change direction but snake will die :\

;;; Uncomment and substitute your solution
;;;(run-grow my_run-grow)



;;; Now you have many apples (5) instead of one.
;;; Function the same as previous but it takes set of apples instead of the single apple.
;;; Each apple in the set is a vector of x and y.
;;; E.g. you can try to reach nearest apple to the snake.

;;; Uncomment and substitute your solution
; (run-many-apples YOUR_SOLUTION_HERE)



;;; Walls are added. So snake can hit wall and die.
;;; Your function now takes third argument - set of walls.
;;; Each wall is a cell that snake is not allowed to  move to.
;;; Wall is a vector of x and y.

(defn get-neighb[cell]
  (let [dir [[0 1] [0 -1] [-1 0] [1 0]]]
   (map #(map + cell %) dir)))

(defn is-any-equal [cell other-cells]
  (some #(and (= (first cell) (first %)) (= (last cell) (last %)))
        other-cells))

(defn non-border [[x y]]
  (and (>= x 0) (>= y 0) (< x 40) (< y 30)))

(defn find-next [cell forb-cells]
  (->> cell
       (get-neighb)
       (remove #(is-any-equal % forb-cells))
       (filter non-border)))

(defn get-first-steps [head forb-cells]
  (let [dir [:down :up :left :right] coord [[0 1][0 -1][-1 0][1 0]]
    	neighb (map #(vector %1 (map + head %2)) dir coord)]
	(->> neighb
    	(remove #(is-any-equal (last %) forb-cells))
         (filter #(non-border (last %))))))

(defn my-run-with-walls[body apples walls]
  (let [head (first body) used-cells (concat body walls)]
    (loop [forb-cells used-cells
           q (get-first-steps head used-cells)]
      (let [elem (first q)]
        (if (is-any-equal (last elem) apples)
          (first elem)
		  (let [new-cells (map #(vector (first elem) %)
                                       (find-next (last elem) forb-cells))]
            (recur (concat forb-cells (map last new-cells))
                   (concat (rest q) new-cells)
      )))))))

;;; Uncomment and substitute your solution
 (run-with-walls my-run-with-walls)