(ns k-means.work
  (:use [k-means.core :only [run-empty run-2-circles run-3-circles run-random-circles]]))


;;; Your task is to implement clustering algorithm.
;;; You're a given a set of points on plane. And your goal is to divide them to k clusters.
;;; Implement k-means algorithm to solve this task: http://en.wikipedia.org/wiki/K-means_clustering
;;; Your function must take collection of points. Each point is a vector of x and y.
;;; It must return collection of clusters. Each cluster - collection of points.
;;; E.g. you have 4 points: [0 0] [1 1] [9 9] [10 10] and you need to partition them to 2 clusters.
;;; Input will be [[0 0] [9 9] [1 1] [10 10]] and output should be something like [[[0 0] [1 1]] [[9 9] [10 10]]].
;;; Note that you don't get k - number of clusters. You need to specify it somewhere in function.
;;; To test you solution use following tests:

(defn dist [p1 p2]
    (+ (* (- (first p1) (first p2)) (- (first p1) (first p2)))
       (* (- (second p1) (second p2)) (- (second p1) (second p2)))))

(defn my-min [point cent-1 cent-2]
    (let [defl-1 (dist point cent-1) defl-2 (dist point cent-2)]
        (< defl-1 defl-2)))

(defn find-center [centers point k]
    (reduce #(if (my-min point (nth centers %1) (nth centers %2)) %1 %2)
	0 (range 1 k))) 

(defn transf-clust [v centers k]
    (map #(find-center centers % k) v))

(defn div [point n]
    (vector (/ (first point) n) (/ (second point) n)))

(defn plus [p1 p2]
    (vector (+ (first p1) (first p2)) (+ (second p1) (second p2))))

(defn count-centers-mass [v numb-clust k]
    (loop [sum-points (zipmap (range k) (for [m (range k)] [0 0]))
	   numb-points (zipmap (range k) (for [m (range k)] 0))
           pos 0]
          (if (= pos (count v))
	      (map #(div (sum-points %) (max (numb-points %) 1)) (range k))
	      (recur (assoc sum-points (nth numb-clust pos) (plus (sum-points 
                                              (nth numb-clust pos))
                                                  (nth v pos)))
		     (assoc numb-points (nth numb-clust pos)
               (inc (numb-points (nth numb-clust pos))))
		     (inc pos)))))

(defn form-ans [v numb-clust k]
    (loop [clusters (zipmap (range k) (for [m (range k)] []))
           pos 0]
	(if (= pos (count v))
	    (into [] (vals clusters))
	    (recur (assoc clusters (nth numb-clust pos)
                 (conj (clusters (nth numb-clust pos)) (nth v pos)))
		   (inc pos)))))

(defn k-means [k v]
    (loop [numb-clust (for [m (range (count v))] (rand-int k))
           centers (count-centers-mass v numb-clust k)]
	   (let [new-clust (transf-clust v centers k)]
	       (if (= numb-clust new-clust)
		   (form-ans v numb-clust k)
		   (recur new-clust (count-centers-mass v new-clust k))))))

; (run-empty SOLUTION)

; (run-2-circles SOLUTION)

(run-3-circles #(k-means 3 %))

;;; Manipulation: mouse click - add new point
;;;               space - reset simulation (remove all points or regenerate them, depenends on test)
;;; Note that may need use different solutions (with k = 2 for run-2-circles and  k = 3 for run-3-circles).



;;; Now try to improve your solution so it can determine k based on given points. So if there are visually 3 clusters it should partition points to 3 clusters, if 4 than to 4 clusters.
;;; Test your solution on this test:

; (run-random-circles SOLUTION)



;;; Implement some other clustering algorithm.
