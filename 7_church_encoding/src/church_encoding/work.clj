(ns church-encoding.work
  (:use [church-encoding.core]))

;;; Task is to implement arithmetic on Church numerals.
;;; Check this page: http://en.wikipedia.org/wiki/Church_encoding
;;; You can use utility function to-church-num and to-normal-num to convert normal to church and church to normal:
;;; Note that to-church-num returns function that takes 1 argument (f)
;;; and returns function that takes 1 argument (x) that calculates (f (f ... (f x)...))
;;; All functions in this task must 1 argument functions that return other functions.

;;; Example:

(def church-5 (to-church-num 5))    ; 5 in church numerals

(defn print-star [x] (print "*") x) ; Takes 1 argument, prints a star and retuns argument without modification.

((church-5 print-star) nil)         ; Prints ***** to console

(to-normal-num church-5)            ; returns 5

(def church-2 (to-church-num 2))    ; we'll use it in examples later



;;; Implement + (plus) for church numerals.

(defn PLUS[m]
    (fn [n]
        (fn [f]
            (fn [x]
		((m f) ((n f) x))))))

(def plus PLUS)

(to-normal-num ((plus church-2) church-2)) ; must return 4

(test-plus plus) ; test your solution



;;; Implement * (multiplication) for church numerals

(defn MULT[m]
    (fn [n]
	(fn [f]
	    (m (n f)))))

(def mult MULT)

(to-normal-num ((mult church-2) church-5)) ; must return 10

(test-mult mult) ; test your solution



;;; Implement ^ (pow function) for church numerals.

(defn POW[a]
    (fn [x]
	(x a)))

(def pow POW)

(to-normal-num ((pow church-2) church-5)) ; must return 32

(test-pow pow) ; test your solution



;;; Implement dec function for church numerals.

(defn TRUE[x]
    (fn[y] x))

(defn FALSE[x]
    (fn [y] y))

(defn PAIR [x]
    (fn [y]
	(fn [f]
	    ((f x) y))))

(defn FIRST [p]
    (p TRUE))

(defn SECOND [p]
    (p FALSE))

(defn INC[n]
    (fn [f]
	(fn [x]
	    (f ((n f) x)))))

(defn INC-SHIFT[p]
    ((PAIR (SECOND p)) (INC (SECOND p))))

(def church-0 (to-church-num 0))
(def church-1 (to-church-num 1))

(defn DEC[n]
    (FIRST ((n INC-SHIFT) ((PAIR church-0) church-0))))

(def dec DEC)

(to-normal-num (dec church-5)) ; must return 4

(test-dec dec) ; test your solution



;;; Implement sum function. sum takes number n and returns sum of all numbers less or equals to n.
;;; You'll need to use recursion here. For recursion you'll need lazy values.
;;; You can use delay for that: http://clojuredocs.org/clojure_core/1.2.0/clojure.core/delay

(defn IS-ZERO [n]
    ((n (fn [x] FALSE)) TRUE))

(defn Y[f] ((fn [x] (f (fn[v] ((x x) v))))
            (fn [x] (f (fn[v] ((x x) v))))))

(defn SUM-N[n] ((Y (fn [f] (fn [n] @( ((IS-ZERO n) (delay church-0)) 
                             (delay ((PLUS n) (f (DEC n)))) )  ))) n))

(def sum SUM-N)

(to-normal-num (sum church-2)) ; must return 3

(test-sum sum)


;;; Implement set of function to create/manipulate lists.
;;; Your need to implement following functions:
;;; empty? - checks if list is empty, returns true or false. see church booleans http://en.wikipedia.org/wiki/Church_encoding#Church_booleans
;;; empty-list - used as "end" of the list.
;;; head - returns head of a list
;;; tail - returns tail of a list
;;; cons - takes 2 arguments h and t, and creates a list such that (head (cons a b)) = a, (tail (cons a b)) = b
;;;
;;; Help: http://en.wikipedia.org/wiki/Church_encoding#List_encodings

(def NIL ((PAIR TRUE) TRUE) )

(defn EMPTY?[l] (FIRST l))

(def EMPTY-LIST NIL)

(defn HEAD[l] (FIRST (SECOND l)))

(defn TAIL[l] (SECOND (SECOND l)))

(defn CONS[h] (fn [t] ((PAIR FALSE) ((PAIR h) t))))

(def empty? EMPTY?)

(def empty-list EMPTY-LIST)

(def head HEAD)

(def tail TAIL)

(def cons CONS)

(((empty? empty-list) true) false) ; must return true

;(head (cons "Hello" empty-list)) ; must return "Hello"

;(let [list (cons "Hello" empty-list)
;      t (tail list)]
;  ((empty? t) true) false) ; must return true

(test-list {:empty? empty?
            :empty-list empty-list
            :head head
            :tail tail
            :cons cons}) ; test your solution



;;; Additional task.
;;; Implement map and reduce functions for lambda lists.
;;; map takes 2 arguments: function and list
;;; reduce takes 3 arguments: function, init value and list

(def map :YOUR_IMPLEMENTATION_HERE)

(def reduce :YOUR_IMPLEMENTATION_HERE)

(test-map-reduce {:empty? empty?
                  :empty-list empty-list
                  :head head
                  :tail tail
                  :cons cons
                  :map map
                  :reduce reduce})
