;;; test.lisp
;;; Used for getting used to Common Lisp

(defvar *PI* 3.14)

(defun square (x)
	"Returns x^2"
	(* x x))

(defun fact (x)
	"Returns x!"
	(if (<= x 1)
		  1
		  (* x (fact (- x 1)))))

(defun fib (n)
	"Returns the nth Fibonacci number"
	(cond
		((= n 0) 0)
		((= n 1) 1)
		(t (+ (fib (- n 1)) (fib (- n 2))))))

(defun fib_fast (n)
	"Returns an approximation of the nth Fibonacci number using the Golden Ratio"
	(/ (expt
		 (/ (+ 1 (sqrt 5)) 2)
		 n)
		(sqrt 5)))

(defun average (lst)
	"Returns the average of all numbers in lst"
	(defun average_helper (lst sum count)
		(if (eq nil lst)
			  (/ sum count)
			  (average_helper (cdr lst) (+ sum (car lst)) (+ count 1))))

	(average_helper lst 0 0))

(defun map1 (fn lst)
	"Applifes fn to every element in lst"
	(if (eq nil lst)
		  nil
		  (cons
			  (funcall fn (car lst))
			  (map1 fn (cdr lst)))))

(defun fold (fn lst acc)
	"Recursively apply fn to the lst
	 E.g. (fn (fn (... (fn acc e1) ,,, ) eN-1) eN)"
	(if (eq nil lst)
		  acc
		  (fold fn (cdr lst) (funcall fn acc (car lst)))))

(write (fold #'* (list 1 2 3 4) 1))
