(setq ext:*warn-on-redefinition* nil)

; Common Lisp Exercises by Oliver Otcasek


;List functions

(defun append (l1 l2) 
	(if (null l1) l2
	(cons (car l1) (append(cdr l1) l2))))

(defun reverse (l)
	(if (null l) 
		nil
		(append (reverse (cdr l)) (list (car l)))))

(defun addtoend (a l)
	(if (null l) nil
		(reverse (cons a (reverse l)))))

(defun remove-all (a l)
	(if (null l)
	 nil
		(if (equal (car l) a)
			(remove-all a (cdr l))
			
			(cons (car l) (remove-all a (cdr l))))))

(defun map (q l)
	(if (null l) nil
		(cons (funcall q (car l)) (map q (cdr l)))))

;Set functions

(defun member(a s)
	(if (or (null s) (null a)) nil
		(if (equal (car s) a) t
			(member a (cdr s)))))


(defun cardinality (a)
	(cardinality-help a 0))


(defun cardinality-help (a b)
	(if (null a) b
		(cardinality-help (cdr a) (+ b 1))))


(defun difference(s a)
	(if (or (null s) (null a)) nil
		(if (not (member (car s) a)) (car s)
			(difference (cdr s) (cdr a)))))

(defun intersection(s a)
	(if (or (null s) (null a)) nil
		(if (member (car a) s) ;Gotta love O(n^2)!
			(cons (car a) (intersection (cdr s) (cdr a)))
			(intersection (cdr s) (cdr a)))))



;Math functions
(defun remo(a b)
	(if (>= a b)
		(remo a (+ b 1))
		(- b 1)))


(defun mod(n s) ;n mod s
	(if(> s n) n
		(- n (* s (remo (/ n s) 1)))))


(defun factorial(i)
	(if (equal i 1) 1
		(* i (factorial (- i 1)))))

(defun gcd(n l) ; Euclid's algorithm
	(if (equal n 0) l
		(gcd (mod l n) n)))

(defun primeph (a b)
	(if (>= b a) t
		(if (equal (mod a b) 0) nil
			(primeph a (+ b 1)))))

(defun primep (a) ; 4111 is the max this can find without a stack overflow.
	(if (equal a 1) ; In the project, 1 is not considered a prime.
		nil
		(primeph a 2)))
	


(defun nth-primep(a b c)
	(if (equal a c) (- b 1)
		(if (primep b)
			(nth-primep a (+ b 1) (+ c 1))
			(nth-primep a (+ b 1) c))))

(defun nth-prime(a)
	(nth-primep a 2 0))





;required


(defun perfect_recursive(n i)
	(if (equal n i) 0
		(if (equal 0 (mod n i)) (+ (perfect_recursive n (+ i 1)) i)
			(perfect_recursive n (+ i 1)))))



(defun perfectp(n)
	(if (equal (perfect_recursive n 1) n) t
		nil))

(defun abundantp (n)
	(if (> (perfect_recursive n 1) n) t nil))

(defun deficientp (n)
	(if (< (perfect_recursive n 1) n) t nil))





; Setup functions

(defun repl-manager (fun a b c)
	(if (equalp (format nil "~a" a) "quit")
	nil
	(print-f fun a b c)))


(defun print-f (fun a b c)
	(if (eq b nil)
		(format t "(~a ~a) = ~a~%" fun a (funcall fun a))
		(if (eq c nil)
			(format t "(~a ~a ~a) = ~a~%" fun a b (funcall fun a b))
            (format t "(~a ~a ~a ~a) = ~a~%" fun a b c (funcall fun a b c))))t)



; setup.

(defun main()
	(format t "~%LIST FUNCTIONS.~%")
	(format t "~%Testing append function.~%")
	(REPL_2_args `append "~%Please input two lists or type \"quit\" to quit: ")
	(format t "~%Testing reverse function.~%")
	(REPL_1_arg `reverse "~%Please input a list or type \"quit\" to quit: ")
	(format t "~%Testing remove all function.~%")
	(REPL_2_args `remove-all "~%Please input two lists or type \"quit\" to quit: ")
	(format t "~%Testing add to end function.~%")
	(REPL_2_args `addtoend "~%Please input two lists or type \"quit\" to quit: ")
	(format t "~%Testing map function.~%")
	(REPL_2_args `map "~%Please input a function name and a list or type \"quit\" to quit: ")


	(format t "~%SET FUNCTIONS.~%")
	(format t "~%Testing member function.~%")
	(REPL_2_args `member "~%Please input two lists or type \"quit\" to quit: ")
	(format t "~%Testing cardinality function.~%")
	(REPL_1_arg `cardinality "~%Please input one list or type \"quit\" to quit: ")
	(format t "~%Testing difference function.~%")
	(REPL_2_args `difference "~%Please input two lists or type \"quit\" to quit: ")
	(format t "~%Testing intersection function.~%")
	(REPL_2_args `intersection "~%Please input two lists or type \"quit\" to quit: ")


	(format t "~%MATH FUNCTIONS.~%")
	(format t "~%Testing factorial function.~%")
	(REPL_1_arg `factorial "~%Please input a number or type \"quit\" to quit: ")
	(format t "~%Testing gcd function.~%")
	(REPL_2_args `gcd "~%Please input two numbers or type \"quit\" to quit: ")
	(format t "~%Testing primep function.~%")
	(REPL_1_arg `primep "~%Please input a number or type \"quit\" to quit: ")
	(format t "~%Testing nth prime function.~%")
	(REPL_1_arg `nth-prime "~%Please input a number or type \"quit\" to quit: ")
	(format t "~%REQUIRED FUNCTIONS.~%")


	(format t "~%Testing perfectp function.~%")
	(REPL_1_arg `perfectp "~%Please input a number or type \"quit\" to quit: ")
	(format t "~%Testing abundantp function.~%")
	(REPL_1_arg `abundantp "~%Please input a number or type \"quit\" to quit: ")
	(format t "~%Testing deficientp function.~%")
	(REPL_1_arg `deficientp "~%Please input a number or type \"quit\" to quit: ")t)





(defun REPL_1_arg (a b)
	(format t b)
	(finish-output nil)
	(if (repl-manager a (read) nil nil)
		(REPL_1_arg a b)))

(defun REPL_2_args (a b)
	(format t b)
	(finish-output nil)
	(if (repl-manager a (read) (read) nil)
		(REPL_2_args a b)))
