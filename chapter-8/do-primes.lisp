(defun primep (number)
  "Test if the supplied number argument is a prime number"
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Return the next prime number after the supplied number argument"
  (loop for n from number when (primep n) return n))

(defmacro with-gensyms ((&rest names) &body body)
  "Abstract the the instructions that generate a variable name"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  "Abstract a loop that iterates through prime numbers in an interval and
execute a set of instructions"
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))
