(defvar *db* nil)

;; make a plist of a CD record
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; add a CD record to the database
(defun add-record (cd)
  (push cd *db*))

;; print the content of the database
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; read a single property of a CD record from the user prompt
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; read a single CD record from the user prompt
(defun prompt-for-cd ()
  (make-cd (prompt-read "Title")
	   (prompt-read "Artist")
	   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
	   (y-or-n-p "Ripped [y/n]: ")))

;; reads multiple CD record from the user prompt
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
	(if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; save the database in a file
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

;; load the saved database from a file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))

;; filter database entries using 'where' macro
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; macro helper function that creates a comparison expression for a single field
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;; macro helper function that creates a list of comparison expressions
(defun make-comparisons-list (fields)
  (loop while fields
	collecting (make-comparison-expr (pop fields) (pop fields))))

;; replace a list of field and value pairs with a list of comparison expressions
(defmacro where (&rest clauses)
  `#'(lambda (cd)
       (and ,@(make-comparisons-list clauses))))

;; update database entries
(defun update (selector-fn &key title  artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar #'(lambda (row)
		    (when (funcall selector-fn row)
		      (if title (setf (getf row :title) title))
		      (if artist (setf (getf row :artist) artist))
		      (if rating (setf (getf row :rating) rating))
		      (if ripped-p (setf (getf row :ripped) ripped)))
		    row)
		*db*)))

;; delete database entries
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
