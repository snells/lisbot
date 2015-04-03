(in-package :lisbot)


(defun pack-filter (lst line)
  (cond ((null lst) t)
	((search (car lst) line)
	 (pack-filter (cdr lst) line))
	(t nil)))



(defun parse-packlist (lst path)
  (with-open-file (ss path :if-does-not-exist nil)
    (let ((res-lst '()))
      (do ((line (read-line ss nil nil)
		 (read-line ss nil nil)))
	  ((null line))
	(when (pack-filter lst (subseq line 20))
	  (let ((num (car (find-words line))))
	    (setf res-lst (cons num res-lst)))))
      res-lst)))


(defun parse-print-packlist (lst path)
  (with-open-file (ss path :if-does-not-exist nil)
    (do ((line (read-line ss nil nil)
	       (read-line ss nil nil)))
	((null line))
      (when (pack-filter lst line)
	(format out "~&~a~%" line)))))



(defun get-bot-name (str)
  (let ((res (with-output-to-string (ss)
	       (do* ((point (+ 5 (search "/MSG" str)) (1+ point))
		     (c (aref str point) (aref str point)))
		    ((char= c #\Space))
		 (write-char c ss)))))
    res))


(defmacro operate-file (stream &optional (line (gensym)) (result-body nil) &body body)
    `(do ((,line (read-line ,stream nil nil)
		 	       (read-line ,stream nil nil)))
              ((null ,line) ,result-body)
	           ,@body))




(defun replace-file (input output)
    (with-open-file (out output :direction :output :if-exists :supersede 
			 		       :if-does-not-exist :create)
          (if (probe-file input)
	    	(with-open-file (in input)
		  	  (operate-file in  x  'done 
						    (write-line x out)))
			(if (stringp input)
			  	    (write-line input out)
				    	    (print "not file or sring")))))

