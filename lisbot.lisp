;;;; lisbot.lisp

(in-package #:lisbot)

(defvar out *standard-output*)
(defvar connections '())
(defvar verbose nil)
(defvar default-dir (user-homedir-pathname))
(defvar pack-dir (merge-pathnames ".lisbot/" default-dir))
(defvar schedu-path (merge-pathnames "schedule" pack-dir))
(defvar packlist-path (merge-pathnames "packlist" pack-dir))
(defvar requests '(() ()))
(defvar known-bots '(("DeadFishXDCC" . "#[deadfish]")))
(defvar *all* nil)

(defmacro do-stream (var stream end-condition &body body)
  `(do ((,var (read-line ,stream nil nil)
	      (read-line ,stream nil nil)))
       ,end-condition
     ,@body))

(defun cdr-ass (key soc)
  (cdr (assoc key soc :test #'string-equal)))

(defun get-connection (key name)
  (cdr-ass key (cdr-ass name connections)))
(defun connection-socket (name)
  (get-connection 'socket name))
(defun connection-stream (name)
  (get-connection 'stream name))
(defun connection-server (name)
  (get-connection 'server name))
(defun connection-nick (name)
  (get-connection 'nick name))
(defun connection-dl-list (name)
  (get-connection 'dl name))

(defun read-stream (name)
  (do-stream line (connection-stream name) ((null line) (format out "~&Disconnected ~a ~%" name))
    (process-line name (delete (code-char 1) line))))

(defun connect (name host nick user &optional (password nil))
  (handler-case
      (let* ((socket (usocket:socket-connect host 6667))
	     (stream (usocket:socket-stream socket)))
	(push (cons name (list (cons 'socket socket)
			       (cons 'stream stream)
			       (cons 'server host)
			       (cons 'nick nick)))
	      connections)
	(register stream nick user password)
	(bt:make-thread (lambda () (read-stream name)))
	t)
    (error (e)
      (format out "Error while connecting server ~a~%~a~%" host e)
      nil)))
      


(defun register (str name usr password)
  (if password (password password str))
  (if name (nick name str))
  (if usr (user str usr 0 "empty-name")))

(defun send (str msg &rest stuff)
  (apply #'format str msg stuff)
  (write-char #\Return str)
  (write-char #\Linefeed str)
  (finish-output str))

(defun password (password str)
  (send str "PASS ~A" password))

(defun quit-connection (name)
  (send (connection-stream name) "QUIT")
  (format out "~&Left from server ~a~%" name))

(defun nick (name str)
  (send str "NICK ~A" name))

(defun user (str usr mode real-name)
  (send str "USER ~A ~A * :~A" usr mode real-name))

(defun msg (str target msg)
  (send str "PRIVMSG ~A :~A" target msg))

(defun pong (str serv)
  (send str "PONG ~A" serv))



(defun process-line (name line)
  (when verbose (format out "~&~A~%" line))
  (when (search "PING" line :end2 4)
    (pong (connection-stream name) (server-from-line line)))
  (when (search "queue" line)
    (print line out))
  (let ((info (cl-ppcre:all-matches-as-strings "DCC SEND .*" line)))
    (when info (when-dcc  (car info)))))


(defun make-request (x)
  (push x (car requests)))

(defun request-dl-started ()
  (push (caar requests) (cadr requests))
  (pop (car requests)))


(defun dl-finished ()
  (pop (cadr requests)))
	   


	   

(defun server-from-line (line)
  (car (cl-ppcre:all-matches-as-strings "irc.\\w+.\\w+" line)))


(defun find-words (str)
	   (do ((counter 0 (1+ counter))
		(res "")
		(lst '()))
	       ((= counter (length str)) (nreverse (push res lst)))
	     (let ((obj (elt str counter)))
	       (if (or (char= obj #\ )
		       (char= obj #\Space)
		       (char= obj #\Tab))
		   (if (string/= "" res) (prog1 (push res lst) (setf res "")))
		   (setf res (concatenate 'string res (string obj)))))))


(defun dec-to-ip (num)
  (let* ((first (floor (/ num 16777216)))
	 (first-rest (- num (* first 16777216)))
	 (second (floor (/ first-rest 65536)))
	 (second-rest (- first-rest (* second 65536)))
	 (third (floor (/ second-rest  256)))
	 (third-rest (- second-rest (* third 256)))
	 (fourth (floor third-rest)))
    (format nil "~d.~d.~d.~d" first second third fourth)))


(defun name-part (lst len)
	   (cond ((= len 0) lst)
		 (t (name-part (butlast lst) (1- len)))))

(defun form-name (lst)
  (let ((res (with-output-to-string (ss)
	       (dolist (str lst)
		 (do ((c 0 (1+ c))
		      (len (length str)))
		     ((= c len))
		   (let ((x (aref str c)))
		     (cond ((char= x #\Space))
			   ((char= x #\"))
			   ((char= x #\'))
			   ((char= x #\[))
			   ((char= x #\]))
			   ((char= x #\{))
			   ((char= x #\}))
			   ((char= x #\())
			   ((char= x #\)))
			   (t (write-char x ss)))))))))
    res))


(defun report-progress (stream counter)
  (do ((shift -24 (incf shift 8)))
      ((> shift 0) (finish-output stream))
    (write-byte (logand #xff (ash counter shift)) stream)))

(defun dcc-download (name ip port seize report &optional (folder default-dir))
  ;(ensure-directories-exist folder)
  (let* ((soc (usocket:socket-connect ip port :element-type '(unsigned-byte 8) :timeout 10))
	 (stream (usocket:socket-stream soc)))
    (format out "~&starting to download ~a size ~a~%" name (convert-size seize))
    (force-output out)
    (with-open-file (file-out (merge-pathnames (pathname-name name) folder)
					; (make-pathname :directory folder
                ;                         :name (pathname-name name)       
					;			     :type (pathname-type name))
			      :direction :output 
			      :element-type '(unsigned-byte 8) :if-exists :supersede
			      :if-does-not-exist :create)
      (do ((counter 0 (1+ counter))
	   (buffer 0 (1+ buffer)))
	  ((= counter seize) (format out "~&done~%" name)
	   (report-progress stream counter)  (dl-finished))
	(when (and (= buffer 1024) (not *all*))
	  (setf buffer 0)
	 ; (if report (report-progress stream counter))
	  (format out "~3,d%" (floor (* 100 (/ counter seize))))
	  (finish-output out)
	  (dotimes (x 4)
	    (princ #\Backspace out)))
					;	  (finish-output))
	(write-byte (read-byte stream) file-out)))))
					;      (do ((counter 0 (incf counter)))
					;	  ((= counter seize) (format out "~&done~%"))
					;	(write-byte (read-byte stream) file-out)))))


(defun find-zeros (num)
  (cond ((< num 1000) 0)
	(t (+ 1 (find-zeros (/ num 1000))))))

(defun convert-size (size)
  (let ((zeros (find-zeros size)))
    (concatenate 'string (format nil "~$~a"
				 (/ size  (if (>= zeros 5) (expt 1000 4) (expt 1000 zeros)))
				 (string (cond ((< zeros 1) 'bytes)
					       ((< zeros 2) 'Kb)
					       ((< zeros 3) 'Mb)
					       ((< zeros 4) 'Gb)
					       ((< zeros 5) 'Tb)))))))

(defun part-of-dl (name list)
  (if (listp (car list))
      (part-of-dl name (car list))
      (labels ((test-fn (lst)
		 (cond ((null lst) t)
		       ((search (car lst) name)
			(test-fn (cdr lst)))
		       (t nil))))
	(or (test-fn list)
	    (search "#" (car list)))))) 
	

(defun when-dcc (info)
  (let ((stuff (cddr (find-words info))))
    (if (cl-ppcre:all-matches "denied" (car stuff))
	(format out info)
	(let* ((len (length stuff))
	       (ip (dec-to-ip (read-from-string (nth (- len 3) stuff))))
	       (port (read-from-string (nth (- len 2) stuff)))
	       (seize (read-from-string (nth (- len 1) stuff)))
	       (name-first (form-name (name-part stuff 3)))
	       (name (if (search "list" (caar requests)) "packlist" name-first)))
	 ;(if (search "packlist" name-first) "packlist" name-first)))
	 ; (if  (part-of-dl name dl-list)
	  (request-dl-started)
	;  (format out "~&~a with size ~a~%" name (convert-size seize))
					;Attempting to connect ~a:~a~%" name (convert-size seize)  ip port)
	  (bt:make-thread
	   (lambda () (dcc-download name ip port seize t (if (search "packlist" name)
							     pack-dir
							     default-dir))))))))
							     
	  
;	  (msg (con-str con) *target* "XDCC cancel"))


(defun generate-nick ()
  (let* ((chars '(a b c d e f g h i j k l m n o p q r s t u v x y z w 0 1 2 3 4 5 6 7 8 9))
	 (len (length chars))
	 (res (with-output-to-string (ss)
		(princ #\z ss)
		(dotimes (x 7)
		  (princ (nth (random len) chars) ss)))))
		res))


(defun join-channel (name channel)
  (send (connection-stream name) "JOIN ~A" channel))


(defun key-p (name a-list)
  (assoc name a-list :test #'string-equal))
(defun get-param (name a-list)
  (cdr (assoc name a-list :test #'string-equal)))

(defun init ()
  (ensure-directories-exist pack-dir)
  (let ((conf (merge-pathnames "conf.lisp" pack-dir)))
    (if (probe-file conf)
	(load conf)
	(with-open-file (out conf :direction :output
			     :if-does-not-exist :create)
	  (write-line "(in-package :lisbot)

; comment

; (setf known-bots '((\"DeadFishXDCC\" . \"#[deadfish]\")))"))))) 

(defun main ()
  (init)
  (multiple-value-bind (values param-vals errors)
		  (getopt:getopt sb-ext:*posix-argv* '(("verbose" :optional nil)
						       ("help" :optional nil)
						       ("file" :optional nil)
						       ("create" :optional nil)
						       ("directory" :optional nil)
						       ("search" :optional nil)
						       ("get" :optional nil)
						       ("all" :optional nil)
						       ("password" :optional nil)))
    (declare (ignore errors))
    (let ((pass (if (key-p "password" param-vals)
		    (get-param "password" param-vals)
		    nil))
	  (vals (cdr values))
	  (s-val (get-param "search" param-vals)))
      (when (key-p "all" param-vals)
	(setf *all* t))
      (when (key-p "directory" param-vals)
	(setf default-dir (get-param "directory" param-vals)))
      (when (key-p "get" param-vals)
	(get-list (get-param "get" param-vals) (car vals) pass)
	(setf vals (cdr vals)))
      (when (key-p "verbose" param-vals)
	(setf verbose t))
      (cond ((key-p "help" param-vals) (print-help))
	    ((key-p "create" param-vals) (create-schedule (cons (get-param "create" param-vals)
								vals)))
	    ((key-p "file" param-vals)
	     (dl-from-file (if (null (get-param "file" param-vals)) schedu-path
			       (get-param "file" param-vals)) pass))
	    ((key-p "search" param-vals) (if (probe-file s-val)
					     (parse-print-packlist vals s-val)
					     (parse-print-packlist (cons s-val vals) packlist-path)))
	    (t (dl-from-cmd vals pass))))
));    (sb-ext:exit)))

(defun print-help ()
  (format t 
	  "Usage
lisbot [options] [server] [xdcc-bot-name] [packages...]
examples
lisbot irc.rizon.net my-xdcc-bot '#100' list '#4'
lisbot --directory /home/dir/ irc.rizon.net my-bot '#100' 
t-llisbot --create '21 3 2014 1 13 irc.rizon.net my-xdcc-bot <name-of-chi> <nese-cartoo> <n>
lisbot --file /home/user/my-generated-file


-h --help~20Tprints this message
-v --verbose~20Tprints servers messages
-c --create~20TCreates schedule 
-f --file~20TDownloads from schedule defaults to .lisbot/schedule
-d --directory~20Tspecify directory where to download, defaults to home directory
-p --password~20TIf server requires password
-g --get~20Tget list
-s --search~20TPrints out all matching lines from packlist


Creating schedule *** not 100% succes rate ***
Start with date in form day month year,
first-episode-number, how-many-times, server, bot search-terms~%"))


(defun connect-channels (name target)
  (dolist (x known-bots)
    (if (search target (car x))
	(join-channel name (cdr x))
	(sleep 2))))

(defun dcc-request (stream target package)
  (send stream "PRIVMSG ~A :xdcc send ~A" target package))


(defun dl-from-cmd (lst pass)
  (cond ((< (length lst) 3)
	 (format out "Needs more parameters~%"))
	(t (let* ((server (car lst))
		  (target (cadr lst))
		  (packs (cddr lst))
		  (con-name (get-connection-name-server server))
		  (lst '()))
	     (when (connect "1" server (generate-nick) "Anon" pass)
	       (sleep 3)
					;    (dcc-request (connection-stream "1") target (car packs))))
	       (connect-channels con-name target)
	       (dolist (pack packs)
		 (push (list (connection-stream con-name) target pack) lst))
	       (setf lst (reverse lst))
	       (download-packs lst t))))))

	   
(defun download-packs (lst end)
  (dolist (info lst)
    (let ((stream (first info))
	  (target (second info))
	  (pack (third info)))
      (make-request pack)
      (dcc-request stream target pack)
      (do ((counter 0 (1+ counter)))
	  ((and (null (car requests))
		(or *all* (null (cadr requests)))))
	(when (null (cadr requests))
	  (when (= counter 4)
	    (format out "~&Requesting ~a from ~a again~%" pack target)
	    (dcc-request stream target pack)
	    (setf counter 0))
	  (sleep 5)))))
  (do ()
      ((null (cadr requests))
       (if end (sb-ext:exit)))
    (sleep 1)))



;    (do* ((len (length (car requests)))
;	(x 0 (- len (length (car requests)))))
;       ((= (length (caddr requests)) (length packs)) (sb-ext:exit))
;    (unless (null (car requests))
;      (dcc-request stream target (nth x packs))
;      (sleep 20))))
;      

(defun create-schedule (lst)
  (multiple-value-bind (day month year first times server bot) (values-list lst)
    (let ((search-terms (subseq lst 7)))
      (make-schedule (read-from-string day)
		     (read-from-string month)
		     (read-from-string year)
		     (read-from-string first)
		     (read-from-string times) server bot search-terms))))


(defun extend-file (input output)
  (with-open-file (out output :direction :output :if-does-not-exist :create :if-exists :append)
    (if (probe-file input)
	  (with-open-file (in input)
	    (do ((line (read-line in nil nil)
			     (read-line in nil nil)))
		      ((null line) 'done)
		    (write-line line out)))
	  (if (stringp input)
	      (write-line input out)
	      (format t "not file or string ~a" input)))))



(defun make-schedule (day mon year first-ep times server name &rest show)
  (cond ((< times 0))
	(t (extend-file (format nil "~d ~d ~d ~a ~a ~{~a ~}~2,'0d" day mon year server name (car show) first-ep)
			(merge-pathnames "schedule" pack-dir))
	   (multiple-value-bind (y m d) (date-calc:add-delta-days year mon day 7)
	     (apply #'make-schedule `(,d ,m ,y ,(+ 1 first-ep) ,(- times 1) ,server ,name ,@show))))))


      
(defun correct-day (lst)
  (multiple-value-bind (s m h d mon y) (get-decoded-time)
    (declare (ignore s m h))
    (multiple-value-bind (year month day) (date-calc:delta-ymd y mon d
							       (read-from-string (caddr lst))
							       (read-from-string (cadr lst))
							       (read-from-string (car lst)))
      (cond ((< year 0) t)
	    ((= year 0)
	     (if (< month 0) t
		 (if (= month 0)
		     (<= day 0) t)))))))



(defun connected-server (server)
  (if (null connections) nil
      (let ((return nil))
	(dolist (x connections)
	  (if (string-equal server (connection-server (car x)))
	      (return t))))))

(defun get-connection-name-server (server)
  (if (null connections) "1"
      (let ((return nil))
	(dolist (x connections)
	  (if (string-equal server (connection-server (car x)))
	      (progn (setf return (car x)))))
	(if (null return) (string (+ 1 (length connections))) return))))
      

(defun get-list (server target pass &optional (end nil))
  (let ((name (get-connection-name-server server)))
    (when (null (assoc name connections))
      (when (connect name server (generate-nick) "Anon" pass)
	(sleep 5))
      (connect-channels name target)
      
      (download-packs (list (list (connection-stream name) target  "list")) end))))

(defun dl-with-params (name target terms)
  (dolist (pack terms)
    (dcc-request (connection-stream name) target pack)
    (sleep 10)))





(defun dl-from-file (path pass)
  (if (null (probe-file path))
      (format out "~&File does not exists ~a" path)
      (let ((tmp-path (merge-pathnames "tmp" pack-dir))
	    (packs '()))
	(with-open-file (tmp tmp-path :direction :output :if-does-not-exist :create
			     :if-exists :supersede)
	  (with-open-file (ss path)
	    (do ((line (read-line ss nil nil)
		       (read-line ss nil nil))
		 (num 1 (1+ num))
		 (list-got nil))
		((null line))
	      (block try (let* ((words (find-words line))
				(server (nth 3 words))
				(target (nth 4 words))
				(con-name (get-connection-name-server server))
				(search-terms (subseq words 5)))
			   (unless list-got
			     (get-list server target pass)
			     (setf list-got t))
			   (if (correct-day words)
			       (progn
				 (when (not (connected-server server))
				   (unless (connect con-name server (generate-nick) "Anon" pass)
				     (format out "error parsing line ~d~%~a~%" num line)
				     (return-from try))
				   (sleep 5))
				 (connect-channels con-name target)
				 (unless (bot-name target path)
				   (get-list server target pass))
				 (let ((pack (parse-packlist search-terms packlist-path)))
				   
					;  (format out "~&Getting list | con ~a | xdcc bot ~a~%"
					;	  con-name target)
					; (get-list server target pass)
					;(setf pack (parse-packlist search-terms packlist-path)))
					;(format out "~&Parsing list~%")
				   (if pack
				       (dolist (x pack)
					 (push (list (connection-stream con-name)
						     target x) packs))
				       
					;(dl-with-params con-name target terms)
				       (write-line line tmp))))))))))
	(replace-file tmp-path schedu-path)
	(delete-file tmp-path)
	(setf packs (reverse packs))
	(download-packs packs t))))
