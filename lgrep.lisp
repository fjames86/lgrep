;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:lgrep
  (:use #:cl)
  (:export #:mapfile
	   #:mapgrep
	   #:grep
	   #:grep-if
	   #:grep-if-not 
	   #:grep*))

(in-package #:lgrep)

(defparameter *default-eol*
  #+(or win32 windows) (format nil "~C~C" #\return #\linefeed)
  #-(or win32 windows) (format nil "~C" #\linefeed))

(defun encoding-le (encoding)
  (case encoding
    (:ucs-2 :ucs2-le)
    (:utf-16 :utf-16le)
    (otherwise encoding)))
(defun encoding-be (encoding)
  (case encoding
    (:ucs-2 :ucs2-be)
    (:utf-16 :utf-16be)
    (otherwise encoding)))


(defun mapfile (func filespec &key encoding eol binaryp)
  "Map over each line of a given file. No return value.
FUNC ::= function taking two parameters, line lineindex.
FILESPEC ::= file specification.
EOL ::= end of line delimiter. defaults to #\return #\linefeed on windows, #\linefeed otherwise.
ENCODING ::= character encoding. defaults to utf-8.
BINARYP ::= don't decode as string, pass raw octets to func.
" 
  (with-open-file (stream filespec :direction :input :element-type '(unsigned-byte 8))
    (do ((buf (make-array 4096 :element-type '(unsigned-byte 8)))
	 (firstloop t)
	 (lineidx 0)
	 (start 0)
	 (end 0)
	 (eof nil)
	 (eol-bytes nil)
	 (done nil))
	(done)

      (flet ((refill-buffer ()
	       (let ((n (read-sequence buf stream :start start)))
		 (when (< n (length buf)) (setf eof t))
		 (setf end n)))
	     (get-eol-bytes ()
	       (if (typep eol '(vector (unsigned-byte 8)))
		   eol
		   (babel:string-to-octets (or eol *default-eol*)
					   :encoding encoding
					   :use-bom nil))))

	;; check for BOM on first loop, use to modify encoding 
	(when firstloop
	  (refill-buffer)
	  (cond
	    ((and (= (aref buf 0) 255) (= (aref buf 1) 254))	   
	     ;; LE
	     (setf encoding (encoding-le encoding)
		   eol-bytes (get-eol-bytes)))
	    ((and (= (aref buf 0) 254) (= (aref buf 1) 255))
	     ;; BE
	     (setf encoding (encoding-be encoding)
		   eol-bytes (get-eol-bytes))))
	  (setf firstloop nil
		eol-bytes (get-eol-bytes)))
	
	(let ((pos (search eol-bytes buf :start2 start :end2 end)))
	  (cond
	    (pos
	     ;; invoke callback 
	     (if binaryp
		 (funcall func (subseq buf start pos) lineidx)
		 (let ((str (restart-case (babel:octets-to-string buf :start start :end pos :encoding (or encoding :utf-8))
			      (ignore-file () (return-from mapfile nil))
			      (use-value (value)
				:report (lambda (stream) (format stream "Enter a value"))
				:interactive
				(lambda ()
				  (format *query-io* "Enter a value: ")
				  (finish-output *query-io*)
				  (list (read-line *query-io*)))
				value))))
		   (funcall func str lineidx)))
	     ;; update offsets 
	     (setf start (+ pos (length eol-bytes))
		   lineidx (1+ lineidx)))
	    (eof
	     ;; end of line not found and we have also hit the end of the file: we are done
	     (setf done t))
	    (t 
	     ;; eol not found but not end of file. memmove and refill buffer
	     (dotimes (i (- end start))
	       (setf (aref buf i) (aref buf (+ start i))))
	     (setf end start
		   start 0)
	     (refill-buffer))))))))

(defun mapgrep (func predicate pathspec &key recursivep encoding eol)
  "Map over each grepped line. 
FUNC ::= function that accepts arguments: path line lineidx 
PREDICATE ::= predicate function that takes the line 
PATHSPEC ::= starting path 
RECURSIVEP ::= if true, recurses into subdirectories
ENCODING ::= character encoding 
EOL ::= end of line delimiter 
" 
  (flet ((grepfile (path)
	   (handler-bind ((error (lambda (c)
				   (warn "Error decoding ~A: ~A" path c)
				   (invoke-restart 'ignore-file))))
	     (mapfile (lambda (line lineidx)
			(when (funcall predicate line)
			  (funcall func path line lineidx)))
		      path 
		      :encoding encoding
		      :eol eol))))
    (cond
      (recursivep
       (dolist (rpath (uiop:directory* pathspec))
	 (cond
	   ((uiop:directory-pathname-p rpath)
	    ;; grep each file
	    (dolist (fpath (uiop:directory-files rpath))
	      (grepfile fpath))
	    ;; recurse into subdirs 
	    (dolist (subdir (uiop:subdirectories rpath))
	      (mapgrep func predicate subdir
		       :recursivep recursivep
		       :encoding encoding
		       :eol eol)))
	   ((uiop:file-exists-p rpath)
	    (grepfile rpath))
	   (t (warn "Path ~S not a directory or file" rpath)
	      nil))))
      (t
       (grepfile pathspec)))))
  	 
(defun grep-if (predicate pathspec &key recursivep encoding eol)
  "Search a file by lines. 
PREDICATE ::= function taking a single parameter, line.
RECURSIVEP ::= if true, recurses into subdirectories
ENCODING ::= character encoding 
EOL ::= end of line delimiter 
"
  (let ((lines nil))
    (mapgrep (lambda (path line lineidx)
	       (let ((fentry (assoc path lines)))
		 (if fentry
		     (push (list lineidx line) (cdr fentry))
		     (push (list path (list lineidx line)) lines))))
	     predicate
	     pathspec
	     :recursivep recursivep
	     :encoding encoding
	     :eol eol)
    lines))

(defun grep-if-not (predicate pathspec &key recursivep encoding eol)
  (grep-if (complement predicate) pathspec
	   :recursivep recursivep
	   :encoding encoding
	   :eol eol))

(defun grep (pattern pathspec &key case-insensitive-p recursivep encoding eol inversep)
  "Walk each file searching for lines matching PATTERN. Returns a list of all the matched files and lines.
PATTERN ::= cl-ppcre regexp 
PATHSPEC ::= file/directory path specifier 
CASE-INSENSITIVE-P ::= if true, does a case insensitive match 
RECURSIVE-P ::= if true, recurses into subdirectories
ENCODING ::= character encoding 
EOL ::= end of line delimiter 
INVERSEP ::= if true, returns all lines that do not match the pattern
"
  (let ((regexp (cl-ppcre:create-scanner pattern :case-insensitive-mode case-insensitive-p)))
    (grep-if (lambda (line)
	       (let ((matchp (cl-ppcre:all-matches-as-strings regexp line)))
		 (if inversep
		     (not matchp)
		     matchp)))
	     pathspec
	     :recursivep recursivep
	     :encoding encoding
	     :eol eol)))
  
(defun grep* (pattern pathspec &key case-insensitive-p recursivep encoding eol inversep)
  "Walk each file searching for lines matching PATTERN, printing match to stdout.
PATTERN ::= cl-ppcre regexp 
PATHSPEC ::= file/directory path specifier 
CASE-INSENSITIVE-P ::= if true, does a case insensitive match 
RECURSIVE-P ::= if true, recurses into subdirectories
ENCODING ::= character encoding 
EOL ::= end of line delimiter 
INVERSEP ::= if true, returns all lines that do not match the pattern
"
  (let ((regexp (cl-ppcre:create-scanner pattern :case-insensitive-mode case-insensitive-p)))
    (mapgrep (lambda (path line lineidx)
	       (format t "~A:~A ~A~%" path lineidx line))
	     (lambda (line)
	       (let ((matchp (cl-ppcre:all-matches-as-strings regexp line)))
		 (if inversep
		     (not matchp)
		     matchp)))
	     pathspec
	     :recursivep recursivep
	     :encoding encoding
	     :eol eol))
  nil)

 
