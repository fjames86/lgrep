;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:lgrep
  (:use #:cl)
  (:export #:mapfile
	   #:mapgrep
	   #:grep-if
	   #:grep
	   #:grep*
	   #:find-files))

(in-package #:lgrep)


;;;
;;; This file defines utilities for walking though files line by line.
;;; mapfile: walks a file line by line 
;;; mapgrep: walks a set of files, searching for lines which match a given predicate
;;; grep: walks a set of files, searching for lines which match a given pattern 
;;; 



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

(defun guess-encoding (buf encoding)
  (cond
    ((and (= (aref buf 0) 255) (= (aref buf 1) 254))	   
     ;; LE
     (setf encoding (if encoding (encoding-le encoding) :utf-16le)))
    ((and (= (aref buf 0) 254) (= (aref buf 1) 255))
     ;; BE
     (setf encoding (if encoding (encoding-be encoding) :utf-16be)))
    ((and (= (aref buf 0) 0) (= (aref buf 2) 0))
     ;; guess LE
     (setf encoding (if encoding (encoding-le encoding) :utf-16le)))
    ((and (= (aref buf 1) 0) (= (aref buf 3) 0))
     ;; guess BE
     (setf encoding (if encoding (encoding-be encoding) :utf-16be)))
    ((and (= (aref buf 0) #xef) (= (aref buf 1) #xbb) (= (aref buf 2) #xbf))
     ;; UTF8
     (setf encoding :utf-8))
    (t
     ;; Neither, guess utf-8
     (setf encoding (or encoding :utf-8))))
  encoding)

(defun mapfile (func filespec &key encoding eol binaryp)
  "Map over each line of a given file. No return value.
FUNC ::= function taking two parameters, line lineindex.
FILESPEC ::= file specification.
EOL ::= end of line delimiter. defaults to #\return #\linefeed on windows, #\linefeed otherwise.
ENCODING ::= character encoding. defaults to utf-8.
BINARYP ::= don't decode as string, pass raw octets to func.
"
  (with-open-file (stream filespec :direction :input :element-type '(unsigned-byte 8))
    (do ((buf (make-array (* 32 1024) :element-type '(unsigned-byte 8)))
	 (firstloop t)
	 (lineidx 0)
	 (start 0)
	 (end 0)
	 (eof nil)
	 (eol-bytes nil)
	 (file-offset 0)
	 (done nil))
	(done)

      (flet ((refill-buffer ()
	       (let ((n (read-sequence buf stream :start end)))
		 (when (< n (length buf)) (setf eof t))
		 (setf end n)))
	     (get-eol-bytes ()
	       (cond
		 ((typep eol '(vector (unsigned-byte 8)))
		  eol)
		 (t
		  (let ((default-eol 
			 (babel:string-to-octets (or eol *default-eol*)
						 :encoding encoding
						 :use-bom nil)))
		    (if (search default-eol buf)
			default-eol
			(babel:string-to-octets (format nil "~C" #\linefeed) :encoding :utf-8)))))))

	;; on first loop, check for BOM to detect encoding and derive eol bytes 
	(when firstloop
	  (refill-buffer)
	  (setf encoding (guess-encoding buf encoding)
		firstloop nil
		eol-bytes (get-eol-bytes)))
	
	(let ((pos (search eol-bytes buf :start2 start :end2 end)))
	  (cond
	    (pos
	     ;; invoke callback 
	     (if binaryp
		 (funcall func (subseq buf start pos) lineidx)
		 (let ((str (restart-case (babel:octets-to-string buf :start start :end pos :encoding (or encoding :utf-8))
			      (use-iso-8859 () (babel:octets-to-string buf :start start :end pos :encoding :iso-8859-1))
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
	     (setf file-offset (+ file-offset (- pos start) (length eol-bytes))
		   start (+ pos (length eol-bytes))
		   lineidx (1+ lineidx)))
	    (eof
	     ;; end of line not found and we have also hit the end of the file: we are done
	     (setf done t))
	    (t 
	     ;; eol not found but not end of file. memmove and refill buffer
	     (when (= start 0) (error "Line larger than buffer"))
	     
	     (dotimes (i (- end start))
	       (setf (aref buf i) (aref buf (+ start i))))
	     (setf end (- end start)
		   start 0)
	     (refill-buffer))))))))

(defun mapgrep (func predicate pathspec &key recursivep encoding eol exclude-files)
  "Map over each grepped line. 
FUNC ::= function that accepts arguments: path line lineidx 
PREDICATE ::= predicate function that takes the line 
PATHSPEC ::= starting path 
RECURSIVEP ::= if true, recurses into subdirectories
ENCODING ::= character encoding 
EOL ::= end of line delimiter 
" 
  (flet ((grepfile (path)
	   (unless (some (lambda (exclude-file)
			   (string-equal (pathname-type path) exclude-file))
			 exclude-files)
	     (handler-bind ((error (lambda (c)				   
				     (warn "Error decoding ~A: ~A" path c)
				     (invoke-restart 'ignore-file))))
	       (restart-case 
		   (mapfile (lambda (line lineidx)
			      (when (funcall predicate line)
				(funcall func path line lineidx)))
			    path 
			    :encoding encoding
			    :eol eol)
		 (ignore-file () (return-from grepfile nil)))))))
    (cond
      ((or recursivep (uiop:directory-pathname-p (uiop:truename* pathspec)))
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
		       :eol eol
		       :exclude-files exclude-files)))
	   ((uiop:file-exists-p rpath)
	    (grepfile rpath))
	   (t (warn "Path ~S not a directory or file" rpath)
	      nil))))
      ((uiop:file-exists-p pathspec)
       (grepfile pathspec)))))
  	 
(defun grep-if (predicate pathspec &key recursivep encoding eol exclude-files)
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
	     :eol eol
	     :exclude-files exclude-files)
    lines))

(defun grep (pattern pathspec &key case-insensitive-p recursivep encoding eol inversep exclude-files)
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
	     :eol eol
	     :exclude-files exclude-files)))
  
(defun grep* (pattern pathspec &key case-insensitive-p recursivep encoding eol inversep exclude-files)
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
    (handler-bind ((warning (lambda (c) (muffle-warning c))))
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
	       :eol eol
	       :exclude-files exclude-files)))
  nil)

(defun find-files (pathspec &key pattern extension name case-insensitive-p)
  "Look for files matching a given extension, name or pattern.
PATHSPEC ::= root directory.
PATTERN ::= if specified, matches against this regexp.
EXTENSION ::= if specified, matches against this file extension.
NAME ::= if specified, matche against this file name.
CASE-INSENSITIVE-P ::= if specified, case insensitive.
"
  (flet ((stringcomp (s1 s2)
	   (if case-insensitive-p
	       (string-equal s1 s2)
	       (string= s1 s2))))
    (let ((matches nil)
	  (regexp (when pattern 
		    (cl-ppcre:create-scanner pattern
					     :case-insensitive-mode case-insensitive-p))))
      (labels ((grepfile (path)
		 (when (and (or (null pattern) (cl-ppcre:all-matches-as-strings regexp (format nil "~A" path)))
			    (or (null extension) (stringcomp (pathname-type path) extension))
			    (or (null name) (stringcomp (pathname-name path) name)))
		   (push path matches)))
	       (%find-files (pathspec)
		 (dolist (rpath (uiop:directory* pathspec))
		   (cond
		     ((uiop:directory-pathname-p rpath)
		      ;; grep each file
		      (dolist (fpath (uiop:directory-files rpath))
			(grepfile fpath))
		      ;; recurse into subdirs 
		      (dolist (subdir (uiop:subdirectories rpath))
			(%find-files subdir)))
		     ((uiop:file-exists-p rpath)
		      (grepfile rpath))
		     (t (warn "Path ~S not a directory or file" rpath)
			nil)))))
	(%find-files pathspec))
      matches)))

