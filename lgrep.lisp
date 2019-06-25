;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:lgrep
  (:use #:cl)
  (:export #:mapfile
	   #:mapgrep
	   #:grep-if
	   #:grep
	   #:grep*
	   #:find-files
	   #:diff
	   #:print-lines))

(in-package #:lgrep)


;;;
;;; This file defines various utilities for walking though files line by line.
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
			(babel:string-to-octets (format nil "~C" #\linefeed) :encoding :utf-8))))))
	     (invoke-callback (pos)
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
		     (funcall func str lineidx)))))
	
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
	     (invoke-callback pos)
	     ;; update offsets 
	     (setf file-offset (+ file-offset (- pos start) (length eol-bytes))
		   start (+ pos (length eol-bytes))
		   lineidx (1+ lineidx)))
	    (eof
	     ;; end of line not found and we have also hit the end of the file: we are done
	     (invoke-callback end)
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

(defun print-lines (filespec &key (start 0) end)
  "Print lines from a given file in range.
FILESPEC ::= input file.
START, END ::= region of file.
" 
  (mapfile (lambda (line lineidx)
	     (when (and (>= lineidx start)
			(or (null end) (< lineidx end)))
	       (format t "~6<~4,'0D~>|~A|~%" lineidx line)))
	   filespec))

;; ----------------------------------------------------------

;; The diffing algorithm is a Linear Space Myers Diff.
;; The codes below were translated from the Python program at blog.robertelder.org/diff-algorithm
;; I don't understand how it works but it does seem to be correct.

(defun subseq% (seq start &optional end)
  "Python semantics with modular indexing" 
  (cond
    ((and (integerp end) (= end 0))
     (vector))
    ((and (integerp end) (= start end))
     (vector))
    (t 
     (let ((len (length seq))
	   (end% (or end (length seq))))  
       (subseq seq (mod (+ start len) len)
	       (if (= end% len)
		   end%
		   (mod (+ end% len) len)))))))

(defun mod% (a b)
  "Pythons MOD always returns positive numbers." 
  (mod (+ a b) b))

(defun diff% (lines1 lines2 index1 index2 test)
  "Good luck understanding how this works. I don't.
LINES1 ::= array of old files lines.
LINES2 ::= array of new file lines.
INDEX1, INDEX2 ::= index into old/new lines
TEST ::= test functions 
Returns an edit script ::= edit*
EDIT ::= (INSERT old-index new-index), (DELETE old-index new-index)
"
  (let* ((nlines1 (length lines1))
	 (nlines2 (length lines2))
	 (sumlines (+ nlines1 nlines2))
	 (minlines (+ (* 2 (min nlines1 nlines2)) 2)))
    (cond
      ((and (> nlines1 0) (> nlines2 0))
       (let ((difflines (- nlines1 nlines2))
	     (g (make-array minlines :initial-element 0))
	     (p (make-array minlines :initial-element 0)))
	 (dotimes (h (+ 1 (truncate sumlines 2) (mod% sumlines 2)))
	   (do ((r 0 (1+ r)) 
		(c g p)
		(d p g))
	       ((= r 2))
	     (do ((k (- (- h (* 2 (max 0 (- h nlines2))))) (+ k 2)))
		 ((>= k (+ 1 h (* -2 (max 0 (- h nlines1))))))
	       (let* ((a (if (or (= k (- h))
				 (and (not (= k h))
				      (< (aref c (mod% (1- k) minlines)) (aref c (mod% (1+ k) minlines)))))
			     (aref c (mod% (1+ k) minlines))
			     (1+ (aref c (mod% (1- k) minlines)))))
		      (b (- a k))
		      (start-a a)
		      (start-b b))
		 ;; increment until lines don't match 
		 (do ()
		     ((not (and (< a nlines1)
				(< b nlines2)
				(funcall test 
					 (aref lines1 (+ (* r nlines1) (* (- 1 (* 2 r)) a) (- r)))
					 (aref lines2 (+ (* r nlines2) (* (- 1 (* 2 r)) b) (- r)))))))
		   (incf a)
		   (incf b))
		 (setf (aref c (mod% k minlines)) a)
		 (let ((zz (- difflines k))
		       (h-plus-r-minus-1 (1- (+ h r))))
		   (when (and (= (mod% sumlines 2) (- 1 r))
			      (>= zz (- h-plus-r-minus-1)) 
			      (<= zz h-plus-r-minus-1) 
			      (>= (+ (aref c (mod% k minlines)) (aref d (mod% zz minlines))) nlines1))
		     (let ((dd (if (zerop r) (1- (* 2 h)) (* 2 h)))
			   (x (if (zerop r) start-a (- nlines1 a)))
			   (y (if (zerop r) start-b (- nlines2 b)))
			   (u (if (zerop r) a (- nlines1 start-a)))
			   (v (if (zerop r) b (- nlines2 start-b))))
		       (cond
			 ((or (> dd 1) (and (not (= x u)) (not (= y v))))
			  (return-from diff%
			    (append (diff% (subseq% lines1 0 x) (subseq% lines2 0 y)
					   index1 index2 test)
				    (diff% (subseq% lines1 u nlines1) (subseq% lines2 v nlines2)
					   (+ index1 u) (+ index2 v) test))))
			 ((> nlines2 nlines1)
			  (return-from diff%
			    (diff% (vector) (subseq% lines2 nlines1 nlines2)
				   (+ index1 nlines1) (+ index2 nlines1) test)))
			 ((< nlines2 nlines1)
			  (return-from diff%
			    (diff% (subseq% lines1 nlines2 nlines1) (vector)
				   (+ index1 nlines2) (+ index2 nlines2) test)))
			 (t
			  (return-from diff% (list)))))))))))))
      ((> nlines1 0)
       (loop :for x :below nlines1 :collect (list 'delete (+ index1 x) index2)))
      (t
       (loop :for x :below nlines2 :collect (list 'insert index1 (+ index2 x)))))))

(defun get-file-lines (filespec &key encoding)
  (let ((lines nil))
    (mapfile (lambda (line lineidx)
	       (declare (ignore lineidx))
	       (push (list (sxhash line) line) lines))
	     filespec
	     :encoding encoding)
    (when (string= (second (car lines)) "")
      (setf lines (cdr lines)))
    (apply #'vector (nreverse lines))))


(defun timestamp-string (&optional when)
  (multiple-value-bind (s m h day month y) (decode-universal-time (or when (get-universal-time)))
    (format nil "~A-~A-~A ~A:~A:~A" y month day h m s)))

;; need to be able to merge edits into hunks, which are groups of changes.
;; hunks are bounded by a configurable number of context lines, which are unchanged lines on either
;; side of the edit sequence.
;; Computing the hunks with context=0 means getting all sequences of changes that are
;; adjacent in the original file. Computing hunks with context>0 allows there to be unchanged
;; lines between changes.

(defun get-hunks (script context max-lines1 max-lines2)
  (do ((edits script)
       (hunk nil)
       (old-idx 0)
       (new-idx 0)
       (hunks nil))
      ((null edits) (nreverse hunks))
    (let ((edit (car edits)))
      (destructuring-bind (cmd oidx nidx) edit
	(declare (ignore cmd))
	(flet ((complete-hunk ()
		 ;; add after context lines
		 (dotimes (i context)
		   (when (and (< old-idx max-lines1)
			      (< new-idx max-lines2))
		     (push (list 'null old-idx new-idx) (cdr hunk))
		     (incf old-idx)
		     (incf new-idx)))
		 
		 (setf (getf (car hunk) :old-end) (min old-idx (1- max-lines1))
		       (getf (car hunk) :new-end) (min new-idx (1- max-lines2))
		       (cdr hunk) (nreverse (cdr hunk)))
		 (push hunk hunks)
		 (setf hunk nil)))
	  
	  ;; if no current hunk, start a new one 
	  (unless hunk
	    (setf old-idx (max (- oidx context) 0)
		  new-idx (max (- nidx context) 0)
		  hunk (list (list :old-start old-idx :new-start new-idx)))
	    ;; add before context lines
	    (do ()
		((>= old-idx oidx))
	      (push (list 'null old-idx new-idx) (cdr hunk))
	      (incf old-idx)
	      (incf new-idx)))
	  
	  ;; check this edit is within context lines of the last line in the hunk
	  (cond
	    ((> oidx (+ old-idx context 1))
	     ;; this edit is the start of a new hunk. complete the old one first
	     (complete-hunk))
	    (t
	     ;; this edit continues the hunk, insert unchanged line commands
	     (incf old-idx)
	     (incf new-idx)
	     (do ()
		 ((>= old-idx oidx))
	       (push (list 'null old-idx new-idx) (cdr hunk))
	       (incf old-idx)
	       (incf new-idx))
	     ;; add this edit 
	     (push edit (cdr hunk))
	     (setf old-idx oidx
		   new-idx nidx
		   edits (cdr edits))
	     (when (null edits)
	       (complete-hunk)))))))))
  
(defun diff (filespec1 filespec2 &key (context 3) encoding)
  "Compute the difference between two files and print to *STANDARD-OUTPUT*.
FILESPEC1 ::= original file.
FILESPEC2 ::= new file.
CONTEXT ::= number of unchanged context lines to print before/after changes. 
ENCODING ::= file encoding.
"
  (let* ((lines1 (get-file-lines filespec1 :encoding encoding))
	 (lines2 (get-file-lines filespec2 :encoding encoding))
	 (hunks
	  (get-hunks (diff% lines1 lines2 0 0
			    (lambda (x y) (and (= (car x) (car y))
					       (string= (cadr x) (cadr y)))))
		     context
		     (length lines1)
		     (length lines2))))
    (format t "--- ~A~C~A~%" filespec1 #\tab (timestamp-string (file-write-date filespec1)))
    (format t "+++ ~A~C~A~%" filespec2 #\tab (timestamp-string (file-write-date filespec2)))
    (dolist (hunk hunks)
      (destructuring-bind ((&key (old-start 0) (new-start 0) (old-end 0) (new-end 0)) &rest edits) hunk
	(format t "@@ -~A,~A +~A,~A @@~%"
		(1+ old-start)
		(let ((n (- old-end old-start)))
		  (if (zerop n) 0 (1+ n)))
		(1+ new-start)
		(let ((n (- new-end new-start)))
		  (if (zerop n) 0 (1+ n))))
	(dolist (edit edits)
	  (destructuring-bind (cmd oldidx newidx) edit 
	    (ecase cmd
	      (insert
	       (format t "+~A~%" (second (aref lines2 newidx))))
	      (delete
	       (format t "-~A~%" (second (aref lines1 oldidx))))
	      (null
	       (format t " ~A~%" (second (aref lines1 oldidx))))))))))
  nil)


(defun get-diff-script (filespec1 filespec2 &key encoding)
  (let ((lines1 (get-file-lines filespec1 :encoding encoding))
	(lines2 (get-file-lines filespec2 :encoding encoding)))
    (mapcar (lambda (cmd)
	      (case (car cmd)
		(insert
		 (list :insert (third cmd) (second (aref lines2 (third cmd)))))
		(delete
		 (list :delete (second cmd)))))
	    (diff% lines1 lines2 0 0
		   (lambda (x y) (and (= (car x) (car y))
				      (string= (cadr x) (cadr y))))))))

(defun apply-diff-script (filespec script &key outfile (encoding :utf-8))
  (let ((lines (get-file-lines filespec :encoding encoding))
	(eol (babel:string-to-octets *default-eol*
				     :encoding encoding
				     :use-bom nil)))
    (with-open-file (stream (or outfile filespec)
			    :direction :output
			    :if-exists :supersede
			    :element-type '(unsigned-byte 8))
      (do ((oldidx 0)
	   (newidx 0)
	   (cmds script))
	  ((and (null cmds) (= oldidx (length lines))))
	(let ((cmd (car cmds)))
	  (cond
	    ((and (eq (car cmd) :delete)
		  (= (second cmd) oldidx))
	     ;; delete this line - i.e. ignore and move into nexxt cmd
	     (setf cmds (cdr cmds)
		   oldidx (1+ oldidx)))
	    ((and (eq (car cmd) :insert)
		  (= (second cmd) newidx))
	     ;; insert this line
	     (write-sequence (babel:string-to-octets (third cmd)
						     :encoding encoding
						     :use-bom nil)
			     stream)
	     (write-sequence eol stream)
	     (setf cmds (cdr cmds)
		   newidx (1+ newidx)))
	    (t ;; copy this line
	     (write-sequence (babel:string-to-octets (second (aref lines oldidx))
						     :encoding encoding
						     :use-bom nil)
			     stream)
	     (write-sequence eol stream)
	     (setf oldidx (1+ oldidx)
		   newidx (1+ newidx))))))))
  nil)
