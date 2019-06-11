;;;; Copyright (c) Frank James 2019 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :lgrep
  :name "lgrep"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Lisp grep utility"
  :license "MIT"
  :serial t
  :components
  ((:file "lgrep"))
  :depends-on (:babel :uiop :cl-ppcre))


