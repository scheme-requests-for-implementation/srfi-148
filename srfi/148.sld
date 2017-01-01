;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi 148)
  (export ck-macro-transformer

	  ;; General
	  ck-expression
	  ck-cut
	  ck-cute
	  ck-constant
	  ck-quote
	  ck-eval
	  ck-apply
	  ck-call
	  ck-error
	  ck-gensym
	  ck-generate-temporaries

	  ;; Boolean logic
	  ck-if
	  ck-not
	  ck-or
	  ck-and
	  ck-null?
	  ck-pair?
	  ck-list?
	  ck-boolean?
	  ck-vector?
	  ck-symbol?
	  ck-bound-identifier=?
	  ck-free-identifier=?
	  ck-equal?
	  
	  ;; Constructors
	  ck-cons
	  ck-cons*
	  ck-list
	  ck-make-list
	  
	  ;; Selectors
	  ck-car
	  ck-cdr
	  ck-caar
	  ck-cadr
	  ck-cdar
	  ck-cddr
	  ck-first
	  ck-second
	  ck-third
	  ck-fourth
	  ck-fifth
	  ck-sixth
	  ck-seventh
	  ck-eighth
	  ck-ninth
	  ck-tenth
	  ck-list-tail
	  ck-list-ref
	  ck-take
	  ck-drop
	  ck-take-right
	  ck-drop-right
	  ck-last
	  ck-last-pair

	  ;; Miscellaneous
	  ck-append
	  ck-reverse

	  ;; Folding, unfolding, and mapping
	  ck-fold
	  ck-fold-right
	  ck-unfold
	  ck-unfold-right
	  ck-map
	  ck-append-map
	  
	  ;; Filtering
	  ck-filter
	  ck-remove
	  
	  ;; Searching
	  ck-find
	  ck-find-tail
	  ck-take-while
	  ck-drop-while
	  ck-any
	  ck-every
	  ck-member
	  
	  ;; Association lists
	  ck-assoc
	  ck-alist-delete

	  ;; Set operationse
	  ck-set<=
	  ck-set=
	  ck-set-adjoin
	  ck-set-union
	  ck-set-intersection
	  ck-set-difference
	  ck-set-xor

	  ;; Vector processing
	  ck-vector
	  ck-list->vector
	  ck-vector->list
	  ck-vector-map
	  ck-vector-ref
	  
	  ;; Combinatorics
	  ck-0
	  ck-1
	  ck-2
	  ck-3
	  ck-4
	  ck-5
	  ck-6
	  ck-7
	  ck-8
	  ck-9
	  ck-10
	  ck-=
	  ck-<
	  ck-<=
	  ck->
	  ck->=
	  ck-zero?
	  ck-even?
	  ck-odd?
	  ck-+
	  ck--
	  ck-*
	  ck-quotient
	  ck-remainder
	  ck-fact
	  ck-binom
	  
	  ;; Auxiliary syntax
	  =>
	  <>
	  ...)
  (include-library-declarations "../custom-macro-transformers.scm")
  (import (srfi 26))
  (include "148.scm"
	   "148.macros.scm"))

;; Local Variables:
;; eval: (put 'ck-macro-transformer 'scheme-indent-function 'defun)
;; eval: (font-lock-add-keywords 'scheme-mode
;;                               '(("(\\(ck-macro-transformer\\)\\>" 1 font-lock-keyword-face)))
;; End:
