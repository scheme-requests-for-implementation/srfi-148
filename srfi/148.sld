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
  (export em-syntax-rules

	  ;; General
	  em
	  em-cut
	  em-cute
	  em-constant
	  em-quote
	  em-eval
	  em-apply
	  em-call
	  em-error
	  em-gensym
	  em-generate-temporaries

	  ;; Boolean logic
	  em-if
	  em-not
	  em-or
	  em-and
	  em-null?
	  em-pair?
	  em-list?
	  em-boolean?
	  em-vector?
	  em-symbol?
	  em-bound-identifier=?
	  em-free-identifier=?
	  em-equal?
	  
	  ;; Constructors
	  em-cons
	  em-cons*
	  em-list
	  em-make-list
	  
	  ;; Selectors
	  em-car
	  em-cdr
	  em-caar
	  em-cadr
	  em-cdar
	  em-cddr
	  em-first
	  em-second
	  em-third
	  em-fourth
	  em-fifth
	  em-sixth
	  em-seventh
	  em-eighth
	  em-ninth
	  em-tenth
	  em-list-tail
	  em-list-ref
	  em-take
	  em-drop
	  em-take-right
	  em-drop-right
	  em-last
	  em-last-pair

	  ;; Miscellaneous
	  em-append
	  em-reverse

	  ;; Folding, unfolding, and mapping
	  em-fold
	  em-fold-right
	  em-unfold
	  em-unfold-right
	  em-map
	  em-append-map
	  
	  ;; Filtering
	  em-filter
	  em-remove
	  
	  ;; Searching
	  em-find
	  em-find-tail
	  em-take-while
	  em-drop-while
	  em-any
	  em-every
	  em-member
	  
	  ;; Association lists
	  em-assoc
	  em-alist-delete

	  ;; Set operationse
	  em-set<=
	  em-set=
	  em-set-adjoin
	  em-set-union
	  em-set-intersection
	  em-set-difference
	  em-set-xor

	  ;; Vector processing
	  em-vector
	  em-list->vector
	  em-vector->list
	  em-vector-map
	  em-vector-ref
	  
	  ;; Combinatorics
	  em-0
	  em-1
	  em-2
	  em-3
	  em-4
	  em-5
	  em-6
	  em-7
	  em-8
	  em-9
	  em-10
	  em=
	  em<
	  em<=
	  em>
	  em>=
	  em-zero?
	  em-even?
	  em-odd?
	  em+
	  em-
	  em*
	  em-quotient
	  em-remainder
	  em-fact
	  em-binom
	  
	  ;; Auxiliary syntax
	  =>
	  <>
	  ...)
  (include-library-declarations "../custom-macro-transformers.scm")
  (import (srfi 26))
  (cond-expand
    (chibi
     (import (scheme cxr))
     (import (srfi 147 er-macro-transformer))
     (include "148.er-macro-transformer.scm"))
    (else
     (include "148.identifier.scm")))
  (include "148.scm"
	   "148.macros.scm"))

;; Local Variables:
;; eval: (put 'em-syntax-rules 'scheme-indent-function 'defun)
;; eval: (font-lock-add-keywords 'scheme-mode
;;                               '(("(\\(em-syntax-rules\\)\\>" 1 font-lock-keyword-face)))
;; End:
