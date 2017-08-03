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

(define-library (srfi 147 implementation)
  (export define-syntax
	  let-syntax
	  letrec-syntax
	  syntax-rules)
  (import (rename (except (scheme base) let-syntax letrec-syntax)
		  (syntax-rules scheme-syntax-rules)
		  (define-syntax scheme-define-syntax)))
  (include "implementation.scm")
  (cond-expand

    (chibi
     (export er-macro-transformer)
     (import (rename (only (chibi) er-macro-transformer display)
		     (er-macro-transformer scheme-er-macro-transformer))
	     (scheme cxr))
     (import (prefix (only (scheme base) let-syntax letrec-syntax) scheme-))
     (include "er-macro-transformer.scm"))

    ;; Larceny exports let-syntax and letrec-syntax with R6RS semantcs,
    ;; which is incompatible to the R7RS semantics.
    (larceny
     (import (rename (only (scheme base) let-syntax letrec-syntax)
		     (let-syntax let-syntax/splicing)
		     (letrec-syntax letrec-syntax/splicing)))
     (begin
       (scheme-define-syntax scheme-let-syntax
	 (scheme-syntax-rules ()
	   ((scheme-let-syntax bindings . body)
	    (let () (let-syntax/splicing bindings . body)))))
       
       (scheme-define-syntax scheme-letrec-syntax
	 (scheme-syntax-rules ()
	   ((scheme-letrec-syntax bindings . body)
	    (let () (letrec-syntax/splicing bindings . body)))))))

    (else
     (import (rename (scheme base) (syntax-rules scheme-er-macro-transformer)))
     (import (prefix (only (scheme base) let-syntax letrec-syntax) scheme-)))))
