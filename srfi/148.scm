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

;; Secret syntactic literals
(define-syntax :call (syntax-rules ()))
(define-syntax :prepare (syntax-rules ()))

(define-syntax ck
  (syntax-rules (quote quasiquote ck-cut ck-cute)
    ((ck () 'v)
     v)
    ((ck (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))
    ((ck s "arg" (quasiquote va))
     (ck-quasiquote-aux :call s va '()))
    ((ck s "arg" (op va ...))
     (op :call s va ...))
    ((ck s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))
    ((ck s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))
    ((ck s (quasiquote ea))
     (ck-quasiquote-aux :prepare s ea '()))
    ((ck s ((ck-cut a1 ...) a2 ...))
    (ck-cut-aux s () (a1 ...) (a2 ...)))
    ((ck s ((ck-cute a1 ...) a2 ...))
     (ck-cut-aux s () (a1 ...) (a2 ...)))
    ((ck s ((op a ...) ea ...))
     (ck-apply :prepare s (op a ...) ea ... '()))
    ((ck s (op ea ...))
     (op :prepare s ea ... ))
    ((ck s v)
     (ck s 'v))))

(define-syntax free-identifier=?
  (syntax-rules ()
    ((free-identifier=? id1 id2 kt kf)
     (begin
       (define-syntax m
	 (syntax-rules :::1 ()
	   ((m %kt %kf)
	    (begin
	      (define-syntax test
		(syntax-rules :::2 (id1)
		  ((test id1 %%kt %%kf) %%kt)
		  ((test x %%kt %%kf) %%kf)))
	      (test id2 %kt %kf)))))
       (m kt kf)))))

(define-syntax bound-identifier=?
  (syntax-rules ()
    ((bound-identifier=? id v kt kf)
     (begin
       (define-syntax m
	 (syntax-rules :::1 ()				       
	   ((m %kt %kf)
	    (begin
	      (define-syntax id
		(syntax-rules :::2 ()
		  ((id %%kt %%kf) %%kf)))
	      (define-syntax ok
		(syntax-rules ()
		  ((ok %%kt %%kf) %%kt)))
	      (define-syntax test
		(syntax-rules :::2 ()
		  ((test v %%kt %%kf) (id %%kt %%kf))
   	          ((test _ %%kt %%kf) (id %%kt %%kf))))
	      (test ok %kt %kf)))))
       (m kt kf)))))

(define-syntax ck-macro-transformer
  (syntax-rules ()
    ((ck-macro-transformer (literal ...)
       (pattern (element => var) ... template)
       ...)
     (ck-macro-transformer-aux1 free-identifier=? () (... ...)
				(literal ...) ((pattern (element => var) ... template) ...)))
    ((ck-macro-transformer ellipsis (literal ...)
       (pattern (element => var) ... template)
       ...)
     (ck-macro-transformer-aux1 bound-identifier=? (ellipsis) ellipsis
				(literal ...) ((pattern (element => var) ... template) ...)))
    ((ck-macro-transformer . _)
     (syntax-error "invalid ck-macro-transformer syntax"))))

(define-syntax ck-macro-transformer-aux1
  (syntax-rules (=>)
    ((ck-macro-transformer-aux1 c e* e l* ((p t) ...))
     (ck-macro-transformer-aux2 c e* e l* ((p t) ...)))
    
    ((ck-macro-transformer-aux1 c e* e l* r*)
     (ck-macro-transformer-aux1 a c e* e l* r* () ()))

    ((ck-macro-transformer-aux1 a c e* e l* ((p t) . r*) (r1 ...) r2*)
     (ck-macro-transformer-aux1 a c e* e l* r* (r1 ... (p t)) r2*))

    ((ck-macro-transformer-aux1 a c e* e l* (((_ p ...) (i => v) w ... t) . r*) (r1 ...) (r2 ...))
     (ck-macro-transformer-aux1 a c e* e l* r*
				(r1 ... ((_ p ...) (a i '(p ...))))
				(r2 ... ((_ v '(p ...)) w ... t))))

    ((ck-macro-transformer-aux1 a c e* e l* () r1* r2*)
     (begin (define-syntax a
	      (ck-macro-transformer-aux1 c e* e l* r2*))
	    (ck-macro-transformer-aux2 c e* e l* r1*)))))

(define-syntax ck-macro-transformer-aux2
  (syntax-rules (quote)
    ((ck-macro-transformer-aux2 c e* e (l ...) ((p t) ...))
     (begin (define-syntax o
	      (ck-macro-transformer-aux2 o c e* e (l ...) ((p t) ...) ()))
	    o))
    ((ck-macro-transformer-aux2 o c (e? ...) e (l ...) () ((p q r t) ...))
     (syntax-rules e? ... (l ... quote :prepare :call)
		   ((_ :prepare s . p)
		    (ck s "arg" (o) . q))
		   ...
		   ((_ :prepare s . args)
		    (syntax-error "bad arguments to macro call" . args))
		   ((_ :call s . r) (ck s t))
		   ...
		   ((_ . args) (ck () (o . args)))))
    
    ((ck-macro-transformer-aux2 o c e* e l* (((op . p) t) . pt*) qu*)
     (ck-macro-transformer-aux2 o c e* e l* pt* qu* (p t) () () ()))
    
    ((ck-macro-transformer-aux2 o c e* e l* pt* (qu ...) (() t) p q r)
     (ck-macro-transformer-aux2 o c e* e l* pt* (qu ... (p q r t))))
    
    ((ck-macro-transformer-aux2 o c e* e l* pt* qu* (('x maybe-ellipsis . p) t)
			       (y ...) (z ...) (w ...))
     (c e maybe-ellipsis
	(ck-macro-transformer-aux2 o c e* e l* pt* qu* (p t) (y ... %x e) (z ... %x e)
				  (w ... 'x e))
	(ck-macro-transformer-aux2  o c e* e l* pt* qu* ((maybe-ellipsis . p) t) (y ... %x) (z ... %x)
				  (w ... 'x))))
    
    ((ck-macro-transformer-aux2 o c e* e l* pt* qu* (('x . p) t) (y ...) (z ...) (w ...))
     (ck-macro-transformer-aux2 o c e* e l* pt* qu* (p t) (y ... %x) (z ... %x) (w ... 'x)))
    
    ((ck-macro-transformer-aux2 o c e* e l* pt* qu* ((x maybe-ellipsis . p) t)
			       (y ...) (z ...) (w ...))
     (c e maybe-ellipsis
	(ck-macro-transformer-aux2 o c e* e l* pt* qu* (p t) (y ... %x e) (z ... '%x e)
				  (w ... 'x e))
	(ck-macro-transformer-aux2 o c e* e l* pt* qu* ((maybe-ellipsis . p) t)
				  (y ... %x) (z ... '%x) (w ... 'x))))

    ((ck-macro-transformer-aux2 o c e* e l* pt* qu* ((x . p) t) (y ...) (z ...) (w ...))
     (ck-macro-transformer-aux2 o c e* e l* pt* qu* (p t) (y ... %x) (z ... '%x) (w ... 'x)))))

(define-syntax ck-expression
  (syntax-rules (quote :prepare :call)
    ((ck-expression :prepare s expression)
     (ck s "arg" (ck-expression) 'expression))
    ((ck-expression :call s 'expression)
     (let ()
       (ck-quasiquote (let () (define x ,expression) x))))
    ((ck-expression expression)
     (ck () (ck-expression expression)))))

(define-syntax ck-suspend
  (syntax-rules (quote :prepare :call)
    ((ck-suspend :prepare s op arg ...)
     (ck s "arg" (ck-suspend) op arg ...))
    ((ck-suspend :call s 'op 'arg ...)
     (op s arg ...))))

(define-syntax ck-resume
  (syntax-rules ()
    ((ck-resume t v)
     (ck t v))))

(define-syntax ck-cut
  (ck-macro-transformer ()
    ((ck-cut slot-or-datum ...)
     `(ck-cut ,(ck-cut-eval slot-or-datum) ...))))

(define-syntax ck-cute
  (ck-macro-transformer ()
    ((ck-cute slot-or-datum ...)
     `(ck-cut ,(ck-cute-eval slot-or-datum) ...))))

(define-syntax ck-cut-eval
  (ck-macro-transformer ::: (<> ...)
    ((ck-cut-eval <>) <>)
    ((ck-cut-eval ...) ...)
    ((ck-cut-eval x) 'x)))

(define-syntax ck-cut-aux
  (syntax-rules ::: (<> ...)

    ((ck-cut-aux s (datum :::) () ())
     (ck-apply :prepare s datum ::: '()))

    ((ck-cut-aux s (datum :::) (<> ...) (input :::))
     (ck-cut-aux s (datum ::: input :::) () ()))

    ((ck-cut-aux s (datum :::) (<> slot-or-datum :::) (input1 input2 :::))
     (ck-cut-aux s (datum ::: input1) (slot-or-datum :::) (input2 :::)))

    ((ck-cut-aux s (datum1 :::) (datum2 slot-or-datum :::) (input :::))
     (ck-cut-aux s (datum1 ::: datum2) (slot-or-datum :::) (input :::)))))

(define-syntax ck-cute-eval
  (ck-macro-transformer ::: (<> ...)
    ((ck-cute-eval '<>) <>)
    ((ck-cute-eval '...) ...)
    ((ck-cute-eval 'x) 'x)))

(define-syntax ck-quasiquote
  (ck-macro-transformer ()
    ((ck-quasiquote form) (ck-quasiquote-aux form '()))))

(define-syntax ck-quasiquote-aux
  (ck-macro-transformer (quasiquote unquote unquote-splicing)
    ((ck-quasiquote-aux ,form '())
     form) 
    ((ck-quasiquote-aux (,@form . rest) '())
     (ck-append form (ck-quasiquote-aux rest '())))
    ((ck-quasiquote-aux `form 'depth)
     (ck-list 'quasiquote (ck-quasiquote-aux form '(#f . depth))))
    ((ck-quasiquote-aux ,form '(#f . depth))
     (ck-list 'unquote (ck-quasiquote-aux form 'depth)))
    ((ck-quasiquote-aux ,@form '(#f . depth))
     (ck-list 'unquote-splicing (ck-quasiquote-aux '(form . depth))))
    ((ck-quasiquote-aux (car . cdr) 'depth)
     (ck-cons (ck-quasiquote-aux car 'depth) (ck-quasiquote-aux cdr 'depth)))
    ((ck-quasiquote-aux #(element ...) 'depth)
     (ck-list->vector (ck-quasiquote-aux (element ...) 'depth)))
    ((ck-quasiquote-aux constant 'depth)
     'constant)))
