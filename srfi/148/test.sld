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

(define-library (srfi 148 test)
  (export run-tests)
  (include-library-declarations "../../custom-macro-transformers.scm")
  (import (srfi 2)
          (srfi 64)
	  (srfi 148))
  (begin
    (define (run-tests)
      (test-begin "SRFI 148")

      (test-equal "Match quoted pattern with quoted element"
	10 
	(let-syntax
	    ((m
	      (ck-macro-transformer ()
		((m 'a) 'a))))
	  (m '(define x 10))
	  x))

      (test-equal "Match quoted pattern with ck-macro use"
	10
	(letrec-syntax
	    ((m1
	      (ck-macro-transformer ()
		((m1 'define 'x) '(define x))))
	     (m2
	      (ck-macro-transformer ()
		((m2 '(d y)) '(d y 10)))))
	  (m2 (m1 'define 'x))
	  x))

      (test-equal "Match unquoted pattern with element"
	10
	(letrec-syntax
	    ((m
	      (ck-macro-transformer ()
		((m a) 'a))))
	  (m (define x 10))
	  x))

      (test-equal "CK-macro uses in expression contexts"
	10
	(letrec-syntax
	    ((m
	      (ck-macro-transformer ()
		((m 'a 'b) '(+ a b)))))
	  (ck-expression (m '5 '5))))

      (test-equal "Self-quoting literals"
	10
	(letrec-syntax
	    ((m
	      (ck-macro-transformer ()
		((m 'a) 10))))
	  (m 1)))

      (test-equal "Quasiquotation in output element"
	10
	(letrec-syntax
	    ((m1
	      (ck-macro-transformer ()
		((m1 'a) '(+ a 3))))
	     (m2
	      (ck-macro-transformer ()
		((m2 'a 'b) `(+ ,(m1 'a) ,(m1 'b))))))
	  (ck-expression (m2 '1 '3))))

      (test-equal "Quasiquotation in input element"
	10
	(letrec-syntax
	    ((m1
	      (ck-macro-transformer ()
		((m1 'a) '(+ a 3))))
	     (m2
	      (ck-macro-transformer ()
		((m2 'a) 'a))))
	  (ck-expression (m2 `(+ 5 ,(m1 '2))))))

      (test-equal "Quasiquotation: vector"
	#((a . 1) 2 3)
	(ck-expression (ck-quote `#(,(ck-cons 'a '1) ,@(ck-list 2 3)))))

      (test-equal "Pattern binding"
	'(foo (foo))
	(letrec-syntax
	    ((m
	      (ck-macro-transformer ()
		((m 'a)
		 ((ck-make-list (ck-2) 'a) => '(b c))
		 ((ck-list 'c) => 'd)
		 '(a d)))))
	  (ck-expression (ck-quote (m 'foo)))))

      (test-equal "Pattern binding with ellipsis"
	'(foo bar (foo))
	(letrec-syntax
	    ((m
	      (ck-macro-transformer ()
		((m 'a 'x ...)
		 ((ck-make-list (ck-2) 'a) => '(b c))
		 ((ck-list 'c) => 'd)
		 '(a x ... d)))))
	  (ck-expression (ck-quote (m 'foo 'bar)))))

      (test-group "General"

	(test-equal "ck-cut: without ..."
	  '(a . <>)
	  (ck-expression (ck-quote (ck-apply (ck-cut 'ck-cons <> '<>) 'a '()))))

	(test-equal "ck-cut: with ..."
	  '(a b c d)
	  (ck-expression (ck-quote (ck-apply (ck-cut 'ck-list <> 'b <> ...) 'a 'c 'd '()))))

	(test-equal "ck-cut: in operator position"
	  '(a . <>)
	  (ck-expression (ck-quote ((ck-cut 'ck-cons <> '<>) 'a))))

	(test-equal "ck-cute: without ..."
	  '(a . <>)
	  (ck-expression (ck-quote (ck-apply (ck-cute 'ck-cons <> '<>) 'a '()))))

	(test-equal "ck-cute: with ..."
	  '(a b c d)
	  (ck-expression (ck-quote (ck-apply (ck-cute 'ck-list <> 'b <> ...) 'a 'c 'd '()))))

	(test-equal "ck-cute: in operator position"
	  '(a . <>)
	  (ck-expression (ck-quote ((ck-cute 'ck-cons <> '<>) 'a))))

	(test-equal "ck-constant"
	  'foo
	  (ck-expression (ck-quote ((ck-constant 'foo) 'a 'b 'c))))

	(test-equal "ck-quote"
	  '(1 2 3)
	  (ck-expression (ck-quote (ck-list 1 2 3))))

	(test-equal "ck-eval"
	  '(1 2 3)
	  (ck-expression (ck-eval '(ck-list 'list 1 2 3))))
	
	(test-equal "ck-apply"
	  '(1 2 3 4 5 6)	
	  (ck-expression (ck-apply 'ck-list 'list 1 2 3 '(4 5 6))))
	
	(test-equal "ck-call"
	  '(1 2 3)
	  (ck-expression (ck-quote (ck-call 'ck-list '1 '2 '3))))

	(test-assert "ck-gensym"
	  (ck-expression (ck-not (ck-bound-identifier=? (ck-gensym) (ck-gensym)))))

	(test-assert "ck-generate-temporaries"
	  (ck-expression (ck-not (ck-apply 'ck-bound-identifier=? (ck-generate-temporaries (ck-2)))))))
      
      (test-group "Boolean logic"
	
	(test-equal "ck-if: true test"
	  'true
	  (ck-expression (ck-quote (ck-if '#t 'true 'false))))

	(test-equal "ck-if: false test"
	  'false
	  (ck-expression (ck-quote (ck-if '#f 'true 'false))))

	(test-equal "ck-not"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-not '#t) 'true 'false))))

	(test-equal "ck-or"
	  'a
	  (ck-expression (ck-or '#f ''a (ck-error "fail"))))

	(test-equal "ck-and"
	  '#f
	  (ck-expression (ck-and ''a ''b '#f (ck-error "fail"))))

	(test-equal "ck-null?: true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-null? '()) 'true 'false))))

	(test-equal "ck-null?: false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-null? 'x) 'true 'false))))

	(test-equal "ck-pair?: true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-pair? '(a . b)) 'true 'false))))

	(test-equal "ck-pair?: false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-pair? '()) 'true 'false))))

	(test-equal "ck-list?: true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-list? '(a b)) 'true 'false))))

	(test-equal "ck-list?: false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-list? '(a . b)) 'true 'false))))
	
	(test-equal "ck-boolean?: true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-boolean? '#f) 'true 'false))))

	(test-equal "ck-boolean?: false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-boolean? '(a . b)) 'true 'false))))
	
	(test-equal "ck-vector?: true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-vector? '#(a b)) 'true 'false))))
	
	(test-equal "ck-vector?: false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-vector? '(a . b)) 'true 'false))))

	(test-equal "ck-symbol?: true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-symbol? 'a) 'true 'false))))

	(test-equal "ck-symbol?: false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-symbol? '(a . b)) 'true 'false))))

	(test-equal "ck-bound-identifier=?: true"
	  'true
	  (letrec-syntax
	      ((m
		(syntax-rules ()
		  ((m a b)
		   (ck-expression (ck-quote (ck-if (ck-bound-identifier=? 'a 'b) 'true 'false)))))))
	    (m c c)))

	(test-equal "ck-bound-identifier=?: false"
	  'false
	  (letrec-syntax
	      ((m
		(syntax-rules ()
		  ((m a b)
		   (ck-expression (ck-quote (ck-if (ck-bound-identifier=? 'a 'b) 'true 'false)))))))
	    (m c d)))

	(test-equal "ck-free-identifier=?: true"
	  'true
	  (letrec-syntax
	      ((m
		(syntax-rules ()
		  ((m a)
		   (ck-expression (ck-quote (ck-if (ck-free-identifier=? 'a 'm) 'true 'false)))))))
	    (m m)))

	(test-equal "ck-free-identifier=?: false"
	  'false
	  (letrec-syntax
	      ((m
		(syntax-rules ()
		  ((m a)
		   (ck-expression (ck-quote (ck-if (ck-free-identifier=? 'a 'm) 'true 'false)))))))
	    (m c)))
	
	(test-equal "ck-equal?: numbers/true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-equal? '10 '10) 'true 'false))))

	(test-equal "ck-equal?: numbers/false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-equal? '10 "foo") 'true 'false))))

	(test-equal "ck-equal?: recursive/true"
	  'true
	  (ck-expression (ck-quote (ck-if (ck-equal? '(a . #(1 2 ("x" "y")))
						     '(a . #(1 2 ("x" "y"))))
					  'true
					  'false))))
	
	(test-equal "ck-equal?: recursive/false"
	  'false
	  (ck-expression (ck-quote (ck-if (ck-equal? '(a . #(1 2 ("x" y)))
						     '(a . #(1 2 ("x" "y"))))
					  'true
					  'false)))))

      (test-group "List processing"

	(test-group "Constructors"        
	  (test-equal "ck-cons"
	    10
	    (let ()
	      (ck-cons 'define '(x 10))
	      x))

	  (test-equal "ck-cons*"
	    '(a b . c)
	    (ck-expression (ck-quote (ck-cons* 'a 'b 'c))))

	  (test-equal "ck-list"
	    10
	    (let ()
	      (ck-list 'define 'x '10)
	      x))

	  (test-equal "ck-make-list"
	    '(a a a a a)
	    (ck-expression (ck-quote (ck-make-list (ck-5) 'a)))))

	(test-group "Selectors"

	  (test-equal "ck-car"
	    'car
	    (ck-expression (ck-car '('car . 'cdr))))
	  
	  (test-equal "ck-car"
	    'cdr
	    (ck-expression (ck-cdr '('car . 'cdr))))
	  
	  (test-equal "ck-caar"
	    'a
	    (ck-expression (ck-quote (ck-caar '((a . b) . c)))))

	  (test-equal "ck-cadr"
	    'c
	    (ck-expression (ck-quote (ck-cadr '((a . b) . (c . d))))))

	  (test-equal "ck-cdar"
	    'b
	    (ck-expression (ck-quote (ck-cdar '((a . b) . c)))))

	  (test-equal "ck-cddr"
	    'd
	    (ck-expression (ck-quote (ck-cddr '((a . b) . (c . d))))))

	  (test-equal "ck-first"
	    1
	    (ck-expression (ck-quote (ck-first '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-second"
	    2
	    (ck-expression (ck-quote (ck-second '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-third"
	    3
	    (ck-expression (ck-quote (ck-third '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-fourth"
	    4
	    (ck-expression (ck-quote (ck-fourth '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-fifth"
	    5
	    (ck-expression (ck-quote (ck-fifth '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-sixth"
	    6
	    (ck-expression (ck-quote (ck-sixth '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-seventh"
	    7
	    (ck-expression (ck-quote (ck-seventh '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-eighth"
	    8
	    (ck-expression (ck-quote (ck-eighth '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-ninth"
	    9
	    (ck-expression (ck-quote (ck-ninth '(1 2 3 4 5 6 7 8 9 10)))))

	  (test-equal "ck-tenth"
	    10
	    (ck-expression (ck-quote (ck-tenth '(1 2 3 4 5 6 7 8 9 10)))))
	  
	  (test-equal "ck-list-tail"
	    '(3 4 5)
	    (ck-expression (ck-quote (ck-list-tail '(1 2 3 4 5) (ck-2)))))

	  (test-equal "ck-list-ref"
	    '4
	    (ck-expression (ck-quote (ck-list-ref '(1 2 3 4 5) (ck-3)))))

	  (test-equal "ck-take"
	    '(1 2 3)
	    (ck-expression (ck-quote (ck-take '(1 2 3) (ck-3)))))

	  (test-equal "ck-take-right"
	    '(2 3 . d)
	    (ck-expression (ck-quote (ck-take-right '(0 1 2 3 . d) (ck-2)))))

	  (test-equal "ck-drop-right"
	    '(0 1)
	    (ck-expression (ck-quote (ck-drop-right '(0 1 2 3 . d) (ck-2)))))

	  (test-equal "ck-last"
	    'c
	    (ck-expression (ck-quote (ck-last '(a b c)))))

	  (test-equal "ck-last-pair"
	    '(c)
	    (ck-expression (ck-quote (ck-last-pair '(a b c))))))

	(test-group "Miscellaneous"
	  (test-equal "ck-append"
	    10
	    (let ()
	      (ck-append '(define) '(x) '(10))
	      x))
	  
	  (test-equal "ck-reverse"
	    '(5 4 3 2 1)
	    (ck-expression (ck-quote (ck-reverse '(1 2 3 4 5))))))
	
	(test-group "Folding, unfolding and mapping"
	  
	  (test-equal "ck-fold"
	    '(* b 2 (* a 1 ()))
	    (ck-expression (ck-quote (ck-fold (ck-cute 'ck-list '* <> ...)
					      '()
					      '(a b c)
					      '(1 2)))))
	  
	  (test-equal "ck-fold-right"
	    '(* a 1 (* b 2 ()))
	    (ck-expression (ck-quote (ck-fold-right (ck-cute 'ck-list '* <> ...)
						    '()
						    '(a b c)
						    '(1 2)))))
	  
	  (test-equal "ck-unfold"
	    '(1 2 3)
	    (ck-expression (ck-quote (ck-unfold 'ck-null? 'ck-car 'ck-cdr '(1 2 3)))))
	  
	  (test-equal "ck-unfold-right"
	    '(3 2 1)
	    (ck-expression (ck-quote (ck-unfold-right 'ck-null? 'ck-car 'ck-cdr '(1 2 3)))))

	  (test-equal "ck-map"
	    '((* . 1) (* . 2) (* . 3))
	    (ck-expression (ck-quote (ck-map (ck-cut 'ck-cons '* <>) '(1 2 3)))))

	  (test-equal "ck-append-map"
	    '(1 a 2 b 3 c)
	    (ck-expression (ck-quote (ck-append-map 'ck-list '(1 2 3) '(a b c))))))

	(test-group "Filtering"

	  (test-equal "ck-filter"
	    '(foo foo)
	    (ck-expression (ck-quote (ck-filter (ck-cut 'ck-equal? 'foo <>) '(foo bar baz foo)))))

	  (test-equal "ck-remove"
	    '(bar baz)
	    (ck-expression (ck-quote (ck-remove (ck-cut 'ck-equal? 'foo <>) '(foo bar baz foo))))))

	(test-group "Searching"
	  (define-syntax pred
	    (ck-macro-transformer ()
	      ((pred 'x 'y) (ck-equal? 'x (ck-car 'y)))))

	  (test-equal "ck-find"
	    '(2 . b)
	    (ck-expression (ck-quote (ck-find (ck-cut 'pred 2 <>) '((1 . a) (2 . b) (3 . c))))))

	  (test-equal "ck-find-tail"
	    '((2 . b) (3 . c))
	    (ck-expression (ck-quote (ck-find-tail (ck-cut 'pred 2 <>) '((1 . a) (2 . b) (3 . c))))))

	  (test-equal "ck-take-while"
	    '()
	    (ck-expression (ck-quote (ck-take-while (ck-cut 'pred 2 <>) '((1 . a) (2 . b) (3 . c))))))

	  (test-equal "ck-drop-while"
	    '((1 . a) (2 . b) (3 . c))
	    (ck-expression (ck-quote (ck-drop-while (ck-cut 'pred 2 <>) '((1 . a) (2 . b) (3 . c))))))

	  (test-equal "ck-any"
	    '#t
	    (ck-expression (ck-quote (ck-any (ck-cut 'pred 2 <>) '((1 . a) (2 . b) (3 . c))))))

	  (test-equal "ck-every"
	    '#f
	     (ck-expression (ck-quote (ck-every (ck-cut 'pred 2 <>) '((1 . a) (2 . b) (3 . c))))))

	  (test-equal "ck-member"
	    '(2 3)
	    (ck-expression (ck-quote (ck-member '2 '(1 2 3) 'ck-equal?)))))

	(test-group "Association lists"

	  (test-equal "ck-assoc"
	    '(2 . b)
	    (ck-expression (ck-quote (ck-assoc '2 '((1 . a) (2 . b) (3 . c))))))

	  (test-equal "ck-assoc"
	    '((1 . a) (3 . c))
	    (ck-expression (ck-quote (ck-alist-delete '2 '((1 . a) (2 . b) (3 . c)))))))

	(test-group "Set operations"

	  (test-assert "ck-set<=: true"
	    (ck-expression (ck-set<= 'ck-equal? '(1 2) '(1 2 3))))

	  (test-assert "ck-set<=: false"
	    (ck-expression (ck-not (ck-set<= 'ck-equal? '(3 1 2 3) '(1 2)))))
	  
	  (test-assert "ck-set-adjoin"
	    (ck-expression (ck-= (ck-set-adjoin 'ck-equal? '(1 2 3) '2 '4) (ck-4))))

	  (test-assert "ck-set-union"
	    (ck-expression (ck-= (ck-set-union 'ck-equal? '(1 2 3) '(3 4 5)) (ck-5))))

	  (test-assert "ck-set-intersection"
	    (ck-expression (ck-= (ck-set-intersection 'ck-equal? '(1 2 3) '(3 4 5)) (ck-1))))

	  (test-assert "ck-set-difference"
	    (ck-expression (ck-= (ck-set-difference 'ck-equal? '(1 2 3) '(3 4 5)) (ck-2))))

	  (test-assert "ck-set-xor"
	    (ck-expression (ck-= (ck-set-xor 'ck-equal? '(1 2 3) '(3 4 5)) (ck-4)))))
	
	;; Vector processing
	(test-group "Vector processing"

	  (test-equal "ck-vector"
	    #(10 20 30)
	    (ck-expression (ck-quote (ck-vector 10 20 30))))

	  (test-equal "ck-list->vector"
	    #(10 20 30)
	    (ck-expression (ck-quote (ck-list->vector '(10 20 30)))))
	  
	  (test-equal "ck-vector->list"
	    '(10 20 30)
	    (ck-expression (ck-quote (ck-vector->list #(10 20 30)))))

	  (test-equal "ck-vector-map"
	    '#((1 . a) (2 . b) (3 . c))
	    (ck-expression (ck-quote (ck-vector-map 'ck-cons '#(1 2 3) '#(a b c d)))))
	  
	  (test-equal "ck-vector-ref"
	    20
	    (ck-expression (ck-quote (ck-vector-ref #(10 20 30) (ck-1))))))
	
	(test-group "Combinatorics"

	  (test-equal "ck-0"
	    #t
	    (ck-expression (ck-= '() (ck-0))))

	  (test-equal "ck-1"
	    #t
	    (ck-expression (ck-= '(0) (ck-1))))

	  (test-equal "ck-2"
	    #t
	    (ck-expression (ck-= '(0 1) (ck-2))))

	  (test-equal "ck-3"
	    #t
	    (ck-expression (ck-= '(0 1 2) (ck-3))))

	  (test-equal "ck-4"
	    #t
	    (ck-expression (ck-= '(0 1 2 3) (ck-4))))

	  (test-equal "ck-5"
	    #t
	    (ck-expression (ck-= '(0 1 2 3 4) (ck-5))))

	  (test-equal "ck-6"
	    #t
	    (ck-expression (ck-= '(0 1 2 3 4 5) (ck-6))))

	  (test-equal "ck-7"
	    #t
	    (ck-expression (ck-= '(0 1 2 3 4 5 6) (ck-7))))

	  (test-equal "ck-8"
	    #t
	    (ck-expression (ck-= '(0 1 2 3 4 5 6 7) (ck-8))))

	  (test-equal "ck-9"
	    #t
	    (ck-expression (ck-= '(0 1 2 3 4 5 6 7 8) (ck-9))))
	  
	  (test-equal "ck-10"
	    #t
	    (ck-expression (ck-= '(0 1 2 3 4 5 6 7 8 9) (ck-10))))

	  (test-assert "ck-=: true"
	    (ck-expression (ck-quote (ck-= '(1 2 3) '(a b c) '(foo bar baz)))))
	  
	  (test-assert "ck-=: false"
	    (ck-expression (ck-quote (ck-not (ck-= '(1 2 3) '(a b c d) '(foo bar baz))))))

	  (test-assert "ck-<: true"
	    (ck-expression (ck-quote (ck-< '(1) '(a b) '(foo bar baz)))))
	  
	  (test-assert "ck-<: false"
	    (ck-expression (ck-quote (ck-not (ck-< '(1 2) '(a b c) '(foo bar baz))))))

	  (test-assert "ck-<=: true"
	    (ck-expression (ck-quote (ck-<= '(1 2) '(a b c) '(foo bar baz)))))
	  
	  (test-assert "ck-<=: false"
	    (ck-expression (ck-quote (ck-not (ck-<= '(1 2 3) '(a b c d) '(foo bar baz))))))

	  (test-assert "ck->: true"
	    (ck-expression (ck-quote (ck-> '(1 2 3 4 5) '(a b c d) '(foo bar baz)))))
	  
	  (test-assert "ck->: false"
	    (ck-expression (ck-quote (ck-not (ck-> '(1 2 3) '(a b c d) '(foo bar baz))))))

	  (test-assert "ck->=: true"
	    (ck-expression (ck-quote (ck->= '(1 2 3 4) '(a b c d) '(foo bar baz)))))
	  
	  (test-assert "ck->=: false"
	    (ck-expression (ck-quote (ck-not (ck->= '(1 2 3) '(a b c d) '(foo bar baz))))))

	  (test-assert "ck-zero?: true"
	    (ck-expression (ck-quote (ck-zero? (ck-0)))))

	  (test-assert "ck-zero?: false"
	    (ck-expression (ck-quote (ck-not (ck-zero? (ck-1))))))
	  
	  (test-assert "ck-even?: true"
	    (ck-expression (ck-quote (ck-even? (ck-4)))))
	  
	  (test-assert "ck-even?: false"
	    (ck-expression (ck-quote (ck-not (ck-even? (ck-3))))))
	  
	  (test-assert "ck-odd?: true"
	    (ck-expression (ck-quote (ck-odd? (ck-1)))))
	  
	  (test-assert "ck-odd?: false"
	    (ck-expression (ck-quote (ck-not (ck-odd? (ck-2))))))

	  (test-equal "ck-+"
	    '(1 2 3 4 5 6 7 8)
	    (ck-expression (ck-quote (ck-+ '(1 2 3) '(4 5 6) '(7 8)))))
	  
	  (test-equal "ck--"
	    '(1 2 3)
	    (ck-expression (ck-quote (ck-- '(1 2 3 4 5 6 7 8) (ck-2) (ck-3)))))

	  (test-equal "ck-*"
	    '((1 a foo)
	      (1 a bar)
	      (2 a foo)
	      (2 a bar))
	    (ck-expression (ck-quote (ck-* '(1 2) '(a) '(foo bar)))))

	  (test-equal "ck-quotient"
	    '(1 3)
	    (ck-expression (ck-quote (ck-quotient '(1 2 3 4 5) (ck-2)))))

	  (test-equal "ck-remainder"
	    '(5)
	    (ck-expression (ck-quote (ck-remainder '(1 2 3 4 5) (ck-2)))))

	  (test-equal "ck-binom"
	    '((1 2)
	      (1 3)
	      (2 3))
	    (ck-expression (ck-quote (ck-binom '(1 2 3) (ck-2)))))
	  
	  (test-equal "ck-fact"
	    '((1 2 3)
	      (1 3 2)
	      (2 1 3)
	      (2 3 1)
	      (3 1 2)
	      (3 2 1))
	    (ck-expression (ck-quote (ck-fact '(1 2 3))))))

	(test-group "Example from specification"

	  (define-syntax simple-match
	    (ck-macro-transformer ()
	      ((simple-match expr (pattern . body) ...)
	       (ck-expression
		`(call-with-current-continuation
		  (lambda (return)
		    (let ((e expr))
		      (or (and-let* 
			      ,(%compile-pattern 'pattern 'e)
			    (call-with-values (lambda () . body) return))
			  ...
			  (error "does not match" expr)))))))))

	  (define-syntax %compile-pattern
	    (ck-macro-transformer ()
	      ((%compile-pattern '() 'e)
	       '(((null? e))))
	      ((%compile-pattern '(pattern1 pattern2 ...) 'e)
	       `(((not (null? e)))
		 (e1 (car e))
		 (e2 (cdr e))
		 ,@(%compile-pattern 'pattern1 'e1)
		 ,@(%compile-pattern '(pattern2 ...) 'e2)))
	      ((%compile-pattern 'x 'e)
	       (ck-if (ck-symbol? 'x)
		      '((x e))
		      '(((equal? x e)))))))

	  (test-equal 'ten (simple-match 10
					 (10 'ten)
					 ((11 x) x)))

	  (test-equal 'eleven (simple-match '(11 eleven)
					    (10 'ten)
					    ((11 x) x))))


	(test-end))
      )))
