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


;; General

(define-syntax ck-constant
  (ck-macro-transformer ::: ()
    ((ck-constant 'const)
     (ck-cut 'ck-constant-aux 'const <> ...))))

(define-syntax ck-constant-aux
  (ck-macro-transformer ()
    ((ck-constant-aux 'const 'arg ...)
     'const)))

(define-syntax ck-append
  (ck-macro-transformer ()
    ((ck-append) ''())
    ((ck-append 'l) 'l)
    ((ck-append 'm ... '(a ...) 'l) (ck-append 'm ... '(a ... . l)))))

(define-syntax ck-list
  (ck-macro-transformer ()
    ((ck-list 'a ...) '(a ...))))

(define-syntax ck-cons
  (ck-macro-transformer ()
    ((ck-cons 'h 't) '(h . t))))

(define-syntax ck-cons*
  (ck-macro-transformer ()
    ((ck-cons* 'e ... 't) '(e ... . t))))

(define-syntax ck-car
  (ck-macro-transformer ()
    ((ck-car '(h . t)) 'h)))

(define-syntax ck-cdr
  (ck-macro-transformer ()
    ((ck-cdr '(h . t)) 't)))

(define-syntax ck-apply
  (ck-macro-transformer ()
    ((ck-apply 'keyword 'datum1 ... '(datum2 ...))
     (keyword 'datum1 ... 'datum2 ...))))

(define-syntax ck-call
  (ck-macro-transformer ()
    ((ck-apply 'keyword 'datum ...)
     (keyword 'datum ...))))

(define-syntax ck-eval
  (ck-macro-transformer ()
    ((ck-eval '(keyword datum ...))
     (keyword datum ...))))

(define-syntax ck-error
  (ck-macro-transformer ()
    ((ck-error 'message 'arg ...)
     (ck-suspend 'ck-error-aux 'message 'arg ...))))

(define-syntax ck-error-aux
  (syntax-rules ()
    ((ck-error-aux s message arg ...)
     (syntax-error message arg ...))))

(define-syntax ck-gensym
  (ck-macro-transformer ()
    ((ck-gensym) 'g)))

(define-syntax ck-generate-temporaries
  (ck-macro-transformer ()
    ((ck-generate-temporaries '()) '())
    ((ck-generate-temporaries '(h . t))
     (ck-cons (ck-gensym) (ck-generate-temporaries 't)))))

(define-syntax ck-quote
  (ck-macro-transformer ()
    ((ck-quote 'x) ''x)))

;; Boolean logic

(define-syntax ck-if
  (ck-macro-transformer ()
    ((ck-if '#f consequent alternate)
     alternate)
    ((ck-if 'test consequent alternate)
     consequent)))

(define-syntax ck-not
  (ck-macro-transformer ()
    ((ck-not '#f)
     '#t)
    ((ck-not '_)
     '#f)))

(define-syntax ck-or
  (ck-macro-transformer ()
    ((ck-or)
     '#f)
    ((ck-or 'x y ...)
     (ck-if 'x 'x (ck-or y ...)))))

(define-syntax ck-and
  (ck-macro-transformer ()
    ((ck-and 'x)
     'x)
    ((ck-and 'x y ...)
     (ck-if 'x (ck-and y ...) '#f))))

(define-syntax ck-null?
  (ck-macro-transformer ()
    ((ck-null? '())
     '#t)
    ((ck-null? '_)
     '#f)))

(define-syntax ck-pair?
  (ck-macro-transformer ()
    ((ck-null? '(_ . _))
     '#t)
    ((ck-null? '_)
     '#f)))

(define-syntax ck-list?
  (ck-macro-transformer ()
    ((ck-list? '())
     '#t)
    ((ck-list? '(_ . x))
     (ck-list? 'x))
    ((ck-list? '_)
     '#f)))

(define-syntax ck-boolean?
  (ck-macro-transformer ()
    ((ck-boolean? '#f)
     '#t)
    ((ck-boolean? '#t)
     '#t)
    ((ck-boolean? '_)
     '#f)))

(define-syntax ck-vector?
  (ck-macro-transformer ()
    ((ck-vector? '#(x ...))
     '#t)
    ((ck-vector? '_)
     '#f)))

(define-syntax ck-symbol?
  (ck-macro-transformer ()
    ((ck-symbol? '(x . y)) '#f)
    ((ck-symbol? '#(x ...)) '#f)
    ((ck-symbol? 'x)
     (ck-suspend 'ck-symbol?-aux 'x))))

(define-syntax ck-symbol?-aux
  (syntax-rules ()
    ((ck-symbol?-aux s x)
     (begin
       (define-syntax test
	 (syntax-rules ::: ()
		       ((test x %s) (ck-resume %s '#t))
		       ((test y %s) (ck-resume %s '#f))))
       (test symbol s)))))

(define-syntax ck-bound-identifier=?
  (ck-macro-transformer ()
    ((ck-bound-identifier=? 'id 'b)
     (ck-suspend ck-bound-identifier=?-aux 'id 'b))))

(define-syntax ck-bound-identifier=?-aux
  (syntax-rules ()
    ((ck-bound-identifier=?-aux s id b)
     (bound-identifier=? id b (ck-resume s '#t) (ck-resume s '#f)))))

(define-syntax ck-free-identifier=?
  (ck-macro-transformer ()
    ((ck-free-identifier=? 'id1 'id2)
     (ck-suspend ck-free-identifier=?-aux 'id1 'id2))))

(define-syntax ck-free-identifier=?-aux
  (syntax-rules ()
    ((ck-free-identifier=?-aux s id1 id2)
     (free-identifier=? id1 id2 (ck-resume s '#t) (ck-resume s '#f)))))

(define-syntax ck-constant=?
  (ck-macro-transformer ()
    ((ck= 'x 'y)
     (ck-suspend ck-constant=?-aux 'x 'y))))

(define-syntax ck-constant=?-aux
  (syntax-rules ()
    ((ck-constant=?-aux s x y)
     (begin
       (define-syntax test
	 (syntax-rules ::: ()
		       ((test x %s) (ck-resume %s '#t))
		       ((test z %s) (ck-resume %s '#f))))
       (test y s)))))

(define-syntax ck-equal?
  (ck-macro-transformer ()
    ((ck-equal? '(x . y) '(u . v))
     (ck-and (ck-equal? 'x 'u) (ck-equal? 'y 'v)))
    ((ck-equal '#(x ...) '#(u ...))
     (ck-and (ck-equal? 'x 'u) ...))
    ((ck-equal '() '())
     '#t)
    ((ck-equal 'x 'u)
     (ck-if (ck-symbol? 'x)
	    (ck-bound-identifier=? 'x 'u)
	    (ck-constant=? 'x 'u)))
    ((ck-equal '_ '_)
     '#f)))

;; List processing

(define-syntax ck-caar
  (ck-macro-transformer ()
    ((ck-caar '((a . b) . c))
     'a)))

(define-syntax ck-cadr
  (ck-macro-transformer ()
    ((ck-cadr '(a . (b . c)))
     'b)))

(define-syntax ck-cdar
  (ck-macro-transformer ()
    ((ck-cdar '((a . b) . c))
     'b)))

(define-syntax ck-cddr
  (ck-macro-transformer ()
    ((ck-cddr '(a . (b . c)))
     'c)))

(define-syntax ck-first
  (ck-macro-transformer ()
    ((ck-first '(a . z))
     'a)))

(define-syntax ck-second
  (ck-macro-transformer ()
    ((ck-second '(a b . z))
     'b)))

(define-syntax ck-third
  (ck-macro-transformer ()
    ((ck-third '(a b c . z))
     'c)))

(define-syntax ck-fourth
  (ck-macro-transformer ()
    ((ck-forth '(a b c d . z))
     'd)))

(define-syntax ck-fifth
  (ck-macro-transformer ()
    ((ck-fifth '(a b c d e . z))
     'e)))

(define-syntax ck-sixth
  (ck-macro-transformer ()
    ((ck-sixth '(a b c d e f . z))
     'f)))

(define-syntax ck-seventh
  (ck-macro-transformer ()
    ((ck-seventh '(a b c d e f g . z))
     'g)))

(define-syntax ck-eighth
  (ck-macro-transformer ()
    ((ck-eighth '(a b c d e f g h . z))
     'h)))

(define-syntax ck-ninth
  (ck-macro-transformer ()
    ((ck-ninth '(a b c d e f g h i . z))
     'i)))

(define-syntax ck-tenth
  (ck-macro-transformer ()
    ((ck-tenth '(a b c d e f g h i j . z))
     'j)))

(define-syntax ck-make-list
  (ck-macro-transformer ()
    ((ck-make-list '() 'fill)
     '())
    ((ck-make-list '(h . t) 'fill)
     (ck-cons 'fill (ck-make-list 't 'fill)))))

(define-syntax ck-reverse
  (ck-macro-transformer ()
    ((ck-reverse '())
     '())
    ((ck-reverse '(h ... t))
     (ck-cons 't (ck-reverse '(h ...))))))

(define-syntax ck-list-tail
  (ck-macro-transformer ()
    ((ck-list-tail 'list '())
     'list)
    ((ck-list-tail '(h . t) '(u . v))
     (ck-list-tail 't 'v))))

(define-syntax ck-drop
  (ck-macro-transformer ()
    ((ck-drop 'arg ...)
     (ck-list-ref 'arg ...))))

(define-syntax ck-list-ref
  (ck-macro-transformer ()
    ((ck-list-ref '(h . t) '())
     'h)
    ((ck-list-ref '(h . t) '(u . v))
     (ck-list-ref 't 'v))))

(define-syntax ck-take
  (ck-macro-transformer ()
    ((ck-take '_ '())
     '())
    ((ck-take '(h . t) '(u . v))
     (ck-cons 'h (ck-take 't 'v)))))

(define-syntax ck-take-right
  (ck-macro-transformer ()
    ((ck-take-right '(a ... . t) '())
     't)
    ((ck-take-right '(a ... b . t) '(u . v))     
     `(,@(ck-take-right '(a ...) 'v) b . t)
     )))

(define-syntax ck-drop-right
  (ck-macro-transformer ()
    ((ck-drop-right '(a ... . t) '())
     '(a ...))
    ((ck-drop-right '(a ... b . t) '(u . v))
     (ck-drop-right '(a ...) 'v))))

(define-syntax ck-last
  (ck-macro-transformer ()
    ((ck-last '(a ... b . t))
     'b)))

(define-syntax ck-last-pair
  (ck-macro-transformer ()
    ((ck-last '(a ... b . t))
     '(b . t))))

;; Folding, unfolding and mapping

(define-syntax ck-fold
  (ck-macro-transformer ()
    ((ck-fold 'kons 'knil '(h . t) ...)
     (ck-fold 'kons (kons 'h ... 'knil) 't ...))
    ((ck-fold 'kons 'knil '_ ...)
     'knil)))

(define-syntax ck-fold-right
  (ck-macro-transformer ()
    ((ck-fold-right 'kons 'knil '(h . t) ...)
     (kons 'h ... (ck-fold-right 'kons 'knil 't ...)))
    ((ck-fold-right 'kons 'knil '_ ...)
     'knil)))

(define-syntax ck-unfold
  (ck-macro-transformer ()
    ((ck-unfold 'stop? 'mapper 'successor 'seed 'tail-mapper)
     (ck-if (stop? 'seed)
	    (tail-mapper 'seed)
	    (ck-cons (mapper 'seed)
		     (ck-unfold 'stop? 'mapper 'successor (successor 'seed) 'tail-mapper))))
    ((ck-unfold 'stop? 'mapper 'successor 'seed)
     (ck-unfold 'stop? 'mapper 'successor 'seed (ck-constant '())))))

(define-syntax ck-unfold-right
  (ck-macro-transformer ()
    ((ck-unfold-right 'stop? 'mapper 'successor 'seed 'tail)
     (ck-if (stop? 'seed)
	    'tail
	    (ck-unfold-right 'stop?
			     'mapper
			     'successor
			     (successor 'seed)
			     (ck-cons (mapper 'seed) 'tail))))
    ((ck-unfold-right 'stop? 'mapper 'successor 'seed)
     (ck-unfold-right 'stop? 'mapper 'successor 'seed '()))))

(define-syntax ck-map
  (ck-macro-transformer ()
    ((ck-map 'proc '(h . t) ...)
     (ck-cons (proc 'h ...) (ck-map 'proc 't ...)))
    ((ck-map 'proc '_ ...)
     '())))

(define-syntax ck-append-map
  (ck-macro-transformer ()
    ((ck-append-map 'proc '(h . t) ...)
     (ck-append (proc 'h ...) (ck-append-map 'proc 't ...)))
    ((ck-append-map map 'proc '_)
     '())))

;; Filtering

(define-syntax ck-filter
  (ck-macro-transformer ()
    ((ck-filter 'pred '())
     '())
    ((ck-filter 'pred '(h . t))
     (ck-if (pred 'h)
	    (ck-cons 'h (ck-filter 'pred 't))
	    (ck-filter 'pred 't)))))

(define-syntax ck-remove
  (ck-macro-transformer ()
    ((ck-remove 'pred '())
     '())
    ((ck-remove 'pred '(h . t))
     (ck-if (pred 'h)
	    (ck-remove 'pred 't)
	    (ck-cons 'h (ck-remove 'pred 't))))))

;; Searching

(define-syntax ck-find
  (ck-macro-transformer ()
    ((ck-find 'pred '())
     '())
    ((ck-find 'pred '(h . t))
     (ck-if (pred 'h)
	    'h
	    (ck-find 'pred 't)))))

(define-syntax ck-find-tail
  (ck-macro-transformer ()
    ((ck-find-tail 'pred '())
     '#f)
    ((ck-find-tail 'pred '(h . t))
     (ck-if (pred 'h)
	    '(h . t)
	    (ck-find-tail 'pred 't)))))

(define-syntax ck-take-while
  (ck-macro-transformer ()
    ((ck-take-while 'pred '())
     '())
    ((ck-take-while 'pred '(h . t))
     (ck-if (pred 'h)
	    (ck-cons 'h (ck-take-while 'pred 't))
	    '()))))

(define-syntax ck-drop-while
  (ck-macro-transformer ()
    ((ck-drop-while 'pred '())
     '())
    ((ck-drop-while 'pred '(h . t))
     (ck-if (pred 'h)
	    (ck-drop-while 'pred 't)
	    '(h . t)))))

(define-syntax ck-any
  (ck-macro-transformer ()
    ((ck-any 'pred '(h . t) ...)
     (ck-or (pred 'h ...) (ck-any 'pred 't ...)))
    ((ck-any 'pred '_ ...)
     '#f)))

(define-syntax ck-every
  (ck-macro-transformer ()
    ((ck-every 'pred '() ...)
     '#t)
    ((ck-every 'pred '(a b . x) ...)
     (ck-and (pred 'a ...) (ck-every 'pred '(b . x) ...)))
    ((ck-every 'pred '(h . t) ...)
     (pred 'h ...))))

(define-syntax ck-member
  (ck-macro-transformer ()
    ((ck-member 'obj 'list 'compare)
     (ck-find-tail (ck-cut 'compare 'obj <>) 'list))
    ((ck-member 'obj 'list)
     (ck-member 'obj 'list 'ck-equal?))))

;; Association lists

(define-syntax ck-assoc
  (ck-macro-transformer ()
    ((ck-assoc 'key '() 'compare)
     '#f)
    ((ck-assoc 'key '((k . v) . t) 'compare)
     (ck-if (compare 'key 'k)
	    '(k . v)
	    (ck-assoc 'key 't 'compare)))
    ((ck-assoc 'key 'alist)
     (ck-assoc 'key 'alist 'ck-equal?))))

(define-syntax ck-alist-delete
  (ck-macro-transformer ()
    ((ck-alist-delete 'key '() 'compare)
     '())
    ((ck-alist-delete 'key '((k . v) . t) 'compare)
     (ck-if (compare 'key 'k)
	    (ck-alist-delete 'key 't 'compare)
	    (ck-cons '(k . v) (ck-alist-delete 'key 't 'compare))))
    ((ck-alist-delete 'key 'alist)
     (ck-alist-delete 'key 'alist 'ck-equal?))))

;; Set operations

(define-syntax ck-set<=
  (ck-macro-transformer ()
    ((ck-set<= 'compare '())
     '#t)
    ((ck-set<= 'compare 'list)
     '#t)
    ((ck-set<= 'compare '() 'list)
     '#t)
    ((ck-set<= 'compare '(h . t) 'list)
     (ck-and (ck-member 'h 'list 'compare)
	     (ck-set<= 'compare 't 'list)))
    ((ck-set<= 'compare 'list1 'list2 'list ...)
     (ck-and (ck-set<= 'compare 'list1 'list2)
	     (ck-set<= 'compare 'list2 'list ...)))))

(define-syntax ck-set=
  (ck-macro-transformer ()
    ((ck-set= 'compare 'list)
     '#t)
    ((ck-set= 'compare 'list1 list2)
     (ck-and (ck-set<= 'compare 'list1 'list2)
	     (ck-set<= 'compare 'list2 'list1)))
    ((ck-set= 'compare 'list1 'list2 'list ...)
     (ck-and (ck-set= 'list1 'list2)
	     (ck-set= 'list1 'list ...)))))

(define-syntax ck-set-adjoin
  (ck-macro-transformer ()
    ((ck-set-adjoin 'compare 'list)
     'list)
    ((ck-set-adjoin 'compare 'list 'element1 'element2 ...)
     (ck-if (ck-member 'element1 'list 'compare)
	    (ck-set-adjoin 'compare 'list 'element2 ...)
	    (ck-set-adjoin 'compare (ck-cons 'element1 'list) 'element2 ...)))))

(define-syntax ck-set-union
  (ck-macro-transformer ()
    ((ck-set-union 'compare 'list ...)
     (ck-apply 'ck-set-adjoin 'compare '() (ck-append 'list ...)))))

(define-syntax ck-set-intersection
  (ck-macro-transformer ()
    ((ck-set-intersection 'compare 'list)
     'list)
    ((ck-set-intersection 'compare 'list1 'list2)
     (ck-filter (ck-cut 'ck-member <> 'list2 'compare) 'list1))
    ((ck-set-intersection 'compare 'list1 'list2 'list ...)
     (ck-set-intersection 'compare (ck-set-intersection 'list1 'list2) 'list ...))))

(define-syntax ck-set-difference
  (ck-macro-transformer ()
    ((ck-set-difference 'compare 'list)
     'list)
    ((ck-set-difference 'compare 'list1 'list2)
     (ck-remove (ck-cut 'ck-member <> 'list2 'compare) 'list1))
    ((ck-set-difference 'compare 'list1 'list2 'list ...)
     (ck-set-difference 'compare (ck-set-difference 'list1 'list2) 'list ...))))

(define-syntax ck-set-xor
  (ck-macro-transformer ()
    ((ck-set-xor 'compare 'list1 'list2)
     (ck-set-union 'compare
		   (ck-set-difference 'compare 'list1 'list2)
		   (ck-set-difference 'compare 'list2 'list1)))))

;; Vector processing

(define-syntax ck-vector
  (ck-macro-transformer ()
    ((ck-vector 'element ...)
     '#(element ...))))

(define-syntax ck-list->vector
  (ck-macro-transformer ()
    ((ck-list->vector '(element ...))
     '#(element ...))))

(define-syntax ck-vector->list
  (ck-macro-transformer ()
    ((ck-list->vector '#(x ...))
     '(x ...))))

(define-syntax ck-vector-map
  (ck-macro-transformer ()
    ((ck-vector-map 'proc 'vector ...)
     (ck-list->vector (ck-map 'proc (ck-vector->list 'vector) ...)))))

(define-syntax ck-vector-ref
  (ck-macro-transformer ()
    ((ck-vector-ref '#(element1 element2 ...) '())
     'element1)
    ((ck-vector-ref '#(element1 element2 ...) '(h . t))
     (ck-vector-ref '#(element2 ...) 't))))

;; Combinatorics

(define-syntax ck-0
  (ck-macro-transformer ()
    ((ck-0)
     '())))

(define-syntax ck-1
  (ck-macro-transformer ()
    ((ck-1)
     '(0))))

(define-syntax ck-2
  (ck-macro-transformer ()
    ((ck-2)
     '(0 1))))

(define-syntax ck-3
  (ck-macro-transformer ()
    ((ck-3)
     '(0 1 2))))

(define-syntax ck-4
  (ck-macro-transformer ()
    ((ck-4)
     '(0 1 2 3))))

(define-syntax ck-5
  (ck-macro-transformer ()
    ((ck-5)
     '(0 1 2 3 4))))

(define-syntax ck-6
  (ck-macro-transformer ()
    ((ck-6)
     '(0 1 2 3 4 5))))

(define-syntax ck-7
  (ck-macro-transformer ()
    ((ck-7)
     '(0 1 2 3 4 5 6))))

(define-syntax ck-8
  (ck-macro-transformer ()
    ((ck-8)
     '(0 1 2 3 4 5 6 7))))

(define-syntax ck-9
  (ck-macro-transformer ()
    ((ck-9)
     '(0 1 2 3 4 5 6 7 8))))

(define-syntax ck-10
  (ck-macro-transformer ()
    ((ck-10)
     '(0 1 2 3 4 5 6 7 8 9))))

(define-syntax ck-=
  (ck-macro-transformer ()
    ((ck-= '_)
     '#t)
    ((ck-= '() '())
     '#t)
    ((ck-= '(h . t) '())
     '#f)
    ((ck-= '() '(h . t))
     '#f)
    ((ck-= '(h . t) '(u . v))
     (ck-= 't 'v))
    ((ck-= 'list1 'list2 'list ...)
     (ck-and (ck-= 'list1 'list2)
	     (ck-= 'list1 'list ...)))))

(define-syntax ck-<
  (ck-macro-transformer ()
    ((ck-<)
     '#t)
    ((ck-< 'list)
     '#t)
    ((ck-< '_ '())
     '#f)
    ((ck-< '() '_)
     '#t)
    ((ck-< '(t . h) '(u . v))
     (ck-< 'h 'v))
    ((ck-< 'list1 'list2 'list ...)
     (ck-and (ck-< 'list1 'list2)
	     (ck-< 'list2 'list ...)))))

(define-syntax ck-<=
  (ck-macro-transformer ()
    ((ck-<=)
     '#t)
    ((ck-<= 'list)
     '#t)
    ((ck-<= '() '_)
     '#t)
    ((ck-<= '_ '())
     '#f)
    ((ck-<= '(t . h) '(u . v))
     (ck-<= 'h 'v))
    ((ck-<= 'list1 'list2 'list ...)
     (ck-and (ck-<= 'list1 'list2)
	     (ck-<= 'list2 'list ...)))))

(define-syntax ck->
  (ck-macro-transformer ()
    ((ck-> 'list ...)
     (ck-apply 'ck-< (ck-reverse '(list ...))))))

(define-syntax ck->=
  (ck-macro-transformer ()
    ((ck->= 'list ...)
     (ck-apply 'ck-<= (ck-reverse '(list ...))))))

(define-syntax ck-zero? ck-null?)

(define-syntax ck-even?
  (ck-macro-transformer ()
    ((ck-even? '())
     '#t)
    ((ck-even? '(a b . c))
     (ck-even? 'c))
    ((ck-even? '_)
     '#f)))

(define-syntax ck-odd?
  (ck-macro-transformer ()
    ((ck-odd? 'list)
     (ck-not (ck-even? 'list)))))

(define-syntax ck-+ ck-append)

(define-syntax ck--
  (ck-macro-transformer ()
    ((ck-- 'list)
     'list)
    ((ck-- 'list '())
     'list)
    ((ck-- '(a ... b) '(u . v))
     (ck-- '(a ...) 'v))
    ((ck-- 'list1 'list2 'list ...)
     (ck-- (ck-- 'list1 'list2) 'list ...))))

(define-syntax ck-*
  (ck-macro-transformer ()
    ((ck-*)
     '(()))
    ((ck-* 'list1 'list2 ...)
     (ck-*-aux 'list1 (ck-* 'list2 ...)))))

(define-syntax ck-*-aux
  (ck-macro-transformer ()
    ((ck-*-aux '(x ...) 'list)
     (ck-append (ck-map (ck-cut 'ck-cons 'x <>) 'list) ...))
    ((ck-*-aux arg ...)
     (ck-error '"???" 'arg ...))))

(define-syntax ck-quotient
  (ck-macro-transformer ()
    ((ck-quotient 'list 'k)
     (ck-if (ck->= 'list 'k)
	    (ck-cons (ck-car 'list)
		     (ck-quotient (ck-list-tail 'list 'k) 'k))
	    '()))))

(define-syntax ck-remainder
  (ck-macro-transformer ()
    ((ck-quotient 'list 'k)
     (ck-if (ck->= 'list 'k)
	    (ck-remainder (ck-list-tail 'list 'k) 'k)
	    'list))))

(define-syntax ck-binom
  (ck-macro-transformer ()
    ((ck-binom 'list '())
     '(()))
    ((ck-binom '() '(h . t))
     '())
    ((ck-binom '(u . v) '(h . t))
     (ck-append (ck-map (ck-cut 'ck-cons u <>) (ck-binom 'v 't))
		(ck-binom 'v '(h . t))))))

(define-syntax ck-fact
  (ck-macro-transformer ()
    ((ck-fact '())
     '(()))
    ((ck-fact 'list)
     (ck-append-map 'ck-fact-cons*
		    'list (ck-map 'ck-fact (ck-fact-del 'list))))))

(define-syntax ck-fact-del
  (ck-macro-transformer ()
    ((ck-fact-del '())
     '())
    ((ck-fact-del '(h . t))
     `(t ,@(ck-map (ck-cut 'ck-cons 'h <>) (ck-fact-del 't))))))

(define-syntax ck-fact-cons*
  (ck-macro-transformer ()
    ((ck-fact-cons* 'a '((l ...) ...))
     '((a l ...) ...))))
 
