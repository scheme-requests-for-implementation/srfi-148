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

(scheme-define-syntax :c
  (scheme-syntax-rules ()))

(scheme-define-syntax expand-transformer
  (scheme-syntax-rules (scheme-syntax-rules scheme-er-macro-transformer syntax-error begin)
    ((expand-transformer (k ...) (scheme-syntax-rules . args))
     (k ... (scheme-syntax-rules . args)))
    ((expand-transformer (k ...) (scheme-er-macro-transformer . args))
     (k ... (scheme-er-macro-transformer . args)))
    ((expand-transformer (k ...) (syntax-error . args))
     (syntax-error . args))
    ((expand-transformer (k ...) (begin definition ... transformer-spec))
     (begin definition
	    ...
	    (expand-transformer (k ...) transformer-spec)))   
    ((expand-transformer (k ...) (keyword . args))
     (keyword (:c expand-transformer (k ...)) . args))
    ((expand-transformer (k ...) keyword)
     (k ... (scheme-syntax-rules ()
	      ((_ . args) (keyword . args)))))))

(scheme-define-syntax define-syntax
  (scheme-syntax-rules ()
    ((define-syntax name transformer-spec)
     (expand-transformer (scheme-define-syntax name) transformer-spec))
    ((define-syntax . _)
     (syntax-error "invalid define-syntax syntax"))))

(scheme-define-syntax let-syntax
  (scheme-syntax-rules ()
    ((let-syntax ((keyword transformer-spec) ...) body1 body2 ...)
     (let ()
       (let-syntax-aux (keyword ...) (transformer-spec ...) () (body1 body2 ...))))
    ((let-syntax . _)
     (syntax-error "invalid let-syntax syntax"))))

(scheme-define-syntax let-syntax-aux
  (scheme-syntax-rules ()
    ((let-syntax-aux (keyword ...) () (transformer-spec ...) body*)
     (scheme-let-syntax ((keyword transformer-spec) ...) . body*))
    ((let-syntax-aux keyword* (transformer-spec1 transformer-spec2 ...) transformer-spec* body*)
     (expand-transformer (let-syntax-aux keyword*
					    (transformer-spec2 ...)
					    transformer-spec*
					    body*)
			 transformer-spec1))
    ((let-syntax-aux keyword*
		     (transformer-spec2 ...)
		     (transformer-spec ...)
		     body*
		     transformer-spec1)
     (let-syntax-aux keyword*
		     (transformer-spec2 ...)
		     (transformer-spec ... transformer-spec1)
		     body*))))

(scheme-define-syntax letrec-syntax
  (scheme-syntax-rules ()
    ((letrec-syntax ((keyword transformer-spec) ...) body1 body2 ...)
     (let ()
       (letrec-syntax-aux (keyword ...) (transformer-spec ...) () (body1 body2 ...))))
    ((letrec-syntax . _)
     (syntax-error "invalid letrec-syntax syntax"))))

(scheme-define-syntax letrec-syntax-aux
  (scheme-syntax-rules ()
    ((letrec-syntax-aux (keyword ...) () (transformer-spec ...) body*)
     (begin
       (define-syntax keyword transformer-spec)
       ...
       (let () . body*)))
    ((letrec-syntax-aux keyword*
			(transformer-spec1 transformer-spec2 ...)
			transformer-spec*
			body*)
     (expand-transformer (letrec-syntax-aux keyword*
					       (transformer-spec2 ...)
					       transformer-spec*
					       body*)
			 transformer-spec1))
    ((letrec-syntax-aux keyword*
			(transformer-spec2 ...)
			(transformer-spec ...)
			body*
			transformer-spec1)
     (letrec-syntax-aux keyword*
			(transformer-spec2 ...)
			(transformer-spec ... transformer-spec1)
			body*))))

(scheme-define-syntax syntax-rules
  (scheme-syntax-rules (:c)
    ((syntax-rules (:c k ...) . args)
     (syntax-rules-aux "state0" :c (k ...) . args))
    ((syntax-rules . _)
     (syntax-error "invalid syntax-rules syntax"))))

(scheme-define-syntax syntax-rules-aux
  (scheme-syntax-rules ()
    ((syntax-rules-aux "state0" :c k* (literal* ...) . rule*)
     (syntax-rules-aux "state1" :c k* (... ...) ((literal* ... :c)) rule* () rule*))

    ((syntax-rules-aux "state0" :c k* ellipsis (literal* ...) . rule*)
     (syntax-rules-aux "state1" :c k* ellipsis (ellipsis (literal* ... :c))
       rule* () rule*))
   
    ((syntax-rules-aux "state1" :c (k ...) e (l ...) () (rule1* ...) rule2*)
     (k ... (scheme-syntax-rules l ... rule1* ... . rule2*)))

    ((syntax-rules-aux "state1" :c k* ::: l*
       (((_ . pattern) template) . rule1*) (rule2 ...) rule3*)
     (syntax-rules-aux "state1" :c k* ::: l* rule1*
       (rule2
	...
	((_ (:c c :::) . pattern)
	 (c ::: template)))
       rule3*))
    ((syntax-rules-aux . _)
     (syntax-error "invalid syntax-rules syntax"))))


;; Local Variables:
;; eval: (put 'scheme-define-syntax 'scheme-indent-function 'defun)
;; eval: (put 'scheme-syntax-rules 'scheme-indent-function 'defun)
;; eval: (put 'syntax-rules-aux 'scheme-indent-function 'defun)
;; eval: (font-lock-add-keywords 'scheme-mode
;;                               '(("(\\(scheme-define-syntax\\)\\>" 1 font-lock-keyword-face)
;;                                 ("(\\(scheme-syntax-rules\\)\\>" 1 font-lock-keyword-face)))
;; End:
