(define quasiquote^ (lambda (t)
  (if (pair? t)
      (if (eq? (car t) 'unquote)
	  (cadr t)
	  (cons 'cons 
		(cons (quasiquote^ (car t))
		      (cons (quasiquote^ (cdr t))
			    '()))))
      (cons 'quote (cons t '())))))

