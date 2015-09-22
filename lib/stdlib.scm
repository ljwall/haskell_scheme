(define (not x)
  (if x #f #t))

(define (list . args) args)

(define (id x) x)

(define (flip fn)
  (lambda (a b) (fn b a)))

(define (curry fn x)
  (lambda xs (apply fn (cons x xs))))

(define (compose f g)
  (lambda (x) (f (g x)) ))

(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define head car)
(define tail cdr)

(define (null? lst) (eqv? '() lst))

(define (foldl fn init xs)
  (if (null? xs)
    init
    (foldl fn (fn init (head xs)) (tail xs))))

(define (foldr fn init xs)
  (define foo (foldl (lambda (fn_acc x) (compose fn_acc (curry fn x))) id xs))
  (foo init))

(define (reverse xs) (foldl (flip cons) '() xs))

(define (unfold func init predicate)
  (if (predicate init)
    (cons init (unfold func (func init) predicate))
    '()))

(define (range from to . step)
  (define s (if (null? step) 1 (head step)))
  (unfold (curry + s) from (curry > to) ))

(define (map fn xs)
  (foldr (lambda (x acc) (cons (fn x) acc)) '() xs))

(define (filter predicate xs)
  (define (fn x acc)
    (if (predicate x)
      (cons x acc)
       acc))
  (foldr fn '() xs))

(define (foldl1 fn xs) (foldl fn (head xs) (tail xs)))
(define (foldr1 fn xs) (foldl1 (flip fn) (reverse xs)))

(define (sum . xs) (foldl1 + xs))
(define (product . xs) (foldl1 * xs))
(define (and . xs) (foldl1 && xs))
(define (or . xs) (foldl1 || xs))

(define (length xs) (foldl (lambda (acc x) (+ acc 1)) 0 xs))

(define (string->charlist str)
  (map (curry string-ref str) (range 0 (string-length str))))

(define (split-by sep xs)
  (define (fn x acc)
    (if (eqv? x sep)
      (cons '() acc)
      (cons (cons x (head acc)) (tail acc) )))
  (foldr fn '(()) xs))

(define (string-split-by sep str)
  (map (curry apply string)
       (split-by sep (string->charlist str)) ))
