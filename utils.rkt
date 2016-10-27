;my implentation of map redefines the default
(define (map func lst)
  (reverse (map-helper func lst '())))

;map whith result parameter
(define (map-helper func lst res)
  (if  (empty? lst) res
       (map-helper func (cdr lst) (cons (func (car lst)) res))))

;general find-all procedure that finds all elements in a list that satifies a predicat
(define (find-all pred lst res)
  (if (empty? lst) res
      (reverse (find-all pred (cdr lst) (if (pred (car lst)) (cons (car lst) res) res )))))

;find the first element that satifies a predicat
(define (find-first pred lst)
  (if (empty? lst) 'None
      (if (pred (car lst)) (car lst) (find-first pred (cdr lst)) )))

;check if alle elements in a list satifies a predicat
(define (for-all pred lst)
  (fold #t (lambda (x y) (and x y)) (map pred lst)))

;fold reduce acumulate (kært barn har mange navne)
(define (fold init op lst)
  (if (= (length lst) 0) init
      (fold (op init (car lst)) op (cdr lst))))

;checks if a given element is to be found in a list
(define (exists a lst)
  (if (equal? (find-first (lambda (x) (equal? a x)) lst) 'None) #f #t))

;fold strings into one string
(define (fold-string lst)
  (fold "" string-append lst))