(define (days-in-month year month)
  (let ((evenMonth (remainder month 2)) (leapYear (if (= (remainder year 4) 0) (if (= (remainder year 100) 0) (= (remainder year 400) 0) #t) #f)  ))
    (cond ((= month 1) (if leapYear 29 28))
          ((= month 7) 31)
          (else (if (<= month 7) (if (= (remainder month 2) 1) 30 31) (if (= (remainder month 2) 1) 31 30))))))

(define (accum-months year month)
  (letrec ((accum-help (lambda (y m res)
     (let ((next-month (if (= m 0) 11 (- m 1))) (next-year (if (= m 0) (- y 1) y)) )
       (if (and (= y 1970) (= m 0)) res
           (accum-help next-year next-month (+ res (days-in-month next-year next-month))))))))
    (accum-help year month 0)))

(define (date-to-epoch year month day hour minut secound)
  (let ((days (+ (accum-months year month) day)))
    (+ (* (+ (* (+ (* days 24) hour) 60) minut) 60) secound)
    ))


