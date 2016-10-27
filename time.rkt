
;a function that return the amount of day in a given month in a given year.
(define (days-in-month year month)
  (let ((evenMonth (remainder month 2)) (leapYear (if (= (remainder year 4) 0) (if (= (remainder year 100) 0) (= (remainder year 400) 0) #t) #f)  ))
    (cond ((= month 1) (if leapYear 29 28)) ;check for leap year and if it's febuary
          ((= month 7) 31)
          (else (if (<= month 7) (if (= (remainder month 2) 1) 30 31) (if (= (remainder month 2) 1) 31 30))))))

;a function that accumulates the amunt of days in months until it reaches 1 Januar 1970 (unix timestamp)
(define (accum-months year month)
  (letrec ((accum-help (lambda (y m res); helper function which y = start year m = start month
     (let ((next-month (if (= m 0) 11 (- m 1))) (next-year (if (= m 0) (- y 1) y)) )
       (if (and (= y 1970) (= m 0)) res
           (accum-help next-year next-month (+ res (days-in-month next-year next-month))))))))
    (accum-help year month 0)))

;a function that converts from (list (year month day hour minute secound)) to unix timestamp aka epoch (integer)
(define (date-to-epoch-helper lst)
  (let ((days (+ (accum-months (list-ref lst 0) (list-ref lst 1)) (list-ref lst 2))))
    (+ (* (+ (* (+ (* days 24) (list-ref lst 3)) 60) (list-ref lst 4)) 60) (list-ref lst 5))))

;flexible parameters for function above and check if year > 1970 after unix timestamp
(define (date-to-epoch x . y)
  (if (and (integer? x) (>= x 1970))
      (date-to-epoch-helper (cons x y))
      (if (and (list? x) (= (length x) 6)) (date-to-epoch-helper x) (error "Invalid argument, should be of form year month day hour minut secound"))
      )
  )

;find the amount of year since a timestamp
(define (epoc-to-year epoc year)
  (let ((sec (*(*(* (accum-months year 0) 24) 60) 60)  ))
    (if (> sec epoc) (- year 1) (epoc-to-year epoc (+ year 1)))
    ))

;finds the month since a (epoc - year)
(define (epoc-to-month epoc year month)
  (let ((sec (*(*(* (accum-months year month) 24) 60) 60)  ))
    (if (> sec epoc) (- month 1) (epoc-to-month epoc year (+ month 1)))))

;converts from epoc to (year month day hour minuts seconds)
(define (epoc-to-date epoc)
  (let* ((year (epoc-to-year epoc 1970))
         (month (epoc-to-month epoc year 0))
         (secounds (- epoc (date-to-epoch year month 0 0 0 0)))
         (minutes (quotient secounds 60))
         (hour (quotient minutes 60))
         (day (quotient hour 24))
         )
    (list year month day (remainder hour 24) (remainder minutes 60) (remainder secounds 60))
    ))
