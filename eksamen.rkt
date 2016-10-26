#lang racket/load
(load "time.rkt")

(define (create-datetime x . y)
  (let ((parms (cons x y)))
    (if (= (length parms) 1) (list 'datetimetype (date-to-epoch (car parms)))
        (if (= (length parms) 6) (list 'datetimetype (date-to-epoch parms))
            (error "Invalid argument, should be of form year month day hour minut secound"))
        )))

(define (datetimetype? date)
  (if (not (list? date)) #f
      (if (not (= (length date) 2)) #f
          (eq? (car date) 'datetimetype)
          )))

(define (datetime-after start end)
    (cond ((not (datetimetype? start)) (error "start is not a datetime"))
          ((not (datetimetype? end)) (error "end is not a datetime"))
        (else (< (- (car (cdr end)) (car (cdr start))) 0))))

(define (create-appointment name description start end)
  (cond ((not (string? name)) (error "name shuld be a string"))
        ((not (string? description)) (error "name shuld be a string"))
        ((datetime-after start end) (error "end datetime must be after start"))
        (else (list 'appointment name description start end))))

(define (appointment? app)
    (cond ((not (list? app)) #f)
          ((not (and (>= (length app) 5) (<= (length app) 6))) #f)
          ((not (eq? (car app) 'appointment)) #f)
          (else #t))
  )

(define (calendar? cal)
    (cond ((not (list? cal)) #f)
          ((not (= (length cal) 4)) #f)
          ((not (eq? (car cal) 'calendar)) #f)
          (else #t))
  )

(define (split-cal-and-app lst rescal resapp)
  (if (empty? lst) (list rescal resapp)
      (let ((obj (car lst)))
        (split-cal-and-app (cdr lst) (if (calendar? obj) (cons obj rescal) rescal)
                           (if (appointment? obj) (cons obj resapp) resapp)))))

(define (create-calendar name . lst)
  (let ((cals (car (split-cal-and-app lst '() '())))
        (apps (car (cdr (split-cal-and-app lst '() '())))))
    (create-calendar-helper name cals apps)
    ))

(define (create-calendar-helper name calendars appointments)
  (cond ((not (list? calendars)) (error "arguments must be a list of calendars and a list of appointments"))
        ((not (list? appointments)) (error "arguments must be a list of calendars and a list of appointments"))
        ((not (for-all calendar? calendars)) (error "arguments must be a list of calendars and a list of appointments"))
        ((not (for-all appointment? appointments)) (error "arguments must be a list of calendars and a list of appointments"))
        (else (list 'calendar name calendars (sort-appointments appointments))))
  )

(define (for-all pred lst)
  (fold #t (lambda (x y) (and x y)) (map pred lst)))

(define (fold init op lst)
  (if (= (length lst) 0) init
      (fold (op init (car lst)) op (cdr lst))))

(define (sort-appointments appointments)
  (sort appointments (lambda (x y)
        (< (car (cdr (list-ref x 3))) (car (cdr (list-ref y 3)))))))

(define (find-appointments cal pred)
  (if (calendar? cal) (find-all pred (list-ref cal 3) '())
      (error "invalid input shoud be a calendar and a predicat")))

(define (find-first-appointment cal pred)
  (if (calendar? cal) (find-first pred (list-ref cal 3))
      (error "invalid input shoud be a calendar and a predicat")))

(define (find-last-appointment cal pred)
  (if (calendar? cal) (find-first pred (reverse (list-ref cal 3)))
      (error "invalid input shoud be a calendar and a predicat")))

(define (find-first pred lst)
  (if (empty? lst) 'None
      (if (pred (car lst)) (car lst) (find-first pred (cdr lst)) ) 
      ))

(define (find-all pred lst res)
  (if (empty? lst) res
      (reverse (find-all pred (cdr lst) (if (pred (car lst)) (cons (car lst) res) res )))))

(define (flatten-calendar cal)
  (let ((apps  (appointment-flatten (all-appointments cal))))
    (create-calendar-helper (list-ref cal 1) '() apps)
    )
  )

(define (all-appointments cal)
  (if (empty? (list-ref cal 2))
      (list-ref cal 3)
      (cons (list-ref cal 3) (map all-appointments (list-ref cal 2)))
       ))

(define (appointments-overlap? ap1 ap2)
  ap1
  )


(define (appointment-flatten apps)
  (cond ((empty? apps) apps)
        ((appointment? apps) (list apps))
        ((list? apps) (append (appointment-flatten (car apps)) (appointment-flatten (cdr apps))))))

(define app1 (create-appointment "first" "test description" (create-datetime 2013 0 0 0 0 0) (create-datetime 2014 0 0 0 0 0)))
(define app2 (create-appointment "secound" "test description" (create-datetime 2015 0 0 0 0 0) (create-datetime 2016 0 0 0 0 0)))
(define app3 (create-appointment "last" "test description" (create-datetime 2017 0 0 0 0 0) (create-datetime 2018 0 0 0 0 0)))

(define app-overlap1 (create-appointment "app-overlap1" "overlaps with app-overlap2" (create-datetime 2016 9 26 7 0 0) (create-datetime 2016 9 26 9 0 0)))
(define app-overlap2 (create-appointment "app-overlap2" "overlaps with app-overlap1" (create-datetime 2016 9 26 8 0 0) (create-datetime 2016 9 26 10 0 0)))

(define testCal1 (create-calendar-helper "test cal" '() (list app3 app2 app1)))
(define testCal2 (create-calendar "test cal2" testCal1 app2 app1))