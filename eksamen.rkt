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

(define (datetime-before start end)
    (cond ((not (datetimetype? start)) (error "start is not a datetime"))
          ((not (datetimetype? end)) (error "end is not a datetime"))
        (else (< (- (car (cdr start)) (car (cdr end)) ) 0) )))

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
          ((not (eq? (car app) 'appointment)) #f)
          (else #t)))

(define (get-appointment-start app)
  (if (appointment? app) (list-ref app 3) (error "input must be an appointment") )
  )

(define (get-appointment-end app)
  (if (appointment? app) (list-ref app 4) (error "input must be an appointment") )
  )

(define (get-appointment-desc app)
  (if (appointment? app) (list-ref app 2) (error "input must be an appointment") )
  )

(define (get-appointment-name app)
  (if (appointment? app) (list-ref app 1) (error "input must be an appointment") )
  )

(define (get-appointment-calendar app)
  (if (appointment? app) (list-ref app 5) (error "this calendar has no appointments") )
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
  (if (not (string? name)) (error "invalid input") 
  (let ((cals (car (split-cal-and-app lst '() '())))
        (apps (car (cdr (split-cal-and-app lst '() '())))))
    (create-calendar-helper name cals apps)
    )))

(define (get-calendar-appointments cal)
  (if (calendar? cal)
      (list-ref cal 3)
      (error "ivalid input should be a calendar")))

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
    (create-calendar-helper (list-ref cal 1) '() apps) ;CHECK DET HER!!!!
    )
  )

(define (get-calendar-name cal)
  (if (calendar? cal)
      (list-ref cal 1)
      (error "invalid input shoud be a calendar"))
  )

(define (get-calendar-appointments cal)
  (if (calendar? cal)
      (list-ref cal 3)
      (error "invalid input shoud be a calendar")) 
  )

(define (all-appointments cal)
  (let ((op (lambda (x)  (reverse (cons (get-calendar-name cal) (reverse x))) )))
  (if (empty? (list-ref cal 2))
      (map op (get-calendar-appointments cal))
      (cons (map op (get-calendar-appointments cal)) (map all-appointments (list-ref cal 2)))
       )))

(define (appointments-overlap? ap1 ap2)
  (letrec ((sorted (sort-appointments (list ap1 ap2)))
           (app1 (car sorted))
           (app2 (car (cdr sorted))))
  (datetime-before (get-appointment-start app2) (get-appointment-end app1)))
  )

(define (calendars-overlap? cal1 cal2)
  (letrec ((cal1-apps (get-calendar-appointments cal1))
           (cal2-apps (get-calendar-appointments cal2))
           (op (lambda (x y) (or x y)))
           (app-overlap (lambda (app lst)
         (fold #f op (map (lambda (x) (appointments-overlap? app x)) lst) ))))
    (fold #f op (map (lambda (x) (app-overlap x cal2-apps)) cal1-apps))
    ))

(define (exists a lst)
  (if (equal? (find-first (lambda (x) (equal? a x)) lst) 'None) #f #t))

(define (get-datetime-year-month d)
  (if (not (datetimetype? d)) (error "not a datetime")
      (let ((date (epoc-to-date (list-ref d 1))))
        (list (list-ref date 0) (list-ref date 1))
      )))

(define (find-distinct-months-in-appointment-list apps res)
  (letrec ((get-month (lambda (a) (get-datetime-year-month (get-appointment-start a)) )))
  (if (empty? apps) res
      (find-distinct-months-in-appointment-list (cdr apps)
           (if (exists (get-month (car apps)) res) res (cons (get-month (car apps)) res))))))

(define (group-appointments-by-year-month apps)
  (let ((months (find-distinct-months-in-appointment-list apps '())))
    (reverse (map (lambda (x)
           (list x (find-all (lambda (app) (equal? (get-datetime-year-month (get-appointment-start app)) x)) apps '()))
           ) months))
  ))

(define (datetime-in-timespan d start end)
  (and (datetime-before d end) (datetime-after d start))
  )

(define (appointments-in-timespan apps start end)
  (let ((pred (lambda (app) (datetime-in-timespan (get-appointment-start app) start end) )))
    (find-all pred apps '())
  ))

(define (group-calendar-appointments cal start end)
  (let ((apps (get-calendar-appointments (flatten-calendar cal))))
    (group-appointments-by-year-month (appointments-in-timespan apps start end))
    ))

(define (appointment-flatten apps)
  (cond ((empty? apps) apps)
        ((appointment? apps) (list apps))
        ((list? apps) (append (appointment-flatten (car apps)) (appointment-flatten (cdr apps))))))

(define (fold-string lst)
  (fold "" string-append lst))

(define (html-tag tag . parms)
  (letrec ((attributes (cdr (reverse parms)) )
           (string-attributes (map (lambda (x)
                                     (fold-string (list (list-ref x 0) "='" (list-ref x 0) "' "))) attributes))
           (end-tag (fold-string (list "</" tag ">")))
           (start-tag (fold-string (list "<" tag " " (fold-string string-attributes) ">")))
           (element (car (reverse parms))))
  (fold-string (list start-tag element end-tag))
  ))

(define (appointment-to-html ap)
  (fold-string (list "<tr>" (html-tag "td" (get-appointment-name ap))
        (html-tag "td" (get-appointment-desc ap))
        (datetime-to-html (get-appointment-start ap))
        (datetime-to-html (get-appointment-end ap))
        (html-tag "td" (get-appointment-calendar ap) ) "</tr>" ))
  )

(define (number->month nr)
  (cond ((not (integer? nr)) (error "shuld be a integer"))
         ((= nr 0) "Januar")
         ((= nr 1) "Feburary")
         ((= nr 2) "Marts")
         ((= nr 3) "April")
         ((= nr 4) "May")
         ((= nr 5) "June")
         ((= nr 6) "July")
         ((= nr 7) "May")
         ((= nr 8) "August")
         ((= nr 9) "September")
         ((= nr 10) "November")
         ((= nr 10) "December"))
  )

(define (present-calendar-html cal from-time to-time)
  (let ((apps (group-calendar-appointments cal from-time to-time)))
        (fold-string (list "<html><body>" (fold-string (map html-table apps)) "</body></html>")))
  )

(define (html-table month)
  (let ((table-title (fold-string (list "<tr>" "<th>" "Appointments for "
                           (number->month (list-ref (car month) 1))
                           " in " (number->string (list-ref (car month) 0)) "</th>" "</tr>" )))
        (headers "<tr><th>Name</th><th>Description</th><th>From time</th><th>To time</th><th>Calendar</th></tr>")
        (table-tag-start "<table style='width:100%' border='1'>")
        (table-tag-end "</table><br/>")
        (app (appointments-to-html (list-ref month 1))))
    (fold-string (list table-tag-start table-title headers app table-tag-end)))
 )

(define (appointments-to-html apps)
  (fold-string (list (fold-string (map appointment-to-html apps)) ))
  )

(define (datetime-to-str d)
  (letrec ((date (epoc-to-date (list-ref d 1)))
        (strings (list (number->string (list-ref date 0)) "/" (number->string (list-ref date 1)) "/"
                       (number->string (list-ref date 2)) " - " (number->string (list-ref date 3)) ":"
                       (number->string (list-ref date 4)) ":" (number->string (list-ref date 5))) ))
    (fold-string strings))
  )

(define (datetime-to-html d)
  (html-tag "td" (datetime-to-str d))
  )

(define app1 (create-appointment "first" "test description" (create-datetime 2013 0 0 0 0 0) (create-datetime 2014 0 0 0 0 0)))
(define app2 (create-appointment "secound" "test description" (create-datetime 2015 0 0 0 0 0) (create-datetime 2016 0 0 0 0 0)))
(define app3 (create-appointment "last" "test description" (create-datetime 2017 0 0 0 0 0) (create-datetime 2018 0 0 0 0 0)))

(define app-overlap1 (create-appointment "app-overlap1" "overlaps with app-overlap2" (create-datetime 2016 8 26 7 0 0) (create-datetime 2016 8 26 9 0 0)))
(define app-overlap2 (create-appointment "app-overlap2" "overlaps with app-overlap1" (create-datetime 2016 8 26 8 0 0) (create-datetime 2016 8 26 10 0 0)))

(define testCal1 (create-calendar-helper "test cal" '() (list app3 app2 app1)))
(define testCal2 (create-calendar "test cal2" testCal1 app2 app1))

(define overlappingCal1 (create-calendar "overlapCal1" app1 app-overlap1))
(define overlappingCal2 (create-calendar "overlapCal2" app2 app-overlap2))

(define ppEksamenApp1 (create-appointment "pp miniprojekt" "pp onsdag" (create-datetime 2016 9 25 7 0 0) (create-datetime 2016 9 25 15 0 0)))
(define ppEksamenApp2 (create-appointment "pp miniprojekt" "pp torsdag" (create-datetime 2016 9 26 7 0 0) (create-datetime 2016 9 26 15 0 0)))
(define ppEksamenApp3 (create-appointment "pp miniprojekt" "pp fredag" (create-datetime 2016 9 27 7 0 0) (create-datetime 2016 9 27 15 0 0)))

(define ppCal (create-calendar "pp calendar" ppEksamenApp1 ppEksamenApp2 ppEksamenApp3))

(define d2016 (create-datetime 2016 0 0 0 0 0))

(define d2017 (create-datetime 2017 0 0 0 0 0))

(define htmlCal (create-calendar "html test calender" overlappingCal1 overlappingCal2 ppCal))