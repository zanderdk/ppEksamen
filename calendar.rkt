;constructor procedure for datetime uses time.tks to convert from (year month day ...) 
;to unix timestamp and validates the input
(define (create-datetime x . y)
  (let ((parms (cons x y)))
    (if (= (length parms) 1) (list 'datetime (date-to-epoch (car parms)))
        (if (= (length parms) 6) (list 'datetime (date-to-epoch parms))
            (error "Invalid argument, should be of form year month day hour minut secound"))
        )))

;vaildate input an constructs appointment
(define (create-appointment name description start end)
  (cond ((not (string? name)) (error "name shuld be a string")); error handling
        ((not (string? description)) (error "name shuld be a string"))
        ((datetime-after start end) (error "end datetime must be after start"))
        (else (list 'appointment name description start end))))

;constructs a calendar
;the lst is a list of both appointments and other calendars
(define (create-calendar name . lst)
  (if (not (string? name)) (error "invalid input") 
  (let ((cals (car (split-cal-and-app lst '() '())))
        (apps (car (cdr (split-cal-and-app lst '() '())))))
    (create-calendar-helper name cals apps))))

;sorts appointments and calendars in to two distinct list
(define (split-cal-and-app lst rescal resapp)
  (if (empty? lst) (list rescal resapp)
      (let ((obj (car lst)))
        (split-cal-and-app (cdr lst) (if (calendar? obj) (cons obj rescal) rescal)
                           (if (appointment? obj) (cons obj resapp) resapp)))))

;validates input from create-calendar
(define (create-calendar-helper name calendars appointments)
  (cond ((not (list? calendars)) (error "arguments must be a list of calendars and a list of appointments"))
        ((not (list? appointments)) (error "arguments must be a list of calendars and a list of appointments"))
        ((not (for-all calendar? calendars)) (error "arguments must be a list of calendars and a list of appointments"))
        ((not (for-all appointment? appointments)) (error "arguments must be a list of calendars and a list of appointments"))
        (else (list 'calendar name calendars (sort-appointments appointments))))
  )

;"type checking procedurse"
;they all check for the fist element in a list to be a specific symbol reprecenting the type
(define (datetimetype? date)
  (if (not (list? date)) #f
      (if (not (= (length date) 2)) #f
          (eq? (car date) 'datetime))))

(define (appointment? app)
    (cond ((not (list? app)) #f)
          ((not (eq? (car app) 'appointment)) #f)
          (else #t)))

(define (calendar? cal)
    (cond ((not (list? cal)) #f)
          ((not (= (length cal) 4)) #f)
          ((not (eq? (car cal) 'calendar)) #f)
          (else #t)))

;accessor functions
(define (get-appointment-start app)
  (if (appointment? app) (list-ref app 3) (error "input must be an appointment") ))

(define (get-appointment-end app)
  (if (appointment? app) (list-ref app 4) (error "input must be an appointment") ))

(define (get-appointment-desc app)
  (if (appointment? app) (list-ref app 2) (error "input must be an appointment") ))

(define (get-appointment-name app)
  (if (appointment? app) (list-ref app 1) (error "input must be an appointment") ))

(define (get-appointment-calendar app)
  (if (appointment? app) (list-ref app 5) (error "this calendar has no appointments") ))

(define (get-calendar-name cal)
  (if (calendar? cal)
      (list-ref cal 1)
      (error "invalid input shoud be a calendar"))
  )

(define (get-nested-calenders cal)
  (if (calendar? cal)
      (list-ref cal 2)
      (error "invalid input shoud be a calendar")) 
  )

(define (get-calendar-appointments cal)
  (if (calendar? cal)
      (list-ref cal 3)
      (error "invalid input shoud be a calendar")) 
  )

(define (get-datetime-year-month d)
  (if (not (datetimetype? d)) (error "not a datetime")
      (let ((date (epoc-to-date (list-ref d 1))))
        (list (list-ref date 0) (list-ref date 1))
      )))

;check if start(datetime) is before end(datetime)
(define (datetime-before start end)
    (cond ((not (datetimetype? start)) (error "start is not a datetime"))
          ((not (datetimetype? end)) (error "end is not a datetime"))
        (else (< (- (car (cdr start)) (car (cdr end)) ) 0) )))

;check if start(datetime) is after end(datetime)
(define (datetime-after start end)
    (cond ((not (datetimetype? start)) (error "start is not a datetime"))
          ((not (datetimetype? end)) (error "end is not a datetime"))
        (else (< (- (car (cdr end)) (car (cdr start))) 0))))

;sorts appointments according to the start datetime of appointments using inbuld sort function
(define (sort-appointments appointments)
  (sort appointments (lambda (x y)
        (< (car (cdr (get-appointment-start x))) (car (cdr (get-appointment-start y)))))))

;find functions they uses the general function in utils.rkt
(define (find-appointments cal pred)
  (if (calendar? cal) (find-all pred (get-calendar-appointments cal) '())
      (error "invalid input shoud be a calendar and a predicat")))

(define (find-first-appointment cal pred)
  (if (calendar? cal) (find-first pred (get-calendar-appointments cal))
      (error "invalid input shoud be a calendar and a predicat")))

(define (find-last-appointment cal pred)
  (if (calendar? cal) (find-first pred (reverse (get-calendar-appointments cal)))
      (error "invalid input shoud be a calendar and a predicat")))

;flattens a calendar
(define (flatten-calendar cal)
  (let ((apps  (appointment-flatten (all-appointments cal)))) ;flattens list of list of list... of appointments
    (create-calendar-helper (get-calendar-name cal) '() apps) ;creates a new calendar based on the flattened appointments
    )
  )

;get all appointments in a calendar also nested
(define (all-appointments cal)
  (let ((op (lambda (x)  (reverse (cons (get-calendar-name cal) (reverse x))) )))
  (if (empty? (get-nested-calenders cal))
      (map op (get-calendar-appointments cal))
      (cons (map op (get-calendar-appointments cal)) (map all-appointments (get-nested-calenders cal )))
       )))

;checks for overlapping appointments by looking at the first start (datetime)
(define (appointments-overlap? ap1 ap2)
  (letrec ((sorted (sort-appointments (list ap1 ap2))); sorting to find fist start datetime
           (app1 (car sorted))
           (app2 (car (cdr sorted))))
  (datetime-before (get-appointment-start app2) (get-appointment-end app1)));check for over lap between start and end time
  )

(define (calendars-overlap? cal1 cal2)
  (letrec ((cal1-apps (get-calendar-appointments cal1))
           (cal2-apps (get-calendar-appointments cal2))
           (op (lambda (x y) (or x y))); my scheme system can't do (map or someList)!! wrap it to a lambda :-)
           (app-overlap (lambda (app lst)
         (fold #f op (map (lambda (x) (appointments-overlap? app x)) lst) )))); make lambda that checks if one appointment in cal1 overlaps with any appointment in cal2
    (fold #f op (map (lambda (x) (app-overlap x cal2-apps)) cal1-apps)); run lambda over all appointments in cal1
    ))

;finds distinc (year month) tupples in list of appointments start dates
(define (find-distinct-months-in-appointment-list apps res)
  (letrec ((get-month (lambda (a) (get-datetime-year-month (get-appointment-start a)) )))
  (if (empty? apps) res
      (find-distinct-months-in-appointment-list (cdr apps)
           (if (exists (get-month (car apps)) res) res (cons (get-month (car apps)) res))))))

;group appointments by (month year) based on start datetime
;return value: distinct list of ((month year), (list of appointments)) go go tupple notation!
(define (group-appointments-by-year-month apps)
  (let ((months (find-distinct-months-in-appointment-list apps '())))
    (reverse (map (lambda (x)
           (list x (find-all (lambda (app) (equal? (get-datetime-year-month (get-appointment-start app)) x)) apps '()))
           ) months))
  ))

;check if a datetime d is between start and end. doh
(define (datetime-in-timespan d start end)
  (and (datetime-before d end) (datetime-after d start))
  )

;filtes appointment list apps for appointments with in timespan
(define (appointments-in-timespan apps start end)
  (let ((pred (lambda (app) (datetime-in-timespan (get-appointment-start app) start end) )))
    (find-all pred apps '())
  ))

;group all (also nested)appointments in a calendar within a given timespan into ((month year), appointmentlist)
(define (group-calendar-appointments cal start end)
  (let ((apps (get-calendar-appointments (flatten-calendar cal))))
    (group-appointments-by-year-month (appointments-in-timespan apps start end))
    ))

;flatten list of appointments
(define (appointment-flatten apps)
  (cond ((empty? apps) apps)
        ((appointment? apps) (list apps))
        ((list? apps) (append (appointment-flatten (car apps)) (appointment-flatten (cdr apps))))))

;convers a cal to html
(define (present-calendar-html cal from-time to-time)
  (let ((apps (group-calendar-appointments cal from-time to-time)))
        (fold-string (list "<html><body>" (fold-string (map html-table apps)) "</body></html>")))
  )

;nice html-tag function is futhere descriped in report
(define (html-tag tag . parms)
  (letrec ((attributes (cdr (reverse parms)) )
           (string-attributes (map (lambda (x)
                                     (fold-string (list (list-ref x 0) "='" (list-ref x 0) "' "))) attributes))
           (end-tag (fold-string (list "</" tag ">")))
           (start-tag (fold-string (list "<" tag " " (fold-string string-attributes) ">")))
           (element (car (reverse parms))))
  (fold-string (list start-tag element end-tag))
  ))

;converst an appointment in html using nice function above
(define (appointment-to-html ap)
  (fold-string (list "<tr>" (html-tag "td" (get-appointment-name ap))
        (html-tag "td" (get-appointment-desc ap))
        (datetime-to-html (get-appointment-start ap))
        (datetime-to-html (get-appointment-end ap))
        (html-tag "td" (get-appointment-calendar ap) ) "</tr>" ))
  )

;fex 0 returns Jaunar
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

;converts a ((year, month) appointment list) to html
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

;convert list of appointments to html
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
