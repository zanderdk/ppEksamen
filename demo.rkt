#lang racket/load
(load "time.rkt")
(load "utils.rkt")
(load "calendar.rkt")

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
