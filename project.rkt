#lang typed/racket

(require "./include/cs151-core.rkt")
(require "./include/cs151-image.rkt")
(require "./include/cs151-universe.rkt")
  
(require typed/test-engine/racket-tests)
(require typed/racket/date)

;; == Data Definitions =========================================================
(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-struct CalFormat
  ([cell-size : Integer]
   [title-bar-bg : Image-Color]
   [title-bar-font : Image-Color]
   [title-bar-height : Integer]
   [day-label-bg : Image-Color]
   [day-label-font : Image-Color]
   [day-label-height : Integer]
   [cell-bg : Image-Color]
   [cell-font : Image-Color]))

(define-struct Time
  ([hour : Integer] ;; from 0 to 23
   [minute : Integer]
   [second : Integer]))

(define-struct Date
  ([month : Integer] ;; 1 for January, ..., 12 for December
   [day : Integer]
   [year : Integer]))

(define-struct Span
  ([start : Time]
   [end : Time]))

(define-struct Event
  ([date : Date]
   [time : (U 'all-day Time Span)]
   [description : String]))

(define-type EventTree
  (U 'Empty EventNode))

(define-struct EventNode
  ([date : Date]
   [events : (Listof Event)] ;; maintain this list in ascending order
   [lsub : EventTree]
   [rsub : EventTree]))

(define-struct CalWorld3
  ([mode : (U 'calendar 'help 'entry)]
   [entry-mode : (U 'start 'end 'description)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date]
   [now-date-string : String]
   [now-time : Time]
   [notepad : String]
   [opt-start : (Optional Time)]
   [opt-end : (Optional Time)]
   [events : EventTree]))

(define-struct Day-Of-Week
  ([num : Integer])) ;; 0 Sun, 6 Sat

;; == Testing Variables ========================================================
(: fmt0 CalFormat)
(define fmt0
  (CalFormat 40
             'dodgerblue 'lightyellow 60
             'silver 'blue 30
             'lightyellow 'black))

(: fmt1 CalFormat)
(define fmt1
  (CalFormat 80
             'dodgerblue 'navy 60
             'silver 'blue 40
             'lightblue 'black))

(define fmt2
  (CalFormat 60
             'darkred 'white 60
             'silver 'darkred 30
             'dimgray 'white))

; == Date Functions ============================================================

(: read-date-now : -> Date)
;; return the current date as a Date
(define (read-date-now)
  (Date (date-month (current-date))
        (date-day (current-date))
        (date-year (current-date))))
;; Checks
"read-date-now eyeball check"
(read-date-now)

(: read-time-now : -> Time)
;; return the current time as a Time
(define (read-time-now)
  (Time (date-hour (current-date))
        (date-minute (current-date))
        (date-second (current-date))))
;; Checks
"read-time-now eyeball check"
(read-time-now)

(: leap? (-> Integer Boolean))
;; Returns true if a given year is a leap year
(define (leap? year)
  (or (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))
      (= (modulo year 400) 0)))
;; Checks
(check-expect (leap? 2018) #f)
(check-expect (leap? 2000) #t)
(check-expect (leap? 2200) #f)
(check-expect (leap? 2020) #t)

(: days-in-month (-> Integer Integer Integer))
;; Returns number of days in a given month
(define (days-in-month month year)
  (match* (month (leap? year))
    [(2 #t) 29]
    [(2 #f) 28]
    [(4 _) 30]
    [(6 _) 30]
    [(9 _) 30]
    [(11 _) 30]
    [(_ _) 31]))
;; Checks
(check-expect (days-in-month 2 2020) 29)
(check-expect (days-in-month 2 2021) 28)
(check-expect (days-in-month 1 2020) 31)
(check-expect (days-in-month 11 2020) 30)

(: date=? (-> Date Date Boolean))
;; Returns true if the dates are equal and false if they are not
(define (date=? t1 t2)
  (match* ((Date-day t1) (Date-day t2) (Date-month t1)
                         (Date-month t2) (Date-year t1) (Date-year t2))
    [(x x y y z z) #t]
    [(_ _ _ _ _ _) #f]))
;; Checks
(check-expect (date=? (Date 1 2 3) (Date 3 2 1)) #f)
(check-expect (date=? (Date 1 2 3) (Date 1 2 3)) #t)

(: days-after (-> Day-Of-Week Integer Day-Of-Week))
;; Returns the Day-Of-Week num days after teh given Day-Of-Week
(define (days-after start num)
  (Day-Of-Week (modulo (+ (Day-Of-Week-num start) num) 7)))
;; Checks
(check-expect (days-after (Day-Of-Week 0) 7) (Day-Of-Week 0))
(check-expect (days-after (Day-Of-Week 0) 9) (Day-Of-Week 2))
(check-expect (days-after (Day-Of-Week 4) 24) (Day-Of-Week 0))

(: date<? (-> Date Date Boolean))
;; Returns true if the dates are in ascending order and false otherwise
(define (date<? d1 d2)
  (cond [(< (Date-year d1) (Date-year d2)) #t]
        [(> (Date-year d1) (Date-year d2)) #f]
        [(< (Date-month d1) (Date-month d2)) #t]
        [(> (Date-month d1) (Date-month d2)) #f]
        [(< (Date-day  d1) (Date-day  d2)) #t]
        [else #f]))
;; Checks
(check-expect (date<? (Date 1 1 2000) (Date 1 2 2000)) #t)
(check-expect (date<? (Date 1 2 2000) (Date 1 2 2000)) #f)
(check-expect (date<? (Date 1 2 2000) (Date 1 2 1999)) #f)
(check-expect (date<? (Date 1 2 2000) (Date 4 9 2001)) #t)

(: time=? (-> Time Time Boolean))
;; returns true if they are the same time
(define (time=? t1 t2)
  (match* (t1 t2)
    [((Time h1 m1 s1) (Time h2 m2 s2)) (and (= h1 h2) (= m1 m2) (= s1 s2))]))
;; Checks
(check-expect (time=? (Time 0 0 0) (Time 0 0 0)) #t)
(check-expect (time=? (Time 2 1 3) (Time 2 1 3)) #t)
(check-expect (time=? (Time 0 0 0) (Time 1 0 0)) #f)
(check-expect (time=? (Time 2 1 3) (Time 3 1 2)) #f)

(: time<? (-> Time Time Boolean))
;; Returns true if the dates are ascending and not equal
(define (time<? t1 t2)
  (cond
    [(> (Time-hour t1) (Time-hour t2)) #f]
    [(< (Time-hour t1) (Time-hour t2)) #t]
    [(> (Time-minute t1) (Time-minute t2)) #f]
    [(< (Time-minute t1) (Time-minute t2)) #t]
    [(> (Time-second t1) (Time-second t2)) #f]
    [(< (Time-second t1) (Time-second t2)) #t]
    [else #f]))
;; Checks
(check-expect (time<? (Time 0 0 0) (Time 1 2 3)) #t)
(check-expect (time<? (Time 1 1 1) (Time 1 1 1)) #f)
(check-expect (time<? (Time 12 3 0) (Time 13 1 0)) #t)
(check-expect (time<? (Time 13 1 0) (Time 14 2 0)) #t)
(check-expect (time<? (Time 1 2 0) (Time 1 3 1)) #t)
(check-expect (time<? (Time 2 1 12) (Time 0 0 13)) #f)

(: add-month : Date -> Date)
;; Takes a date and returns the date one month later
(define (add-month t)
  (match t
    [(Date 12 d y) (Date 1 d (+ y 1))]
    [(Date m d y) (if (< (days-in-month (+ 1 m) y) d)
                      (Date (+ 1 m) (days-in-month (+ 1 m) y) y)
                      (Date (+ 1 m) d y))]))
;; Checks
(check-expect (add-month (Date 1 31 2000)) (Date 2 29 2000))
(check-expect (add-month (Date 1 23 2000)) (Date 2 23 2000))
(check-expect (add-month (Date 12 1 2020)) (Date 1 1 2021))

(: subtract-month : Date -> Date)
;; Takes a date and returns the date one month later
(define (subtract-month t)
  (match t
    [(Date 1 d y) (Date 12 d (- y 1))]
    [(Date m d y) (if (< (days-in-month (- m 1) y) d)
                      (Date (- m 1) (days-in-month (- m 1) y) y)
                      (Date (- m 1) d y))]))
;; Checks
(check-expect (subtract-month (Date 3 31 2000)) (Date 2 29 2000))
(check-expect (subtract-month (Date 3 23 2000)) (Date 2 23 2000))
(check-expect (subtract-month (Date 1 1 2020)) (Date 12 1 2019))

(: add-year : Date -> Date)
;; Takes a date and returns the date a year later
(define (add-year t)
  (match t
    [(Date 2 29 y) (Date 3 1 (+ y 1))]
    [(Date m d y) (Date m d (+ y 1))]))
;; Checks
(check-expect (add-year (Date 2 29 2020)) (Date 3 1 2021))
(check-expect (add-year (Date 2 28 2020)) (Date 2 28 2021))

(: subtract-year : Date -> Date)
;; Takes a date and returns the date a year later
(define (subtract-year t)
  (match t
    [(Date 2 29 y) (Date 3 1 (- y 1))]
    [(Date m d y) (Date m d (- y 1))]))
;; Checks
(check-expect (subtract-year (Date 2 29 2020)) (Date 3 1 2019))
(check-expect (subtract-year (Date 2 28 2020)) (Date 2 28 2019))

(: tomorrow : Date -> Date)
;; returns the date of tomorrow
(define (tomorrow t)
  (match t
    [(Date 12 31 y) (Date 1 1 (+ 1 y))]
    [(Date m d y) (if (= (Date-day t)
                         (days-in-month (Date-month t) (Date-year t)))
                      (Date (+ m 1) 1 y)
                      (Date m (+ d 1) y))]))
;; Checks
(check-expect (tomorrow (Date 2 29 2020)) (Date 3 1 2020))
(check-expect (tomorrow (Date 5 31 2021)) (Date 6 1 2021))
(check-expect (tomorrow (Date 12 31 1990)) (Date 1 1 1991))
(check-expect (tomorrow (Date 10 2 1234)) (Date 10 3 1234))

(: yesterday : Date -> Date)
;; returns the date of yesterday
(define (yesterday t)
  (match t
    [(Date 1 1 y) (Date 12 31 (- y 1))]
    [(Date m 1 y) (Date (- m 1) (days-in-month (- m 1) y) y)]
    [(Date m d y) (Date m (- d 1) y)]))
;; Checks
(check-expect (yesterday (Date 3 1 2020)) (Date 2 29 2020))
(check-expect (yesterday (Date 6 1 2021)) (Date 5 31 2021))
(check-expect (yesterday (Date 1 1 1990)) (Date 12 31 1989))
(check-expect (yesterday (Date 10 2 1234)) (Date 10 1 1234))

(: doomsday-key (-> Integer Integer Integer))
;; Returns the doomsday of a year as an integer given the century code and year
(define (doomsday-key key y)
  (define r (modulo y 100))  
  (modulo (+ key r (quotient r 4)) 7))

(: doomsday-in-year (-> Integer Integer))
;; Returns the day of the week of the doomsday of a year given the year
(define (doomsday-in-year y)
  (match (modulo (quotient y 100) 4)
    [0 (doomsday-key 2 y)]
    [1 (doomsday-key 0 y)]
    [2 (doomsday-key 5 y)]
    [3 (doomsday-key 3 y)]))
;; Check
(check-expect (doomsday-in-year 2004) 0)
(check-expect (doomsday-in-year 1900) 3)
(check-expect (doomsday-in-year 2150) 6)

(: doomsday-in-month (-> Integer Integer Integer))
;; Returns the doomsday of the given month and year
(define (doomsday-in-month m y)
  (cond [(and (not(= m 2)) (= (modulo m 2) 0)) m]
        [else (match m
                [1 (if [leap? y] 32 31)]
                [2 (if [leap? y] 29 28)]
                [3 0]
                [5 9]
                [7 11]
                [9 5]
                [11 7])]))
;; Checks
(check-expect (doomsday-in-month 1 2020) 32)
(check-expect (doomsday-in-month 1 2019) 31)
(check-expect (doomsday-in-month 2 2020) 29)
(check-expect (doomsday-in-month 2 2019) 28)
(check-expect (doomsday-in-month 3 2020) 0)
(check-expect (doomsday-in-month 7 2020) 11)
(check-expect (doomsday-in-month 6 2020) 6)
(check-expect (doomsday-in-month 8 2020) 8)

(: find-day-of-week (-> Date Day-Of-Week))
;; Returns the day of the week as a Day-Of-Week object given a Date object
(define (find-day-of-week t)
  (days-after (Day-Of-Week (doomsday-in-year (Date-year t)))
              (- (Date-day t)
                 (doomsday-in-month (Date-month t) (Date-year t)))))
;; Checks
(check-expect (find-day-of-week (Date 7 4 1776)) (Day-Of-Week 4))
(check-expect (find-day-of-week (Date 2 29 2020)) (Day-Of-Week 6))
(check-expect (find-day-of-week (Date 4 4 2020)) (Day-Of-Week 6))
(check-expect (find-day-of-week (Date 2 28 2100)) (Day-Of-Week 0))

(: labor-day : Integer -> Date)
;; returns the date of labor day for a given year
(define (labor-day y)
  (define days (filter (lambda ([x : Date])
                         (= (Day-Of-Week-num (find-day-of-week x)) 1))
                       (map (lambda ([x : Integer]) (Date 9 x y))
                            (build-list 7 (lambda ([x : Integer]) (+ 1 x))))))
  (list-ref days 0))
;; Checks
(check-expect (labor-day 2100) (Date 9 6 2100))
(check-expect (labor-day 2098) (Date 9 1 2098))

(: memorial-day : Integer -> Date)
;; returns the date of memorial day for a given year
(define (memorial-day y)
  (define days (filter (lambda ([x : Date])
                         (= (Day-Of-Week-num (find-day-of-week x)) 1))
                       (map (lambda ([x : Integer]) (Date 5 x y))
                            (build-list 31 (lambda ([x : Integer]) (+ 1 x))))))
  (list-ref days (- (length days) 1)))
;; Checks
(check-expect (memorial-day 2099) (Date 5 25 2099))
(check-expect (memorial-day 2100) (Date 5 31 2100))

(: thanksgiving : Integer -> Date)
;; returns the date of Thanksgiving for a given year
(define (thanksgiving y)
  (define days (filter (lambda ([x : Date])
                         (= (Day-Of-Week-num (find-day-of-week x)) 4))
                       (map (lambda ([x : Integer]) (Date 11 x y))
                            (build-list 31 (lambda ([x : Integer]) (+ 1 x))))))
  (list-ref days 3))
;; Checks
(check-expect (thanksgiving 2096) (Date 11 22 2096))
(check-expect (thanksgiving 2104) (Date 11 27 2104))


;; Event Definitions ===========================================================
(: char->int : (Listof Char) -> Integer)
;; converts a character to an integer
(define (char->int c)
  (cast (string->number (list->string c)) Integer))
;; Checks
(check-expect (char->int (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\0)) 12345670)
(check-expect (char->int (list #\0)) 0)
(check-expect (char->int (list #\2)) 2)
(check-expect (char->int (list #\0 #\3)) 3)

(: smart-time-char : Integer Integer Integer Char -> (Optional Time))
;; creates a (Some Time) object if the Time is legitimate, but 'None if not
(define (smart-time-char h m s t)
  (if
   (cond
     [(or (< h 0) (> h 12)) #f]
     [(or (< m 0) (> m 60)) #f]
     [(or (< s 0) (> s 60)) #f]
     [else #t])
   (match t
     [#\p (Some (Time (if (= h 12) 12 (+ h 12)) m s))]
     [#\a (Some (Time (if (= h 12) 0 h) m s))]
     [_ 'None])
   'None))
;; Checks
(check-expect (smart-time-char 0 0 0 #\a) (Some (Time 0 0 0)))
(check-expect (smart-time-char 13 0 0 #\p) 'None)
(check-expect (smart-time-char 1 61 0 #\a) 'None)
(check-expect (smart-time-char 1 -1 0 #\p) 'None)
(check-expect (smart-time-char 1 1 62 #\a) 'None)
(check-expect (smart-time-char 1 20 0 #\a) (Some (Time 1 20 0)))
(check-expect (smart-time-char 1 20 0 #\p) (Some (Time 13 20 0)))
(check-expect (smart-time-char 12 23 04 #\a) (Some (Time 0 23 4)))
(check-expect (smart-time-char 12 23 4 #\p) (Some (Time 12 23 4)))
(check-expect (smart-time-char 4 0 0 #\p) (Some (Time 16 0 0)))
(check-expect (smart-time-char 4 0 0 #\a) (Some (Time 4 0 0)))

(: date->string : Date -> String)
;; Takes a date object and returns a string with the day of week,
;; day, month, and year
(define (date->string d)
  (string-append
   (day-name (find-day-of-week d))
   ", " (month-name (Date-month d))
   " " (number->string (Date-day d))
   ", " (number->string (Date-year d))))
;; Checks
(check-expect (date->string (Date 3 16 2021)) "Tuesday, March 16, 2021")
;; Eyeball checks when running world

(: time->string : (Optional Time) -> String)
;; takes an optional time and returns an empty string if it is 'None,
;; and a string with the time data otherwise
(define (time->string t)
  (match t
    ['None ""]
    [(Some (Time h m s))
     (string-append (number->string (cond
                                      [(= h 0) 12]
                                      [(> h 12) (- h 12)]
                                      [else h]))
                    ":" (if (< m 10) (string-append "0" (number->string m))
                            (number->string m))
                    (if (>= h 12) "pm" "am"))]))
;; Checks
(check-expect (time->string (Some (Time 0 0 0))) "12:00am")
(check-expect (time->string 'None) "")
(check-expect (time->string 'None) "")
(check-expect (time->string 'None) "")
(check-expect (time->string 'None) "")
(check-expect (time->string (Some (Time 1 20 0))) "1:20am")
(check-expect (time->string (Some (Time 13 20 0))) "1:20pm")
(check-expect (time->string (Some (Time 0 23 4))) "12:23am")
(check-expect (time->string (Some (Time 12 23 4))) "12:23pm")
(check-expect (time->string (Some (Time 16 0 0))) "4:00pm")
(check-expect (time->string (Some (Time 4 0 0))) "4:00am")

(: string->time : String -> (Optional Time))
;; convert a string like "11:25am" to a Time struct, if possible
;; ex: (string->time "10:00am") -> (Some (Time 10 0 0))
;; ex: (string->time "4:30pm")  -> (Some (Time 16 30 0))
;; ex: (string->time "abcde")   -> 'None
(define (string->time s)
  (match (string->list (match (length (string->list s))
                         [6 (string-append "0" s)]
                         [_ s]))
    [(list h1 h2 #\: m1 m2 t _) (smart-time-char
                                 (char->int (list h1 h2))
                                 (char->int (list m1 m2)) 0 t)]
    [_ 'None]))
;; Checks
(check-expect
 (map string->time
      (build-list 12 (lambda ([x : Integer])
                       (string-append (number->string x) ":00am"))))
 (build-list 12 (lambda ([x : Integer])
                  (Some (Time x 0 0)))))
(check-expect
 (map string->time
      (build-list 12 (lambda ([x : Integer])
                       (string-append (number->string x) ":00pm"))))
 (build-list 12 (lambda ([x : Integer])
                  (Some (Time (+ 12 x) 0 0)))))
(check-expect (string->time "9am") 'None)
(check-expect (string->time "8:12pm") (Some (Time 20 12 0)))
(check-expect (string->time "19:34pm") 'None)

(: event-time<? : Event Event -> Boolean)
;; checks if an event is chronologically before another event
(define (event-time<? ev1 ev2)
  (match* ((Event-time ev1) (Event-time ev2))
       [('all-day 'all-day) (string<? (Event-description ev1)
                                      (Event-description ev2))]
       [(_ 'all-day) #f]
       [('all-day _) #t]
       [((Span s1 e1) (Span s2 e2))
        (cond
          [(time<? s1 s2) #t]
          [(and (time=? s1 s2) (time<? e1 e2)) #t]
          [(and (time=? s1 s2) (time=? e1 e2))
           (string<? (Event-description ev1) (Event-description ev2))]
          [else #f])]
       [((Span s1 _) (Time h m s)) (and (not (time=? s1 (Time h m s)))
                                        (time<? s1 (Time h m s)))]
       [((Time h m s) (Span s1 _)) (or (time=? (Time h m s) s1)
                                       (time<? (Time h m s) s1))]
       [((Time h1 m1 s1) (Time h2 m2 s2))
        (or (time<? (Time h1 m1 s1) (Time h2 m2 s2))
            (and (time=? (Time h1 m1 s1) (Time h2 m2 s2))
                 (string<? (Event-description ev1) (Event-description ev2))))]
       [(_ _) (error "event-time<? match error")]))
;; Checks factored into event<? checks

(: event<? : Event Event -> Boolean)
;; determine if the first event is "less than" the second
;; according to event order
(define (event<? e1 e2)
  (or (and (date=? (Event-date e1) (Event-date e2)) (event-time<? e1 e2))
           (date<? (Event-date e1) (Event-date e2))))
;; Checks
(check-expect (event<? (Event (Date 1 2 2002) (Time 0 0 0) "b")
                            (Event (Date 1 2 2003) (Time 0 0 0) "a")) #t)
(check-expect (event<? (Event (Date 1 2 2003) (Time 0 0 0) "a")
                            (Event (Date 1 2 2002) (Time 0 0 0) "b")) #f)
(check-expect (event<? (Event (Date 1 1 2001) (Time 0 0 0) "hi")
                            (Event (Date 1 1 2001) (Time 0 0 0) "hi")) #f)
(check-expect (event<? (Event (Date 1 1 2001) (Time 0 0 0) "he")
                            (Event (Date 1 1 2001) (Time 0 0 0) "hi")) #t)
(check-expect (event<? (Event (Date 1 1 2001) 'all-day "hi")
                            (Event (Date 1 1 2001) (Time 0 0 0) "hi")) #t)
(check-expect (event<? (Event (Date 1 1 2001) (Time 0 0 0) "hi")
                            (Event (Date 1 1 2001) 'all-day "hi")) #f)
(check-expect (event<? (Event (Date 1 1 2001) 'all-day "hi")
                            (Event (Date 1 1 2001) 'all-day "he")) #f)
(check-expect (event<? (Event (Date 1 1 2001) 'all-day "he")
                            (Event (Date 1 1 2001) 'all-day "hi")) #t)
(check-expect (event<? (Event (Date 1 1 2001) 'all-day "hi")
                            (Event (Date 1 1 2001)
                                   (Span (Time 0 0 0) (Time 0 30 0)) "he")) #t)
(check-expect (event<? (Event (Date 1 1 2001)
                                   (Span (Time 0 0 0) (Time 0 30 0)) "he")
                            (Event (Date 1 1 2001) 'all-day "hi")) #f)
(check-expect (event<? (Event (Date 1 1 2001) 
                                   (Span (Time 0 0 0) (Time 0 30 0)) "he")
                            (Event (Date 1 1 2001) 
                                   (Span (Time 0 0 0) (Time 0 30 0)) "ho")) #t)
(check-expect (event<? (Event (Date 1 1 2001) 
                                   (Span (Time 0 0 0) (Time 0 30 0)) "ho")
                            (Event (Date 1 1 2001) 
                                   (Span (Time 0 0 0) (Time 0 30 0)) "he")) #f)
(check-expect (event<? (Event (Date 1 1 2001) 
                                   (Span (Time 0 0 0) (Time 0 29 0)) "hy")
                            (Event (Date 1 1 2001) 
                                   (Span (Time 0 0 0) (Time 0 30 0)) "ho")) #t)
(check-expect (event<? (Event (Date 1 1 2001)  (Time 0 0 0) "hy")
                            (Event (Date 1 1 2001) 
                                   (Span (Time 0 0 0) (Time 0 30 0)) "ho")) #t)
(check-expect (event<? (Event (Date 1 1 2001) (Time 0 30 0) "he")
                            (Event (Date 1 1 2001) (Time 0 30 0) "ho")) #t)
(check-expect (event<? (Event (Date 1 1 2001) (Time 0 30 0) "ho")
                            (Event (Date 1 1 2001) (Time 0 30 0) "he")) #f)

(: insert-event-list : Event (Listof Event) -> (Listof Event))
;; inserts an event into a list keeping the list in ascending order
(define (insert-event-list e evs)
  (match evs
    ['() (list e)]
    [(cons head tail) (if (event<? e head) (cons e (cons head tail))
                          (cons head (insert-event-list e tail)))]))
;; Eyeball checks in world

(: insert-event-tree : Event EventTree -> EventTree)
;; insert an event into an event tree, creating the node if needed
;; note: no duplicate check is necessary; insert the event no matter what
;; note: maintain the events list in ascending event order
(define (insert-event-tree e tree)
  (match* (e tree)
    [((Event d t des) (EventNode d evs l r))
     (EventNode d (insert-event-list e evs) l r)]
    [((Event d t des) 'Empty) (EventNode d (list (Event d t des))
                                         'Empty 'Empty)]
    [((Event ed t des) (EventNode d evs l r))
     (EventNode d evs (if (date<? ed d) (insert-event-tree e l) l)
                (if (date<? ed d) r (insert-event-tree e r)))]))
;; Eyeball Checks in world

(: insert-event-world : Event CalWorld3 -> CalWorld3)
; insert an event into the event tree in a cal world
(define (insert-event-world e world)
  (match world
    [(CalWorld3 m em f ccd nd nds nt npd os oe ev)
     (CalWorld3 m em f ccd nd nds nt npd os oe (insert-event-tree e ev))]))
;; Eyeball checks

(: retrieve-events : Date EventTree -> (Listof Event))
;; fetch the list of events for the given date
;; return empty list if that date is not present in the tree
(define (retrieve-events d tree)
  (match* (d tree)
    [(d 'Empty) '()]
    [(d (EventNode d evs l r)) evs]
    [(d (EventNode a _ l r)) (if (date<? d a) (retrieve-events d l)
                                 (retrieve-events d r))]))
;; Eyeball checks in world

;; == Drawing Functions ========================================================
(: month-name : Integer -> String)
;; takes in a number 1-12 and returns the month name
(define (month-name m)
  (match m
    [1 "January"] [2 "February"] [3 "March"] [4 "April"] [5 "May"] [6 "June"]
    [7 "July"] [8 "August"] [9 "September"] [10 "October"] [11 "November"]
    [12 "December"] [_ (error "month-name input is not between 1 and 12")]))
;; Checks
(check-expect (map month-name (build-list 12 (lambda ([x : Integer]) (+ x 1))))
              (list "January" "February" "March" "April" "May" "June" "July"
                    "August" "September" "October" "November" "December"))

(: day-abbreviation : Day-Of-Week -> String)
;; Takes in a Day-Of-Week and returns the name of the day
(define (day-abbreviation d)
  (match (Day-Of-Week-num d)
    [0 "Sun"] [1 "Mon"] [2 "Tue"] [3 "Wed"] [4 "Thu"] [5 "Fri"] [6 "Sat"]))
;; Checks
(check-expect (map day-abbreviation (build-list 7 Day-Of-Week))
              (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(: day-name : Day-Of-Week -> String)
;; Takes in a Day-Of-Week and returns the name of the day
(define (day-name d)
  (match (Day-Of-Week-num d)
    [0 "Sunday"] [1 "Monday"] [2 "Tuesday"] [3 "Wednesday"]
    [4 "Thursday"] [5 "Friday"] [6 "Saturday"]))
;; Checks
(check-expect (map day-name (build-list 7 Day-Of-Week))
              (list "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
                    "Friday" "Saturday"))

(: height-to-font-size : Integer -> Byte)
;; takes a height in and returns the Byte that correspods to a reasonable font
;; size (half of the height if the height is below 2*255 and 255 otherwise)
(define (height-to-font-size h)
  (if (< (quotient h 2) 255) (cast (quotient h 2) Byte) 255))
;; Checks
(check-expect (height-to-font-size 400) 200)
(check-expect (height-to-font-size 700) 255)

(: create-day-labels : CalFormat -> Image)
;; takes in a CalFormate and returns the day labels row
(define (create-day-labels fmt)
  (local {(: create-day-labels-private : CalFormat Integer -> Image)
          (define (create-day-labels-private fmt day)
            (if (<= day 6)
                (beside
                 (overlay (rectangle (CalFormat-cell-size fmt)
                                     (CalFormat-day-label-height fmt)
                                     'outline 'black)
                          (text (day-abbreviation (Day-Of-Week day))
                                (height-to-font-size
                                 (CalFormat-day-label-height fmt))
                                (CalFormat-day-label-font fmt))
                          (rectangle (CalFormat-cell-size fmt)
                                     (CalFormat-day-label-height fmt)
                                     'solid (CalFormat-day-label-bg fmt)))
                 (create-day-labels-private fmt (+ 1 day)))
                (rectangle 0 0 'solid 'blue)))}
    (create-day-labels-private fmt 0)))

(: num-weeks : Integer Integer -> Integer)
;; Returns the number of weeks in a month given the month and year
(define (num-weeks m y)
  (+ 1 (ceiling (/ (+ (days-in-month m y) -7
                      (Day-Of-Week-num (find-day-of-week (Date m 1 y)))) 7))))
;; Checks
(check-expect (num-weeks 2 1993) 5)
(check-expect (num-weeks 3 2021) 5)
(check-expect (num-weeks 10 2100) 6)

(: create-weeks : CalFormat Integer -> Image)
;; takes in a CalFormat and a number of weeks and returns the column of weeks
(define (create-weeks fmt num)
  (define week (foldr
                beside (rectangle 0 0 'outline 'white)
                (make-list
                 7 (overlay (rectangle (CalFormat-cell-size fmt)
                                       (CalFormat-cell-size fmt)
                                       'outline 'black)
                            (rectangle (CalFormat-cell-size fmt)
                                       (CalFormat-cell-size fmt)
                                       'solid (CalFormat-cell-bg fmt))))))
  (if (> num 1) (above week (create-weeks fmt (- num 1))) week))

(: create-month-template : CalFormat Integer Integer -> Image)
;; takes in a CalFormat and a month and year and returns
;; the calendar month with no days
(define (create-month-template fmt m y)
  (above
   ;; Add Title Bar
   (overlay (rectangle (* 7 (CalFormat-cell-size fmt))
                       (CalFormat-title-bar-height fmt)
                       'outline 'black)
            (text (string-append (month-name m) " " (number->string y))
                  (height-to-font-size (CalFormat-title-bar-height fmt))
                  (CalFormat-title-bar-font fmt))
            (rectangle (* 7 (CalFormat-cell-size fmt))
                       (CalFormat-title-bar-height fmt)
                       'solid (CalFormat-title-bar-bg fmt)))
   ;; Add Day Labels
   (create-day-labels fmt)
   ;; Add Weeks
   (create-weeks fmt (num-weeks m y))))

(: day-x-coordinate : CalFormat Integer Integer Integer -> Integer)
;; provides the x coordinate on a calendar page for a given Date in a given
;; calendar format
(define (day-x-coordinate fmt m d y)
  (quotient (* (- (* 2 (+ (Day-Of-Week-num (find-day-of-week (Date m d y)))
                          1)) 1)
               (CalFormat-cell-size fmt)) 2))
;; Checks
(check-expect (day-x-coordinate fmt0 1 1 2020)
              (+ (/ (CalFormat-cell-size fmt0) 2)
                 (* 3 (CalFormat-cell-size fmt0))))
(check-expect (day-x-coordinate fmt0 12 24 2021)
              (+ (/ (CalFormat-cell-size fmt0) 2)
                 (* 5 (CalFormat-cell-size fmt0))))

(: day-y-coordinate : CalFormat Integer Integer Integer -> Exact-Rational)
;; provides the y coordinate on a calendar page for a given Date in a given
;; calendar format
(define (day-y-coordinate fmt m d y)
  (+ (CalFormat-title-bar-height fmt) (CalFormat-day-label-height fmt)
     (* (/ (CalFormat-cell-size fmt) 2)
        (- (* (ceiling
               (/ (+ (Day-Of-Week-num (find-day-of-week (Date m 1 y))) d) 7)) 2)
           1))))
;; Checks
(check-expect (day-y-coordinate fmt0 1 1 2020) 110)
(check-expect (day-x-coordinate fmt0 12 24 2021) 220)
  
(: add-days : Image CalFormat Integer Integer Integer -> Image)
;; Takes an image (a calendar template) and a month, number of days in the
;; month, and year and adds the days to it. 
(define (add-days temp fmt m d y)
  (cond
    [(> d 1)
     (add-days
      (place-image
       (text (number->string d) (quotient (height-to-font-size
                                           (CalFormat-cell-size fmt)) 2)
             (if [or (date=? (Date m d y) (labor-day y))
                     (date=? (Date m d y) (memorial-day y))
                     (date=? (Date m d y) (thanksgiving y))]
                 (match (CalFormat-cell-bg fmt) ['red 'crimson] [_ 'red])
                 (CalFormat-cell-font fmt)))
       (+ (day-x-coordinate fmt m d y) (/ (CalFormat-cell-size fmt) 4))
       (- (day-y-coordinate fmt m d y) (/ (CalFormat-cell-size fmt) 4))
       temp) fmt m (- d 1) y)]
    [else
     (place-image
      (text (number->string d) (quotient (height-to-font-size
                                          (CalFormat-cell-size fmt)) 2)
            (CalFormat-cell-font fmt))
      (+ (day-x-coordinate fmt m d y) (/ (CalFormat-cell-size fmt) 4))
      (- (day-y-coordinate fmt m d y) (/ (CalFormat-cell-size fmt) 4))
      temp)]))

(: in-rect : Image Integer -> Image)
;; Put an image inside of a black rectangle outline of a given width
(define (in-rect i width)
  (overlay (rectangle width (* 1.2 (image-height i)) 'outline 'black) i))

(: filled-rect : Image Integer Image-Color -> Image)
;; Put an image inside of a filled rectangle of a
;; given width and color with a black outline
(define (filled-rect i w bg)
  (overlay (in-rect i w) (rectangle w (* 1.2 (image-height i)) 'solid bg)))

(: event->image : CalFormat Event -> Image)
;; turns an event and a calformat into an image of an event's information
;; to be displayed in a calworld
(define (event->image fmt e)
  (: text-size Byte)
  (define text-size (height-to-font-size
                     (quotient (* (CalFormat-cell-size fmt) 1) 2)))
  (beside
   (rectangle 3 (* 2.5 text-size) 'solid
              (CalFormat-title-bar-bg fmt))
   (square 10 'solid 'white)
   (above/align
    "left"
    (text (date->string (Event-date e)) text-size 'black)
    (text (match (Event-time e)
            ['all-day "(all day)"]
            [(Span s end)
             (string-append
              (time->string (Some s)) " - "(time->string (Some end)))]
            [(Time h m s) (time->string (Some (Time h m s)))]) text-size 'black)
    (text (Event-description e) text-size 'black)
    (square 5 'solid 'white))))

(: stack-events : CalFormat (Listof Event) -> Image)
;; Takes in a calformat list of events and stacks them on top of each other
;; to be displayed in a calworld
(define (stack-events fmt es)
  (match es
    ['() empty-image]
    [(cons head '()) (event->image fmt head)]
    [(cons head tail)
     (above/align
      "left"
      (event->image fmt head)
      (rectangle
       (* 1.25 (image-width
                (text (date->string (Event-date head))
                      (height-to-font-size
                       (quotient (* (CalFormat-cell-size fmt) 1) 2))
                      'black))) 3 'solid (CalFormat-title-bar-bg fmt))
      (stack-events fmt tail))]))

(: agenda : CalFormat Date EventTree -> Image)
;; Takes a calformat, date, and eventtree and turns it into the agenda to be
;; displayed on a calworld
(define (agenda fmt d tree)
  (match (retrieve-events d tree)
    ['()
     (above
      (rectangle (* (CalFormat-cell-size fmt) 5) 3 'solid
                 (CalFormat-title-bar-bg fmt))
      (text "-- NO EVENTS --"
            (height-to-font-size
             (quotient (* (CalFormat-cell-size fmt) 1) 2)) 'black))]
    [a (above/align
        "left"
        (rectangle (* (CalFormat-cell-size fmt) 5) 3 'solid
                   (CalFormat-title-bar-bg fmt))
        (stack-events fmt a))]))

;; == World Functions ==========================================================
(: draw-month : CalFormat Integer Integer -> Image)
;; draws the calendar object for a given month and year in the style of a given
;; CalFormat
(define (draw-month fmt m y)
  (define month
    (add-days (create-month-template fmt m y) fmt m (days-in-month m y) y))
  (above
   (overlay
    (rectangle (image-width month) (image-height month) 'outline 'black)
    month)
   (rectangle (* 7 (CalFormat-cell-size fmt))
              (* (CalFormat-cell-size fmt) (- 6 (num-weeks m y)))
              'solid 'white)))

(: react-to-keyboard : CalWorld3 String -> CalWorld3)
;;On the calendar page
;; when the user presses the left or right arrow keys, they change the month
;; when the user presses the up or down arrow keys, they change the year
;; when the user presses the + or - keys, they change the day
;; when the user presses t, the day switches to today
;; when the user presses ?, the help page shows up
;;On the help page
;; when the user presses esc, the calendar page shows up
;;On the entry page
;; when the user presses esc, the calendar page shows up
;; when the user presses return, the data in the box is submitted to be
;; processed
;; when the user types, the letters are entered into the entry box
;; when the user preses ` the field clears
(define (react-to-keyboard world key)
  (match world
    [(CalWorld3 m em fmt ccd nd nds nt npd os oe evs)
     (match m
       ['calendar
        (match key
          ["+" (CalWorld3 'calendar em fmt (tomorrow ccd)
                          nd nds nt npd os oe evs)]
          ["-" (CalWorld3 'calendar em fmt (yesterday ccd)
                          nd nds nt npd os oe evs)]
          ["t" (CalWorld3 'calendar em fmt nd nd nds nt npd os oe evs)]
          ["right" (CalWorld3 'calendar em fmt (add-month ccd) nd
                              nds nt npd os oe evs)]
          ["left" (CalWorld3 'calendar em fmt (subtract-month ccd)
                             nd nds nt npd os oe evs)]
          ["up" (CalWorld3 'calendar em fmt (add-year ccd)
                           nd nds nt npd os oe evs)]
          ["down" (CalWorld3 'calendar em fmt (subtract-year ccd)
                             nd nds nt npd os oe evs)]
          ["?" (CalWorld3 'help em fmt ccd nd nds nt npd os oe evs)]
          ["\r" (CalWorld3 'entry 'start fmt ccd nd nds nt "" 'None 'None evs)]
          [_ world])]
       ['help
        (match key
          ["escape" (CalWorld3 'calendar em fmt ccd nd
                               nds nt npd os oe evs)]
          [_ world])]
       ['entry
        (match key
          ["escape" (CalWorld3 'calendar 'start fmt ccd nd
                               nds nt "" 'None 'None evs)]
          ["\r" (match* (em npd)
                  [('start "") (CalWorld3 'entry 'description fmt ccd nd nds nt
                                          "" 'None 'None evs)]
                  [('start a) (match (string->time a)
                                ['None (CalWorld3 'entry 'start fmt ccd nd nds
                                                  nt "" 'None 'None evs)]
                                [s (CalWorld3 'entry 'end fmt ccd nd nds nt
                                              "" s 'None evs)])]
                  [('end "") (CalWorld3 'entry 'description fmt ccd nd nds nt ""
                                        os 'None evs)]
                  [('end a) (match (string->time a)
                              ['None (CalWorld3 'entry 'end fmt ccd nd nds nt ""
                                                os 'None evs)]
                              [s (CalWorld3 'entry 'description fmt ccd nd nds
                                            nt "" os s evs)])]
                  [('description a)
                   (CalWorld3 'calendar 'start fmt ccd nd nds nt "" os oe
                              (insert-event-tree
                               (Event ccd (match* (os oe)
                                            [('None 'None) 'all-day]
                                            [((Some a) 'None) a]
                                            [((Some a) (Some b)) (Span a b)])
                                      a) evs))])]
          ["\b" (CalWorld3 'entry em fmt ccd nd nds nt "" os oe evs)]
          ["`" (CalWorld3 'entry em fmt ccd nd nds nt "" os oe evs)]
          [a (CalWorld3 'entry em fmt ccd nd nds nt
                        (string-append npd (if (= (string-length a) 1) a ""))
                        os oe evs)]
          [_ world])])]))

(: draw : CalWorld3 -> Image)
;; draw a CalWorld3
(define (draw world)
  (match world
    [(CalWorld3 'calendar em fmt ccd nd nds nt npd os oe evs)
     (overlay/align/offset
      "left" "top"
      (above/align
       "left"
       (underlay/xy
        (draw-month fmt (Date-month ccd) (Date-year ccd))
        (- (day-x-coordinate
            fmt (Date-month ccd) (Date-day ccd) (Date-year ccd))
           (/ (CalFormat-cell-size fmt) 5))
        (- (day-y-coordinate
            fmt (Date-month ccd) (Date-day ccd) (Date-year ccd))
           (/ (CalFormat-cell-size fmt) 10))
        (overlay
         (text (number->string (Date-day ccd))
               (quotient (height-to-font-size (CalFormat-cell-size fmt)) 2)
               'white)
         (circle (/ (CalFormat-cell-size fmt) 5) 'outline 'white)
         (circle (/ (CalFormat-cell-size fmt) 5) 'solid 'black)))
       (square (/ (CalFormat-cell-size fmt) 6) 'solid 'white)
       (text (date->string ccd)
             (height-to-font-size
              (quotient (* (CalFormat-cell-size fmt) 3) 4))  'blue))
      -3 -3
      (overlay/align/offset
       "right" "top"
       (above/align
        "right"
        (text nds (height-to-font-size
                   (quotient (* (CalFormat-cell-size fmt) 3) 4)) 'black)
        (text (string-append
               (number->string
                (match (modulo (Time-hour nt) 12)
                  [0 12]
                  [a a]))
               ":" (if (< (Time-minute nt) 10) "0" "")
               (number->string (Time-minute nt))
               ":" (if (< (Time-second nt) 10) "0" "")
               (number->string (Time-second nt))
               (if (> (Time-hour nt) 12) "pm" "am"))
              (height-to-font-size (quotient
                                    (* (CalFormat-cell-size fmt) 3) 4))
              'black)
        (text "? for help"
              (height-to-font-size (quotient
                                    (* (CalFormat-cell-size fmt) 3) 4))
              'black)
        (agenda fmt ccd evs))
       4 -3
       (rectangle (* (CalFormat-cell-size fmt) 13)
                  (+ (CalFormat-cell-size fmt) (image-height
                                                (draw-month fmt 1 2021)))
                  'solid 'white)))]
    [(CalWorld3 'help em fmt ccd nd nds nt npd os oe evs)
     (above/align
      "left"
      (text "HELP" 24 'black)
      (beside
       (foldr above empty-image
              (map (lambda ([x : String]) (in-rect (text x 18 'black) 60))
                   (list "+" "-" "[right]" "[left]" "[up]" "[down]" "T" "?")))
       (foldr above empty-image
              (map (lambda ([x : String]) (in-rect (text x 18 'black) 90))
                   (list "+day" "-day" "+month" "-month" "+year"
                         "-year" "today" "help"))))
       (text "\n\nPress esc to return to the calendar" 18 'black))]
    [(CalWorld3 'entry em fmt ccd nd nds nt npd os oe evs)
     (above/align
      "left"
      (text "enter start time ([esc] to cancel, ` to clear)" 18 'black)
      (rectangle 0 10 'outline 'white)
      (filled-rect (text (date->string ccd) 18 'black) 300 'darkgray)
      (rectangle 0 10 'outline 'white)
      (match em
        ['start (in-rect (text npd 18 'black) 80)]
        ['end (above
               (filled-rect (text (time->string os) 18 'black)
                            80 'darkgray)
               (rectangle 0 10 'outline 'white)
               (in-rect (text npd 18 'black) 80))]
        ['description
         (above/align
          "left"
          (filled-rect (text (time->string os) 18 'black)
                       80 'darkgray)
          (rectangle 0 10 'outline 'white)
          (filled-rect (text (time->string oe) 18 'black)
                       80 'darkgray)
          (rectangle 0 10 'outline 'white)
          (in-rect (text npd 18 'black) (+ 20 (* 9 (string-length npd)))))]))]))

(: tick : CalWorld3 -> CalWorld3)
;; updates the time and date every tick
(define (tick world)
  (match world
    [(CalWorld3 'calendar em fmt cd _ nds _ npd os oe ev)
     (CalWorld3 'calendar em fmt cd (read-date-now)
                nds (read-time-now) npd os oe ev)]
    [a a]))
 
(: run : CalFormat Integer Integer -> CalWorld3)
;; runs the CalWorld3 for a given month, year in the style of a given CalFormat
(define (run fmt m y)
  (big-bang
      (CalWorld3 'calendar 'start fmt (Date m 1 y) (read-date-now)
                 (date->string (read-date-now))
                 (read-time-now) "" 'None 'None 'Empty) : CalWorld3
    [to-draw draw]
    [on-tick tick 1/4]
    [name "Calendar"]
    [on-key react-to-keyboard]))

(run fmt1 3 2021)

(test)