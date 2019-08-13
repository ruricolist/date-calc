(in-package :date-calc/test)

(def-suite date-calc)
(in-suite date-calc)

(defun run-date-calc-tests ()
  (run! 'date-calc))

(defun gen-year ()
  (gen-integer :min 1 :max 10000))

(defun gen-month ()
  (gen-integer :min 1 :max 12))

(defun gen-day ()
  (gen-integer :min 1 :max 28))

(defun gen-hour ()
  (gen-integer :min 0 :max 23))

(defun gen-minute ()
  (gen-integer :min 0 :max 59))

(defun gen-second ()
  (gen-integer :min 0 :max 59))

(defun gen-ymd ()
  (fbind ((gen-year (gen-year))
          (gen-month (gen-month))
          (gen-day (gen-day)))
    (lambda ()
      (list (gen-year)
            (gen-month)
            (gen-day)))))

(defun gen-hms ()
  (fbind ((gen-hour (gen-hour))
          (gen-minute (gen-minute))
          (gen-second (gen-second)))
    (lambda ()
      (list (gen-hour)
            (gen-minute)
            (gen-second)))))

(defun gen-ymdhms ()
  (fbind ((gen-ymd (gen-ymd))
          (gen-hms (gen-hms)))
    (lambda ()
      (append (gen-ymd) (gen-hms)))))

;;; Tests, based on examples from the Date::Calc documentation and the Perl Cookbook.

(test cookbook-example-1
  (multiple-value-bind (year month day hh mm ss)
      (add-delta-dhms
       1973 1 18 3 45 40     ;18/Jan/1973, 3:45:40 AM
       55 2 17 5             ;55 days, 2 hours, 17 minutes, 5 seconds.
       )
    (is (eql hh 6)
        (eql mm 2)
        (eql ss 55)
        (eql month 3)
        (eql day 14)
        (eql year 1974))))

(test cookbook-example-2
  (multiple-value-bind (year month day)
      (add-delta-days
       1973 1 18
       55)
    (is (eql month 3)
        (eql day 14)
        (eql year 1973))))

(test cookbook-example-3
  (let* ((bree '(1981 6 16))
         (nat '(1973 1 18))
         (difference
           (multiple-value-call #'delta-days
             (values-list bree)
             (values-list nat))))
    (is (eql -3071 difference))))

(test cookbook-example-4
  (mvlet* ((bree '(1981 6 16 4 35 25))  ;6/16/81 4:35:25
           (nat '(1973 1 18 3 45 50))   ;1/18/73, 3:45:50
           (days hours minutes seconds
            (multiple-value-call #'delta-dhms
              (values-list nat)
              (values-list bree))))
    (is (eql days 3071)
        (eql hours 0)
        (eql minutes 49)
        (eql seconds 35))))

(test days-in-year
  (is (eql 31 (days-in-year 1998 1)))
  (is (eql 59 (days-in-year 1998 2)))
  (is (eql 90 (days-in-year 1998 3))))

(test days-in-month
  (is (eql 31 (days-in-month 1998 1)))
  (is (eql 28 (days-in-month 1998 2)))
  (is (eql 29 (days-in-month 2000 2)))
  (is (eql 31 (days-in-month 1998 3))))

(test leap-year-p
  (is-true (leap-year-p 2000))
  (is-false (leap-year-p 2001)))

(test date-to-days
  (is (eql 1 (date-to-days 1 1 1)))
  (is (eql 365 (date-to-days 1 12 31)))
  (is (eql 366 (date-to-days 2 1 1)))
  (is (eql 729510 (date-to-days 1998 5 1))))

(test week-of-year
  (multiple-value-bind (week year)
      (week-of-year 2002 12 31)
    (is (eql year 2003))
    (is (eql week 1))))

(test check-date
  (is-false
   (check-date 2000 13 1)))

(test invert-delta-dhms
  "Test that add-delta-dhms is the inverse of delta-dhms."
  (for-all ((date1 (gen-ymd))
            (time1 (gen-hms))
            (date2 (gen-ymd))
            (time2 (gen-hms)))
    (is (equal
         (append date2 time2)
         (multiple-value-list
          (apply #'add-delta-dhms
                 `(,@date1
                   ,@time1
                   ,@(multiple-value-list
                      (apply #'delta-dhms
                             `(,@date1 ,@time1 ,@date2 ,@time2))))))))
    (is (equal
         (append date1 time1)
         (multiple-value-list
          (apply #'add-delta-dhms
                 `(,@date2
                   ,@time2
                   ,@(mapcar #'-
                             (multiple-value-list
                              (apply #'delta-dhms
                                     `(,@date1 ,@time1 ,@date2 ,@time2)))))))))
    (let ((delta
            (multiple-value-list
             (apply #'delta-dhms
                    `(,@date1 ,@time1 ,@date2 ,@time2)))))
      (is (equal
           delta
           (multiple-value-list
            (apply #'delta-dhms
                   `(,@date1 ,@time1
                             ,@(multiple-value-list
                                (apply #'add-delta-dhms
                                       `(,@date1
                                         ,@time1
                                         ,@delta)))))))))))

(test delta-ymd
  "Test the \"one-by-one\" semantics of delta-ymd."
  (for-all ((date1 (gen-ymd))
            (date2 (gen-ymd)))
    (let* ((result
             (multiple-value-list
              (apply #'delta-ymd (append date1 date2))))
           (neg-result
             (mapcar #'- result)))
      (is (equal date2 (mapcar #'+ date1 result)))
      (is (equal date1 (mapcar #'+ date2 neg-result))))))

(test delta-ymdhms
  "Test the \"one-by-one\" semantics of delta-ymdhms."
  (flet ((normalize (pt)
           (append (firstn 2 pt)
                   (multiple-value-list
                    (apply #'normalize-dhms (cddr pt))))))
    (for-all ((date1 (gen-ymd))
              (date2 (gen-ymd))
              (time1 (gen-hms))
              (time2 (gen-hms)))
      (let* ((pt1 (append date1 time1))
             (pt2 (append date2 time2))
             (result
               (multiple-value-list
                (apply #'delta-ymdhms
                       (append pt1 pt2))))
             (neg-result
               (mapcar #'- result)))
        (is (equal pt2 (normalize (mapcar #'+ pt1 result))))
        (is (equal pt1 (normalize (mapcar #'+ pt2 neg-result))))))))

(test canonical
  (for-all ((date (gen-ymd)))
    (let ((canonical (apply #'date-to-days date)))
      (is (equal date
                 (multiple-value-list
                  (add-delta-days 1 1 1 (1- canonical))))))))

(test invert-delta-days
  "Test the add-delta-days is the inverse of delta-days."
  (for-all ((date1 (gen-ymd))
            (date2 (gen-ymd)))
    (is (equal
         date2
         (multiple-value-list
          (apply #'add-delta-days
                 `(,@date1
                   ,(apply #'delta-days
                           `(,@date1 ,@date2)))))))
    (is (equal
         date1
         (multiple-value-list
          (apply #'add-delta-days
                 `(,@date2
                   ,(- (apply #'delta-days
                              `(,@date1 ,@date2))))))))
    (let ((delta
            (apply #'delta-days
                   `(,@date1 ,@date2))))
      (is (eql delta
               (apply #'delta-days
                      `(,@date1
                        ,@(multiple-value-list
                           (apply #'add-delta-days
                                  `(,@date1 ,delta))))))))))

(test add-delta-dhms
  (for-all ((date (gen-ymd))
            (time (gen-hms))
            (offsets
             (fbind ((gen (gen-integer :min -100 :max 100)))
               (lambda ()
                 (loop repeat 4 collect (gen))))))
    (let ((result
            (multiple-value-list
             (apply #'add-delta-dhms
                    (append date time offsets)))))
      (is (length= 6 result))
      (is (every #'numberp result))
      (is (equal
           (append date time)
           (multiple-value-list
            (apply #'add-delta-dhms
                   (append result
                           (mapcar #'- offsets)))))))))
