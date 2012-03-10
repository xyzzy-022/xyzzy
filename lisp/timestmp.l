;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

;;;
;;; format-date
;;;
;;; a: ’Z‚¢Œ`Ž®‚Ì—j“ú
;;; A: ’·‚¢Œ`Ž®‚Ì—j“ú
;;; b: ’Z‚¢Œ`Ž®‚ÌŒŽ
;;; B: ’·‚¢Œ`Ž®‚ÌŒŽ
;;; d: “ú(00`31)                 # (0`31)
;;; e: ˜a—ï‚Ì”N(01`)             # (1`)
;;; E: ˜a—ï‚Ì”N(Œ³, 02`)         # (Œ³, 2`)
;;; g: Œ³†(–¾Ž¡,‘å³,º˜a,•½¬)  # (–¾,‘å,º,•½)
;;; G: Œ³†(M, T, S, H)
;;; H: Žž(00`23)                 # (0`23)
;;; I: 12ŽžŠÔ‚ÌŽž(01`12)         # (1`12)
;;; i: Internet Time(000`999)
;;; m: ŒŽ(01`12)                 # (1`12)
;;; M: •ª(00`59)                 # (0`59)
;;; p: Œß‘O/ŒßŒã
;;; P: AM/PM                      # am/pm
;;; S: •b(00`59)                 # (0`59)
;;; v: —j“ú(“ú–{Œê)
;;; y: ”N(2Œ…)
;;; Y: ”N(4Œ…)
;;; z: ƒ^ƒCƒ€ƒ][ƒ“–¼(JST-9)
;;; Z: ƒ^ƒCƒ€ƒ][ƒ“(+0900)        # (+09:00)

(provide "timestmp")

(in-package "editor")

(export '(format-date format-date-string *date-formats* add-date-format
	  insert-date-string parse-date-string))

(defconstant *abbreviated-weekday-names*
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defconstant *full-weekday-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defconstant *japanese-weekday-names* "ŒŽ‰Î…–Ø‹à“y“ú")

(defconstant *abbreviated-month-names*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defconstant *full-month-names*
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defvar *timezone-name* "JST")

;; Œ³†‚Æ¼—ï‚Ì‘Î‰ž•\(‚½‚Ô‚ñ‡‚Á‚Ä‚é)
(defconstant *japanese-era-list*
  '(("•½¬" "H" 1989 1 8)
    ("º˜a" "S" 1926 12 25)
    ("‘å³" "T" 1912 7 30)
    ;; Common Lisp‚Å‚ÍGMT1900”N‚æ‚è‘O‚Í‘¶Ý‚µ‚È‚¢
    ;; ("–¾Ž¡" "M" 1868 5 9)
   ))

(defconstant *japanese-era*
  (mapcar #'(lambda (x)
	      (list (encode-universal-time
		     0 0 0 (fifth x) (fourth x) (third x) -9)
		    (third x) (first x) (second x)))
	  *japanese-era-list*))

(defun get-japanese-era (universal-time year)
  (let ((x (find universal-time *japanese-era* :test #'>= :key #'car)))
    (if x
	(cons (+ (- year (cadr x)) 1) (cddr x))
      (list (- year 1867) "–¾Ž¡" "M")  ; ‚¢‚Ü‚¢‚¿
    )))

(defun format-date (s fmt &optional universal-time)
  (unless universal-time
    (setq universal-time (get-universal-time)))
  (multiple-value-bind (sec min hour day mon year dow daylight tz)
      (decode-universal-time universal-time)
    (do ((i 0 (+ i 1))
	 (l (length fmt))
	 (jp nil))
	((= i l))
      (let ((c (elt fmt i)))
	(cond ((char= c #\%)
	       (let ((pound nil))
		 (incf i)
		 (when (= i l)
		   (return))
		 (setq c (elt fmt i))
		 (when (char= c #\#)
		   (setq pound t)
		   (incf i)
		   (when (= i l)
		     (return))
		   (setq c (elt fmt i)))
		 (let ((fmtd (if pound "~d" "~2,'0d")))
		   (case c
		     (#\a
		      (princ (svref *abbreviated-weekday-names* dow) s))
		     (#\A
		      (princ (svref *full-weekday-names* dow) s))
		     (#\b
		      (princ (svref *abbreviated-month-names* (- mon 1)) s))
		     (#\B
		      (princ (svref *full-month-names* (- mon 1)) s))
		     (#\d
		      (format s fmtd day))
		     (#\e
		      (unless jp
			(setq jp (get-japanese-era universal-time year)))
		      (format s fmtd (car jp)))
		     (#\E
		      (unless jp
			(setq jp (get-japanese-era universal-time year)))
		      (if (= (car jp) 1)
			  (princ "Œ³" s)
			(format s fmtd (car jp))))
		     (#\g
		      (unless jp
			(setq jp (get-japanese-era universal-time year)))
		      (princ (if pound (svref (cadr jp) 0) (cadr jp)) s))
		     (#\G
		      (unless jp
			(setq jp (get-japanese-era universal-time year)))
		      (princ (caddr jp) s))
		     (#\H
		      (format s fmtd hour))
		     (#\I
		      (let ((h (mod hour 12)))
			(format s fmtd (if (zerop h) 12 h))))
		     (#\i
		      (format s "~3,'0d"
			      (truncate (rem (+ universal-time 3600) 86400) 86.4)))
		     (#\m
		      (format s fmtd mon))
		     (#\M
		      (format s fmtd min))
		     (#\p
		      (princ (if (< hour 12) "Œß‘O" "ŒßŒã") s))
		     (#\P
		      (if pound
			  (princ (if (< hour 12) "am" "pm") s)
			(princ (if (< hour 12) "AM" "PM") s)))
		     (#\S
		      (format s fmtd sec))
		     (#\v
		      (princ (svref *japanese-weekday-names* dow) s))
		     (#\y
		      (format s "~2,'0d" (mod year 100)))
		     (#\Y
		      (princ year s))
		     (#\z
		      (format s "~A~D" *timezone-name* tz))
		     (#\Z
		      (let ((x (abs tz)))
			(format s "~:[+~;-~]~2,'0d~:[~;:~]~2,'0d"
				(plusp tz) (truncate x) pound
				(mod (truncate (* x 60)) 60))))
		     (t
		      (write-char c s))))))
	      (t
	       (write-char c s)))))))

(defun format-date-string (fmt &optional universal-time)
  (with-output-to-string (s)
    (format-date s fmt universal-time)))

(defvar *date-formats*
  '("%a, %d %b %Y %H:%M:%S %Z"
    "%a, %d %b %Y %H:%M:%S %z"
    "%a %b %d %H:%M:%S %Y"
    "%d %b %Y %H:%M:%S %Z"
    "%d %b %Y %H:%M:%S %z"
    "%Y-%m-%dT%H:%M:%S%#Z"
    "%B %d, %Y"
    "%b %d %Y"
    "%Y-%m-%d"
    "%d %b %y"
    "%y/%m/%d"
    "%y-%m-%d"
    "%g%#e”N%#mŒŽ%#d“ú %v—j“ú"
    "%g%#e”N%#mŒŽ%#d“ú"
    "%Y”N%#mŒŽ%#d“ú(%v)"
    "%Y”N%#mŒŽ%#d“ú"
    "%y”N%#mŒŽ%#d“ú(%v)"
    "%y”N%#mŒŽ%#d“ú"
    " %H:%M:%S"
    " %#H:%M:%S"
    " %#I:%M:%S %P"
    " %#HŽž%#M•ª%#S•b"
    " %p%#IŽž%#M•ª%#S•b"
    "@%i"
    ))

(defun add-date-format (fmt)
  (pushnew fmt *date-formats* :test #'string=))

(defun insert-date-string ()
  (interactive "*")
  (multiple-value-bind (result data)
      (dialog-box '(dialog 0 0 260 120
		    (:caption "“ú•t‚ÆŽž")
		    (:font 9 "MS UI Gothic")
		    (:control
		     (:listbox list nil #x50a10001 4 5 192 114)
		     (:button IDOK "OK" #x50030001 205 5 52 14)
		     (:button IDCANCEL "ƒLƒƒƒ“ƒZƒ‹" #x50030000 205 22 52 14)))
		  (list (cons 'list (mapcar #'format-date-string *date-formats*)))
		  '((list :must-match t :enable (IDOK))))
    (when result
      (insert (cdr (assoc 'list data))))))

(defvar *date-format-regexp*
  (ed:compile-regexp
   "\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\) +\\([0-9][0-9]?\\):\\([0-9][0-9]?\\):\\([0-9][0-9]?\\)"))

(defun parse-date-string (string)
  (when (and (stringp string)
	     (ed:string-match *date-format-regexp* string))
    (handler-case (apply #'encode-universal-time
			 (do ((x 1 (1+ x))
			      (r nil))
			     ((> x 6) r)
			   (push (parse-integer (substring string
							   (ed:match-beginning x)
							   (ed:match-end x)))
				 r)))
      (error () nil))))
