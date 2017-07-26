(if (not *csv-tools-loaded*)
    (progn (princ "Please load csv-tools.lsp first\n")
	   (exit)))

(defun draw-profile ( / x startx lastx grad current prev tmp starty y lasty val yscale first-point xscale tmp choice fname)
  (progn
	(princ "\nSelect file containing the grid data\n")
	(setq fin (open (setq fname (getfiled "Grid Data" "" "" 4)) "r"))
	(setq count 1)
	(read-line fin)
	(read-line fin)
	(read-line fin)
	(read-line fin)
  (setq buf (read-line fin))
  (princ"\n")
  (setq gap (read (substr buf 12)))
  (princ "Spatial Resolution is ")
  (princ gap)
  (princ "m\n")
  (close fin)
  (setq startx (fix (/ (getint "Enter the last three digits of the westmost x coordinate\n") gap)))
  ;(setq starty (fix (/ (getint "Enter the last three digits of the westmost y co-ordinate\n") gap)))
  (setq lastx (fix (/ (getint "Enter the last three digits of the eastmost x coordinate\n") gap)))
  (if (= startx lastx)
      (progn
	(setq starty (fix (/ (getint "Enter the last three digits of the northmost y coordinate\n") gap)))
	(setq lasty  (fix (/ (getint "Enter the last three digits of the southmost y coordinate\n") gap)))
	(setq valscale (getreal "Enter y scale\n"))
	(setq first-point (list 0 (get-nmth fname startx starty)))
	(setq current first-point)
	(setq y starty)
	(while (> y lasty)
	  (setq prev current)
	  (setq val (get-nmth fname startx y))                    ;xscale is incorrect
	  (if (> val -9000)
	      (progn
		(setq current (list (* 1000 gap y) (* 1000 valscale val) 0))
		(make-profile-text (list (car current) (+ (cadr first-point) (* valscale -1000)) 0) val)
		(plot-line prev current)))
	  (setq y (+ y 1))))
	  
					;(while (not gradient)
					;  (setq tmp (read (getstring ("Enter direction gradient of the profile (as a whole number), N (north) or S (south)"))))
					;  (if (or (= tmp "N") (= tmp "S")  (= (type tmp) (type 1)))
					;	(setq gradient tmp))
    (progn
      (setq starty (fix (/ (getint "Enter the last three digits of the westmost y coordinate\n") gap)))

      ;(while (not grad)
;	(setq tmp (read (getstring "Enter the desired gradient\n")))
;	(if (= (type tmp) (type 1))
;	    (setq grad tmp)
;	  (if (= (/ 1 tmp) (fix (/ 1 tmp)))
;	      (setq grad tmp)
					;	    (princ "The gradient must be either a whole number or the reciprical of a whole number\n"))))
      (while (not choice)
	(setq tmp (getint "In which form do you wish to enter the section bearing?\n  1) x in 1 OR 2) 1 in x\n"))
	(if (or (= tmp 1) (= tmp 2))
	    (setq choice tmp)))
      (setq grad (getint "Enter x\n"))
      (setq valscale (getreal "Enter y scale\n"))
      (if (= choice 1)
	  (progn
	    (setq x (+ 1 startx))
	    (setq xscale (expt (+ 1 (expt grad 2)) 0.5))
	    (setq first-point (list (* 1000 startx xscale) (* 1000 valscale (get-nmth fname startx starty)) 0))
	    (setq current first-point)
	    (while (< x lastx)
	      (setq val (get-nmth fname x (+ starty (* grad x))))	      (setq prev current)
	      (if (not (< val -9000))     ;nodata value	      
		  (progn
		    (setq current (list (* 1000 x xscale) (* 1000 valscale val) 0))
		    (plot-line prev current)
		    (make-profile-text (list (car current) (+ (cadr first-point) (* valscale -1000)) 0) val)))
	      (setq x (+ x 1))))
	(progn
	  (setq x (+ 1 startx))
	  (setq y (if (>= grad 0) (+ starty 1) (- starty 1)))
	  (setq yscale (expt (+ 1 (expt grad 2)) 0.5))
	  (setq first-point (list (* 1000 starty yscale (if (< 0 grad) 1 -1)) (* 1000 valscale (get-nmth fname startx starty)) 0))
	  (setq current first-point)
	  (while (and (> x startx) (< x lastx))
	    (setq x (+ x (abs grad)))
	    (setq val (get-nmth fname x y))
	    (if (not (< val -9000))
		(progn
		  (setq prev current)
		  (setq current (list (* 1000 (if (>= grad 0) y (* -1 y)) yscale) (* valscale 1000 val) 0))
		  (make-profile-text (list (car current) (+ (cadr first-point) (* valscale -1000)) 0) val)
		  (plot-line prev current)))
	    (setq y (+ y (if (> grad 0) 1 -1))))))))))

(defun get-nmth (fname x y / fin) ;Resets file position
  (progn (setq fin (open fname "r"))
	 (if (not fin)
	     (progn
	       (princ "Couldn't open file ") (princ filename) (princ ". Try reopening the drawing.\n")   ;This happens after lots of file IO
	       (exit)))
	 (repeat (- (/ 1000 gap) y -6)
		  (read-line fin))
	 (nth (+ 1 x) (str-lst (read-line fin) " "))))

(defun make-profile-text (pos val)
  (entmake 
   (list                     
    '(000 . "TEXT")
    '(100 . "AcDbMText")
    '(40 . 500) ; text height
    '(50 . 1.57)  ;Angle is in radians
    (cons 10 pos)
    (cons 001 (rtos val)))))
(draw-profile)
