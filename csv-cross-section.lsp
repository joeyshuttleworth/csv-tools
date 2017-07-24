(if (not *csv-tools-loaded*)
    (progn (princ "Please load csv-tools.lsp first\n")
	   (exit)))

(defun draw-profile ( / x startx lastx grad current prev tmp starty y lasty val yscale first-point)
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
	(setq yscale (getreal "Enter y scale\n"))
	(setq current (list 0 (get-nmth fname startx starty)))
	(setq y starty)
	(while (> y lasty)
	  (setq prev current)
	  (setq val (get-nmth fname startx y))
	  (if (not (= -9999 val))
	      (progn
		(setq current (list (* gap y) (* yscale val) 0))
		(plot-profile-line prev current)))
	  (setq y (+ y 1))))
	  
					;(while (not gradient)
					;  (setq tmp (read (getstring ("Enter direction gradient of the profile (as a whole number), N (north) or S (south)"))))
					;  (if (or (= tmp "N") (= tmp "S")  (= (type tmp) (type 1)))
					;	(setq gradient tmp))
    (progn
      (setq starty (fix (/ (getint "Enter the last three digits of the westmost y coordinate\n") gap)))
      (setq grad (getint "Enter the desired gradient\n"))
      (setq yscale (getreal "Enter y scale\n"))
      (setq x (+ 1 startx))
      (setq first-point (list (* 1000 (expt (* (+ 1 (expt grad 2))(expt (* gap xstart) 2)) 0.5)) (* 1000 yscale (get-nmth fname startx starty)) 0))
      (setq current first-point)
	  (while (< x lastx)
	  (setq val (get-nmth fname x (+ starty (* grad x))))
	  (setq prev current)
	  (if (not (= val -9999))     ;nodata value	      
	      (progn (setq current (list (* 1000 (expt (* (+ 1 (expt grad 2))(expt (* gap x) 2)) 0.5)) (* 1000 yscale val) 0))
		     (plot-line prev current)
		     (entmake 
		       (list                     
			'(000 . "TEXT")
			'(100 . "AcDbMText")
			'(40 . 1000) ; text height
			(list 10 (car current) (+ (if (= 0 (rem x 2)) 1500 0) (cadr first-point) (* yscale -1000)) 0)         		;; Insertion point
			(cons 001 (rtos val))))))
	    (setq x (+ x 1)))))))
  

(defun get-nmth (fname x y / fin) ;Resets file position
  (progn (setq fin (open fname "r"))
	 (if (not fin)
	     (progn
	       (princ "Couldn't open") (princ filename) (princ "Try restarting Progecad\n")   ;This happens after lots of file IO
	       (exit)))
	 (repeat (- (/ 1000 gap) y -6)
		  (read-line fin))
	 (nth (+ 1 x) (str-lst (read-line fin) " "))))

(draw-profile)
