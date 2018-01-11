(if (not *csv-tools-loaded*)
    (progn (princ "Please load csv-tools.lsp first\n")
	   (exit)))
(setq gap 2)

;;;     _4
;;;   1|_|3
;;;     2

(setq *cell-types* (list NIL
			 '(1 2)
			 '(2 3)
			 '(1 3)
			 '(3 4)
			 NIL
			 '(2 4)
			 '(1 4)
			 '(1 4)
			 '(2 4)
			 NIL
			 '(3 4)
			 '(1 3)
			 '(2 3)
			 '(1 2)
			 NIL))
					;code showing which sides of the square lines connect to, single numbers represent special cases 
(defun create-grid ( / fin c x y lst xlast ylast tmp xstart ystart step buf c max-val min-val)
(progn
	(princ "\nSelect file containing the grid data ")
	(setq fin (open (setq fname (getfiled "Grid Data" "" "" 4)) "r"))
	(setq count 1)
	(read-line fin)
	(read-line fin)
	(read-line fin)
	(read-line fin)
  (setq buf (read-line fin))
  (princ"\n")
  (setq gap (read (substr buf 12)))  ;2 for 2m lidar data
  (princ "Spatial resolution is ")
  (princ gap)
  (princ "m\n")
  (read-line fin)
  (setq count 0)
  (setq i 0)
  (setq tmp (getint "\nEnter the last three digits of the x coordinate of the leftmost point\n"))
  (if (and (>= tmp 0) (<= tmp 999))
		(setq xstart (fix (/ tmp gap)))
		(progn (setq xstart 0)
		(princ "\nInvalid input; defaulting to 0\n")))
 (setq tmp (getint "\nEnter the last three digits of the x coordinate of the rightmost point\n"))
 (if (and (>= tmp xstart) (< tmp 1000))
       (setq xlast  (fix (/ tmp gap)))
       (progn (setq xlast 499)
	   (princ "\nInvalid input; defaulting to 999\n")))
 (setq tmp (getint "\nEnter the last three digits of the y coordinate of the top point\n"))
 (if (and (>= tmp 0) (< tmp 1000))
     (setq ystart  (fix(/ tmp gap)))
   (progn (setq ystart 499)
	  (princ "\nInvalid input; defaulting to 999\n")))
 (setq y ystart)
 (setq tmp (getint "\nEnter the last three digits of the y coordinate of the bottom point\n"))
 (if (and (< tmp (* y gap)) (>= tmp 0))
     (setq ylast  (fix (/ tmp gap)))
   (progn 
     (setq ylast 0)
     (princ "\nInvalid input; defaulting to 0\n")))
 (if (> (setq step (getint "\nEnter the desired distance between cells\n")) 0)
     (setq step (fix (/ step gap)))
   (setq step 1))
 (princ "\nUsing a gap of ")
 (princ (* gap step))
 (princ "m.")
 (princ "\nPlotting points, this may take some time...\n")
 (if (or (< step 1) (not step))
     (setq step 1))
 (setq c 0)
 (while (< c (- (/ 998 gap) y))
   (progn (read-line fin)
	  (setq c (+ c 1))))
 (setq buf (read-line fin))
 (setq max-val 0)
 (setq min-val 999999)
 (while (>= y ylast)
   (setq x xstart)
   (setq lst (str-lst buf " "))    
   (while (<= x xlast)
     (setq val (nth x lst))
     (if (> val max-val)
	 (setq max-val val)
       (if (< val min-val)
	   (setq min-val val)))
     (plotpoint layer (* 1000 gap x) (* 1000 gap y) (* 1000 val))
     (setq x (+ x step)))
   (setq y (- y step))
   (setq c 1)
   (while (< c step)
     (read-line fin)
     (setq c (+ 1 c)))
   (setq buf (read-line fin)))
 (setq values NIL)
 (close fin)
 (setq fin (open fname "r"))
 (princ "Maximum value: ")
 (princ max-val)
 (princ " Minimum value: ")
 (princ min-val)
 (princ "\n")
 (setq num-contours (getint "Enter the number of contour lines you wish to plot\n"))
 (if (> num-contours 0)
     (progn
       (princ "Initialising values - please wait.\n")
       (setq values (get-values fin))
       (while (> num-contours 0)
	 (progn 
	   (setq contour-step (getreal "\nEnter the threshold for the first contour line\n"))
	   (setq threshold contour-step)
	   (if (> num-contours 1)
	       (setq contour-step (/ (- (getreal "\nEnter the threshold for the last contour line\n") contour-step) (- num-contours 1))))
	   (repeat num-contours
		   (progn
		     (princ "Drawing contour with threshold ")
		     (princ threshold)
		     (princ "\n")
		     (draw-contour values step threshold)
		     (setq threshold (+ threshold contour-step)))))
       (setq num-contours (getint "Enter the number of contour lines you wish to plot\n")))))
       "Done"))

(defun plotpoint (layer a b c)
(progn (entmake 
            (list                     
	     '(000 . "TEXT")
	     '(100 . "AcDbMText")
	     '(40 . 500) ; text height
	     (list 10 (- a 166.67) (- b 166.67) c)          		;; Insertion point
	     '(001 . "+")))
       (entmake 
	(list                     
	 '(000 . "TEXT")
	 '(100 . "AcDbMText")
	 '(40 . 500) ; text height
	 '(50 . 45)
	 (list 10 (+ 400 a) (+ 400 b) c)          		;; Insertion point
	 (cons 001  (rtos (/ c 1000) 2 3))))))

(defun str-lst (str del / len lst pos)
  (progn
    (setq len (1+ (strlen del)))
    (while (setq pos (search str del))
      (setq lst (cons (read (substr str 1 pos)) lst)
	    str (substr str (+ pos len))))
    (setq lst (reverse (cons (read str) lst)))
    (while (not (car lst))
      (setq lst (cdr lst)))
    lst))
	
(defun search (str char / count tmp)
	(setq count 1)
	(while (and (/= tmp char) (<= count (strlen str)))
		(setq tmp (substr str count 1))
			(setq count (+ count 1)))
			(if (< count (strlen str))
				(- count 2)
				(if (= (substr str (strlen str) 1) char)
					(- (strlen str) 1)
					(if(> (- (strlen str) 1) 0)
						(if (= (substr str (- (strlen str) 1) 1) char)
							(- (strlen str) 2))))))

;;; Diagram showing which vertices of a given cell correspond to which bit positions in bitmask:
;;;
;;;      4---3
;;;      |   |
;;;      1---2
;;;
;;;
;;; See en.wikipedia.org/wiki/Marching_squares

(defun draw-contour (values step threshold / cell-grid i j start-edge end-edge spacing bitmask)
  (progn
    (setq spacing (* 1000 gap))
    (setq dimension (length values))
    (setq i 0)
    (setq cell-grid NIL)
    ;(princ values)
    (repeat (fix (- ystart ylast))
	    (setq j 0)
	    (repeat (- xlast xstart)
		    (setq bitmask (calculate-bitmask threshold (nmth i j values) (nmth i (+ j 1) values) (nmth (+ i 1) (+ j 1) values) (nmth (+ i 1) j values)))
		   ; (princ (list i j bitmask))
		   ; (princ ":\n")
		    (if (nth bitmask *cell-types*)
			(progn
			  (setq end-edge (nmth bitmask 1 *cell-types*))
			  (setq start-edge (car (nth bitmask *cell-types*)))
			  (if (and start-edge end-edge)
			      (progn
				;(princ "start edge: ") (princ start-edge) (princ "\n")
			;	(princ "end edge: ") (princ end-edge) (princ "\n")
				(plot-line
				 (append  (cond ((= start-edge 1)
						(list (* spacing (+ xstart j))
						      (* spacing (+ ylast i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold)))))   
					       ((= start-edge 2)
						(list (* spacing (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold)))
						      (* spacing (+ ylast i))))
					       ((= start-edge 3)
						(list (* spacing (+ xstart j 1))
						      (* spacing (+ ylast i (find-threshold (nmth i (+ j 1) values) (nmth (+ i 1) (+ j 1) values) threshold)))))
					       ((= start-edge 4)
						(list (* spacing (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ 1 i) (+ 1 j) values) threshold))
						      (* spacing (+ ylast i 1))))))
					       '(0))
				 (append (cond ((= end-edge 1)
						(list (* spacing (+ xstart j))
						      (* spacing (+ ylast i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold)))))   
					       ((= end-edge 2)
						(list (* spacing (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold)))
						      (* spacing (+ ylast i))))
					       ((= end-edge 3)
						(list (* spacing (+ xstart j 1))
						      (* spacing (+ ylast i (find-threshold (nmth i (+ j 1) values) (nmth (+ i 1) (+ j 1) values) threshold)))))
					       ((= end-edge 4)
						(list (* spacing (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ 1 i) (+ 1 j) values) threshold)))
						      (* spacing (+ ylast i 1)))))
					       '(0)))))))					        
		      ;   (if (= bitmask 5)
		;	      (progn
		;		(plot-line
		;		 (list (* spacing (+ xstart j)) (* spacing (- ystart i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold))) 0)
		;		 (list (* spacing (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold))) (* spacing (- ystart i)) 0))
		;		(plot-line
		;		 (list (* spacing (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ i 1) (+ j 1) values) threshold))) (- ystart i 1) 0)
		;		 (list (* spacing (+ xstart j 1)) (* spacing (- ystart i (find-threshold (nmth i (+ 1 j) values) (nmth (+ 1 i) (+ 1 j) values) threshold))) 0))) NIL);
;			  (if (= bitmask 10)
;			      (progn
;				(plot-line
;				 (list (* spacing (+ xstart j)) (* spacing (- ystart i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold))) 0)
;				 (list (* spacing (+ xstart j 1)) (* spacing (- ystart i (find-threshold (nmth i (+ 1 j) values) (nmth (+ 1 i) (+ 1 j) values) threshold))) 0))
;				(plot-line
;				 (list (* spacing (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ i 1) (+ j 1) values) threshold))) (- ystart i 1) 0)
;				 (list (* spacing (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold) (- ystart i))) 0))) NIL)))
		    (setq j (+ 1 j)))
	    (setq i (+ 1 i)))))

(defun get-values (fin  / grid str row)
  (progn
    (read-line fin)
    (setq row (nth 1 (str-lst (read-line fin) " ")))
    (repeat (fix (+ 4 (- 499 ystart)))
	    (read-line fin))
    (repeat (- ystart ylast -1)
	  ;  (princ grid)
	   ; (princ "\n")
	    (setq str (read-line fin))
	    (if str
		(setq grid (cons (sublist (+ 0 xstart) (+ 1 xlast) (str-lst (substr str 2) " ")) grid))))))

(defun find-threshold (start end val)
  (if (= start end)
      start (/ (- val start) (- end start))))

(defun calculate-bitmask (val i j k l / bitmask)
  (progn					;(princ (list i j k l))
    (setq bitmask (if (> i val) 1 0))
    (setq bitmask (+ bitmask (if (> j val) 2 0)))
    (setq bitmask (+ bitmask (if (> k val) 4 0)))
    (+ bitmask (if (> l val) 8 0))))

(create-grid)
