(setq gap 2)
(defun create-grid ( / fin)
(progn
	(princ "Select file containing the grid data \n")
	(setq fin (open (getfiled "Grid Data" "" "" 4) "r"))
	(setq count 1)
	(read-line fin)
	(read-line fin)
	(read-line fin)
	(read-line fin)
  (setq buf (read-line fin))
  (princ"\n")
  (setq gap (read (substr buf 12)))  ;2 for 2m lidar data
  (read-line fin)
  (setq count 0)
  (setq i 0)
  (setq tmp (getint "Enter the last three digits of the x coordinate of the leftmost point\n"))
  (if (and (>= tmp 0) (<= tmp 999))
		(setq xstart (fix (/ tmp gap)))
		(progn (setq xstart 1)
		(princ "Invalid input; defaulting to 0\n")))
 (setq tmp (getint "Enter the last three digits of the x coordinate of the rightmost point\n"))
 (if (and (>= tmp xstart) (< tmp 1000))
       (setq xlast  (fix (/ tmp gap)))
       (progn (setq xlast 500)
	   (princ "Invalid input; defaulting to 999\n")))
 (setq tmp (getint "Enter the last three digits of the y coordinate of the top point\n"))
 (if (and (>= tmp 0) (< tmp 1000))
     (setq ystart  (fix (/ tmp gap)))
   (progn (setq ystart 500)
	  (princ "Invalid input; defaulting to 999\n")))
 (setq y ystart)
 (setq tmp (getint "Enter the last three digits of the y coordinate of the bottom point\n"))
 (if (and (< tmp (* y gap)) (>= tmp 0))
     (setq ylast  (/ tmp gap))
   (progn 
     (setq ylast 1)
     (princ "Invalid input; defaulting to 0\n")))
 (setq step (getint "Enter the desired distance between cells \n"))
 (setq step (/ step gap))
 (princ "Using a gap of ")
 (princ (* gap step))
 (princ "m.\n")
 (princ "Plotting points, this may take some time...\n")
 (if (or (< step 1) (not step))
     (setq step 1))
 (setq c 0)
 (while (< c (- (/ 998 gap) y))
   (progn (read-line fin)
	  (setq c (+ c 1))))
 (setq buf (read-line fin))
 (while (>= y ylast)
   (setq x xstart)
   (setq lst (str-lst buf " "))
   (while (<= x xlast)
     (setq val (read (nth x lst)))
     (plotpoint layer (* 1000 gap x) (* 1000 gap y) (* 1000 val))
     (setq x (+ x step)))
   (setq y (- y step))
   (setq c 1)
   (while (< c step)
     (read-line fin)
     (setq c (+ 1 c)))
   (setq buf (read-line fin)))
 (setq values NIL)
 (while (setq threshold (getint princ "Enter threshold for contour line or press enter to exit"))
   (where values NIL
	  (setq values (get-values fin xstart xlast ystart ylast))
	  (draw-contour values step threshold))
   (close fin))))

(defun plotpoint (layer a b c)
(progn (entmake 
            (list                     
	     '(000 . "TEXT")
	     '(100 . "AcDbMText")
	     '(40 . 500) ; text height
	     (list 10 a b c)          		;; Insertion point
	     '(001 . "+")))
       (entmake 
	(list                     
	 '(000 . "TEXT")
	 '(100 . "AcDbMText")
	 '(40 . 500) ; text height
	 '(50 . 45)
	 (list 10 (+ 550 a) (+ 550 b) c)          		;; Insertion point
	 (cons 001  (rtos (/ c 1000) 2 3))))))

(defun str-lst (str del / len lst pos)
    (setq len (1+ (strlen del)))
    (while (setq pos (search str del))
        (setq lst (cons (substr str 1 pos) lst)
              str (substr str (+ pos len))))
    (reverse (cons str lst)))
	
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
;;;      1---2
;;;      |   |
;;;      3---4
;;;
;;;
;;; Also, see en.wikipedia.org/wiki/Marching_squares

(defun draw-contour (values step threshold / cell-grid dimmension i j)
  (progn
    (setq
     (dimension (length values))
     (i 1)
     (j 1)
     (cell-grid NIL))                                                                 
    (repeat (- dimmension 1)                                                 
	    (repeat (- dimmension 1)                                       
		    (setq bitmask 0)
		    (setq row (nth i grid))
		    (when (>= (nmth i j values) threshold)
		      (setq bitmask 1))
		    (when (>= (nmth i (+ 1 j) values) threshold)
		      (setq bitmask (+ 2 bitmask)))
		    (when (>= (nmth (+ 1 i) j values) theshold)
		      (setq bitmask (+ 4 bitmask)))
		    (when (>= (nmth (+ 1 i) (+ 1 j)))
		      (setq bitmask (+ bitmask 8)))
		    (when (or (= bitmask 5) (= bitmask 10))
		      (if (>= (mean (nmth i j values) (nmth i (+ 1 j) values )(nmth (+ 1 i) j values) (nmth (+ 1 i) (+ 1 j) values)) threshold)
			  (when (= bitmask 6)
			    (setq bitmask 10))
			(when (= bitmask 9)
			  (setq bitmask 6)))      
		      (setq row (cons bitmask row))
		      (incf j)
		      (draw-cell i j bitmask dimmension)
	    (cons (reverse row) cell-grid)	    
	    (incf i))
	    (setq cell-grid (reverse cell-grid))))))
    
(defun nmth (n m lst)
  (nth m (nth n lst)))

;;lst should contain real numbers
(defun mean (lst / sum)
  (if (= 0 length list)
      NIL
    (progn
      (setq sum 0)
      (foreach i lst
	       (setq sum (+ sum i)))
      (/ sum (length lst)))))

(defun sublist (start end lst)
  (progn
    (repeat start
	    (setq lst (cdr lst)))
    (setq lst (reverse lst))
    (repeat (- (length lst) end)
	    (setq lst (cdr lst)))
    (reverse lst)))
	    
(defun get-values (fin xstart xlast ystart ylast / grid str)
  (progn
    (setq fin (open (getfiled "Grid Data" "" "" 4) "r"))
	      (read-line fin)
	      (setq rows (nth 1 (str-lst (read-line fin) " ")))
	      (repeat  (+ 5 (- (/ 998 gap) ystart))
		       (read-line fin))
	      (princ (read-line fin))
	      (repeat (- ystart ylast 1)
		      (setq str (read-line fin))
		      (setq grid (cons (sublist xstart xlast (str-lst str " ")) grid)))
	      (reverse grid)))

(defun draw-cell (i j bitmask)
  (progn
    '('(1 2) '(2 3) '(1 3)' (1 4) '(2 4) '(1 1) '(3 4) '(2 2) '(2 4) '(1 4) '(1 3) '(2 3) '(1 2) '(3 3) '(4 4) ;code showing which sides of the square lines connect to, '(x x) indicates a special case
  ;;(create-grid)
