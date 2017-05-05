(setq gap 2)
(setq *cell-types*  '(NIL '(1 2) '(2 3) '(1 3)' (1 4) '(2 4) NIL '(3 4) NIL '(2 4) '(1 4) '(1 3) '(2 3) '(1 2) NIL)) ;code showing which sides of the square lines connect to, single numbers represent special cases 
(defun create-grid ( / fin c)
(progn
	(princ "Select file containing the grid data \n")
	(setq fin (open (setq fname (getfiled "Grid Data" "" "" 4)) "r"))
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
		(progn (setq xstart 0)
		(princ "Invalid input; defaulting to 0\n")))
 (setq tmp (getint "Enter the last three digits of the x coordinate of the rightmost point\n"))
 (if (and (>= tmp xstart) (< tmp 1000))
       (setq xlast  (fix (/ tmp gap)))
       (progn (setq xlast 499)
	   (princ "Invalid input; defaulting to 999\n")))
 (setq tmp (getint "Enter the last three digits of the y coordinate of the top point\n"))
 (if (and (>= tmp 0) (< tmp 1000))
     (setq ystart  (fix (/ tmp gap)))
   (progn (setq ystart 499)
	  (princ "Invalid input; defaulting to 999\n")))
 (setq y ystart)
 (setq tmp (getint "Enter the last three digits of the y coordinate of the bottom point\n"))
 (if (and (< tmp (* y gap)) (>= tmp 0))
     (setq ylast  (/ tmp gap))
   (progn 
     (setq ylast 0)
     (princ "Invalid input; defaulting to 0\n")))
 (if (> (setq step (getint "Enter the desired distance between cells \n")) 0)
     (setq step (/ step gap))
   (setq step 1))
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
   (setq lst (cdr (str-lst buf " ")))    ;space so first is always null 
   (while (<= x xlast)
     (setq val (nth x lst))
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
 (while (setq threshold (getreal "Enter threshold for contour line or press enter to exit \n"))
   (if (not values)
	  (setq values (get-values fin)))
   (draw-contour values step threshold))
 (close fin)))

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
        (setq lst (cons (read (substr str 1 pos)) lst)
              str (substr str (+ pos len))))
    (reverse (cons (read str) lst)))
	
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

(defun draw-contour (values step threshold / cell-grid i j start-edge end-edge)
  (progn
    (setq dimension (length values))
    (setq i ylast)
    (setq cell-grid NIL)                                                                
    (repeat (- ystart ylast 1)
	    (princ i)
	    (setq j xstart)
	    (repeat (- xlast xstart 1)
		    (setq bitmask 0)
		    (if (>= (nmth i j values) threshold)
		      (setq bitmask 1))
		    (if (>= (nmth i (+ 1 j) values) threshold)
		      (setq bitmask (+ 2 bitmask)))
		    (if (>= (nmth (+ 1 i) j values) theshold)
		      (setq bitmask (+ 4 bitmask)))
		    (if (>= (nmth (+ 1 i) (+ 1 j) values) threshold)
		      (setq bitmask (+ bitmask 8)))
		    (if (or (= bitmask 6) (= bitmask 8))
		      (if (>= (mean (nmth i j values) (nmth i (+ 1 j) values )(nmth (+ 1 i) j values) (nmth (+ 1 i) (+ 1 j) values)) threshold)
			  (if (= bitmask 6)
			    (setq bitmask 8))
			(if (= bitmask 8)
			  (setq bitmask 6))))      
		    (if (setq end-edge (cdr (nth bitmask *cell-types*)))
			(progn
			  (setq start-edge (car (nth bitmask *cell-type*)))
			  (plot-line (append 
	        	      (if (= start-edge 1)
				      	  (cons (* gap (+ xstart j))
				      		(* gap (- ystart i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold))))
				      	(if (= start-edge 2)
				      	    (cons (* gap (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold)))
				      		  (* gap (- ystart i)))
				      	  (if (= start-edge 3)
				      	      (cons (* gap (+ xstart j 1))
				      		    (* gap (- ystart i (find-threshold (nmth i (+ j 1) values) (nmth (+ i 1) (+ j 1) values) threshold)))) 
				      	    (cons (* gap (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ 1 i) (+ 1 j) values) threshold)))
				      		  (* gap (- ystart i 1)))))) '(0))
				      (append 
				      (if (= end-edge 1)
				      	  (cons (* gap (+ xstart j))
				      		(* gap (- ystart i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold))))
				      	(if (= end-edge 2)
				      	    (cons (* gap (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold)))
				      		  (* gap (- ystart i)))
				      	  (if (= end-edge 3)
				      	      (cons (* gap (+ xstart j 1))
				      		    (* gap (- ystart i (find-threshold (nmth i (+ j 1) values) (nmth (+ i 1) (+ j 1) values) threshold)))) 
				      	    (cons (* gap (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ 1 i) (+ 1 j) values) threshold)))
				      		 (* gap (- ystart i 1)))))) '(0)))))
				     
		      (progn (if (= bitmask 6)
				 (progn
				   (plot-line
				    (list (* gap (+ xstart j)) (* gap (- ystart i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold))) 0)
				    (list (* gap (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold))) (* gap (- ystart i)) 0))
				   (plot-line
				    (list (* gap (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ i 1) (+ j 1) values) threshold))) (- ystart i 1) 0)
				    (list (* gap (+ xstart j 1)) (* gap (- ystart i (find-threshold (nmth i (+ 1 j) values) (nmth (+ 1 i) (+ 1 j) values) threshold))) 0))) NIL)
			     (if (= bitmask 8)
				 (progn
				   (plot-line
				    (list (* gap (+ xstart j)) (* gap (- ystart i (find-threshold (nmth i j values) (nmth (+ 1 i) j values) threshold))) 0)
				    (list (* gap (+ xstart j 1)) (* gap (- ystart i (find-threshold (nmth i (+ 1 j) values) (nmth (+ 1 i) (+ 1 j)) threshold))) 0))
				   (plot-line
				    (list (* gap (+ xstart j (find-threshold (nmth (+ 1 i) j values) (nmth (+ i 1) (+ j 1) values) threshold))) (- ystart i 1) 0)
				(list (* gap (+ xstart j (find-threshold (nmth i j values) (nmth i (+ 1 j) values) threshold) (- ystart i)))))) NIL))
		      (setq j (+ 1 j)))
	    (setq i (+ 1 i)))))
  
(defun nmth (n m lst)
  (nth m (nth n lst)))

(defun plot-line (p1 p2)
  (princ p1)
  (princ p2)
  (entmake (list (cons 0  "LINE") (cons 10 p1) (cons 11 p2))))

(defun mean (lst / sum)
  (if (= 0 length list)
      NIL
    (progn
      (setq sum 0)
      (foreach i lst
	       (setq sum (+ sum i)))
      (/ sum (length lst)))))

(defun when (statement lst)
  (if statement
      (eval lst)
    NIL))

(defun sublist (start end lst)
  (progn
    (repeat start
	    (setq lst (cdr lst)))
    (setq lst (reverse lst))
    (repeat (- (length lst) end)
	    (setq lst (cdr lst)))
    (reverse lst)))
	    
(defun get-values (fin  / grid str row)
  (progn
    (read-line fin)
    (setq row (nth 1 (str-lst (read-line fin) " ")))
    (repeat (+ 5 (- 500 ystart))
	    (read-line fin))
    (repeat (- ystart ylast -1)
	    (setq str (read-line fin))
	    (setq grid (cons (sublist (+ 1 xstart) (+ 1 xlast) (str-lst (substr str 2) " ")) grid)))))

(defun find-threshold (start end val)
  (+ start (/ (- val start) (- end start))))

(create-grid)
