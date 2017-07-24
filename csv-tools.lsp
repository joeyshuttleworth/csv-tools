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
(defun nmth (n m lst)
  (nth m (nth n lst)))

(defun plot-line (p1 p2)
  ;(princ p1) (princ p2)
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
	    
(defun get-values (fin startx starty xlast ylast / grid str row)
  (progn
    (read-line fin)
    (setq row (nth 1 (str-lst (read-line fin) " ")))
    (repeat (+ 5 (- 499 ystart))
	    (read-line fin))
    (repeat (- ystart ylast 1)
	    (setq str (read-line fin))
	    (if str
		(setq grid (cons (sublist (+ 1 xstart) (+ 1 xlast) (str-lst (substr str 2) " ")) grid))))))

(defun find-threshold (start end val)
  0.5)
(princ "csv-tools.lsp loaded successfully.\n")
(setq *csv-tools-loaded* t)
					; (if (= start end)
    ;  start (/ (- val start) (- end start))))
