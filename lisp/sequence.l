;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "sequence")

(in-package "lisp")

(export '(complement make-sequence concatenate map map-into
	  some every notany notevery reduce
	  remove-duplicates delete-duplicates
	  substitute substitute-if substitute-if-not nsubstitute
	  nsubstitute-if nsubstitute-if-not count count-if count-if-not
	  mismatch merge sort))

(setf (symbol-function 'sort) #'stable-sort)

(defun complement (fn)
  #'(lambda (&rest args)
     (not (apply fn args))))

(setf (get 'with-sequence-start-end 'ed:lisp-indent-hook) 3)

(defmacro with-sequence-start-end (start end sequence &body body)
  `(let ((,end (cond ((null ,end)
		      (length ,sequence))
		     ((> ,end (length ,sequence))
		      (error 'range-error :datum ,end))
		     (t ,end))))
     (if (< ,end ,start)
	 (error 'range-error :datum ,end))
     ,@body))

(defun make-sequence (type size &key initial-element)
  (setq type
	(cond ((eq type 'list)
	       (return-from make-sequence
		 (make-list size :initial-element initial-element)))
	      ((or (eq type 'vector)
		   (eq type 'simple-vector))
	       't)
	      ((or (eq type 'string)
		   (eq type 'simple-string))
	       'character)
	      (t
	       (let ((ctype (si:canonicalize-type type)))
		 (cond ((eq (car ctype) 'list)
			(return-from make-sequence
			  (make-list size :initial-element initial-element)))
		       ((and (or (eq (car ctype) 'array)
				 (eq (car ctype) 'simple-array))
			     (cond ((atom (caddr ctype))
				    (eql (caddr ctype) 1))
				   ((endp (cdaddr ctype))
				    (cond ((eq (caaddr ctype) '*))
					  ((= (caaddr ctype) size))
					  (t (error "型指定子のサイズと指定されたサイズが異なります"))))
				   (t nil))))
		       (t
			(error 'bad-type-specifier :datum type)))
		 (upgraded-array-element-type (cadr ctype))))))
  (si:*make-vector size type initial-element nil nil nil nil))

(defun concatenate (result-type &rest sequences)
  (apply #'si:*copy-into-seq (make-sequence result-type
					    (apply #'+ (mapcar #'length
							       sequences)))
	 sequences))

(defun map (result-type function sequence &rest more-sequences)
  (push sequence more-sequences)
  (let ((l (apply #'min (mapcar #'length more-sequences))))
    (if (null result-type)
	(do ((i 0 (+ i 1)))
	    ((= i l) nil)
	  (apply function (mapcar #'(lambda (x) (elt x i))
				  more-sequences)))
      (let ((result-sequence (make-sequence result-type l)))
	(if (vectorp result-sequence)
	    (do ((i 0 (+ i 1)))
		((= i l))
	      (setf (elt result-sequence i)
		    (apply function (mapcar #'(lambda (x) (elt x i))
					    more-sequences))))
	  (do ((i 0 (+ i 1))
	       (r result-sequence (cdr r)))
	      ((= i l))
	    (setf (car r) (apply function (mapcar #'(lambda (x) (elt x i))
						  more-sequences)))))
	result-sequence))))

(defun map-into (result-sequence function &rest sequences)
  (when result-sequence
    (if (vectorp result-sequence)
	(let ((l (min (array-total-size result-sequence)
		      (apply #'min (mapcar #'length sequences)))))
	  (when (and (array-has-fill-pointer-p result-sequence)
		     (> l (length result-sequence)))
	    (setf (fill-pointer result-sequence) l))
	  (do ((i 0 (+ i 1)))
	      ((= i l))
	    (setf (elt result-sequence i)
		  (apply function (mapcar #'(lambda (x) (elt x i))
					  sequences)))))
      (let ((l (min (length result-sequence)
		    (apply #'min (mapcar #'length sequences)))))
	  (do ((i 0 (+ i 1))
	       (r result-sequence (cdr r)))
	      ((= i l))
	    (setf (car r) (apply function (mapcar #'(lambda (x) (elt x i))
						  sequences)))))))
  result-sequence)

(defun some (predicate sequence &rest more-sequences)
  (push sequence more-sequences)
  (do ((i 0 (+ i 1))
       (l (apply #'min (mapcar #'length more-sequences))))
      ((= i l) nil)
    (let ((r (apply predicate
		    (mapcar #'(lambda (x) (elt x i)) more-sequences))))
      (and r (return r)))))

(defun every (predicate sequence &rest more-sequences)
  (push sequence more-sequences)
  (do ((i 0 (+ i 1))
       (l (apply #'min (mapcar #'length more-sequences))))
      ((= i l) t)
    (or (apply predicate (mapcar #'(lambda (x) (elt x i)) more-sequences))
	(return nil))))

(defun notany (predicate sequence &rest more-sequences)
  (not (apply #'some predicate sequence more-sequences)))

(defun notevery (predicate sequence &rest more-sequences)
  (not (apply #'every predicate sequence more-sequences)))

(defun reduce (function sequence
			&key from-end (start 0) end
			(initial-value nil sv)
			(key #'identity))
  (unless key (setq key #'identity))
  (with-sequence-start-end start end sequence
    (cond ((null from-end)
	   (unless sv
	     (if (= start end)
		 (return-from reduce (funcall function)))
	     (setq initial-value (funcall key (elt sequence start)))
	     (incf start))
	   (do ((x initial-value (funcall function
					  x
					  (prog1
					      (funcall key (elt sequence start))
					    (incf start)))))
	       ((= start end) x)))
	  (t
	   (unless sv
	     (if (= start end)
		 (return-from reduce (funcall function)))
	     (decf end)
	     (setq initial-value (funcall key (elt sequence end))))
	   (do ((x initial-value (funcall function
					  (funcall key (elt sequence (decf end))) x)))
	       ((= start end) x))))))

(defun remove-duplicates (sequence &key from-end test test-not (start 0) end (key #'identity))
  (delete-duplicates (copy-seq sequence)
		     :from-end from-end
		     :test test
		     :test-not test-not
		     :start start
		     :end end
		     :key key))

(defun list-delete-duplicates (list &key from-end test test-not start end key)
  (unless test (setq test #'eql))
  (unless key (setq key #'identity))
  (if (or (endp list)
	  (zerop end)
	  (= start end))
      list
    (do* ((r (cons nil list))
	  (xlast (nthcdr start r))
	  (x (cdr xlast) (cdr x))
	  (tail (nthcdr (- end start) x)))
	((eq x tail) (cdr r))
      (do ((y (cdr x) (cdr y))
	   (ylast x)
	   (item (funcall key (car x))))
	  ((eq y tail)
	   (setq xlast (cdr xlast)))
	(if (if test-not
		(not (funcall test-not item (funcall key (car y))))
	      (funcall test item (funcall key (car y))))
	    (if from-end
		(setf (cdr ylast) (cdr y))
	      (progn
		(setf (cdr xlast) (cdr x))
		(return)))
	  (setq ylast (cdr ylast)))))))

(defun vector-delete-duplicates (vector &key from-end test test-not start end key)
  (unless key (setq key #'identity))
  (do ((i start (1+ i))
       (j start))
      ((= i end)
       (do ((i i (1+ i))
	    (j j (1+ j))
	    (l (length vector)))
	   ((= i l)
	    (si:*set-vector-length vector j)
	    vector)
	 (setf (elt vector j) (elt vector i))))
    (setf (elt vector j) (elt vector i))
    (unless (find (funcall key (elt vector i)) vector
		  :start (if from-end start (1+ i))
		  :end (if from-end j end)
		  :test test :test-not test-not :key key)
      (setq j (1+ j)))))

(defun delete-duplicates (sequence &key from-end test test-not
				   (start 0) (end (length sequence))
				   (key #'identity))
  (with-sequence-start-end start end sequence
    (if (listp sequence)
	(list-delete-duplicates sequence :from-end from-end
				:test test :test-not test-not
				:start start :end end :key key)
      (vector-delete-duplicates sequence :from-end from-end
				:test test :test-not test-not
				:start start :end end :key key))))

(defun seq-testfn (olditem test test-not)
  (and test test-not
       (error ":testと:test-notが両方指定されています"))
  (cond (test
	 #'(lambda (x) (funcall test olditem x)))
	(test-not
	 #'(lambda (x) (not (funcall test-not olditem x))))
	(t
	 #'(lambda (x) (eql olditem x)))))

(defun substitute (newitem olditem sequence &rest rest)
  (apply #'nsubstitute newitem olditem (copy-seq sequence) rest))

(defun substitute-if (newitem test sequence &rest rest)
  (apply #'nsubstitute-if newitem test (copy-seq sequence) rest))

(defun substitute-if-not (newitem test sequence &rest rest)
  (apply #'nsubstitute-if-not newitem test (copy-seq sequence) rest))

(defun nsubstitute (newitem olditem sequence
			    &key from-end test test-not (start 0) end count (key #'identity))
  (nsubstitute-if newitem (seq-testfn olditem test test-not) sequence
		  :from-end from-end
		  :start start
		  :end end
		  :count count
		  :key key))

(defun nsubstitute-if (newitem test sequence &key from-end (start 0) end count (key #'identity))
  (unless key (setq key #'identity))
  (with-sequence-start-end start end sequence
    (if from-end
	(do ((i (1- end) (1- i)))
	    ((< i start))
	  (when (funcall test (funcall key (elt sequence i)))
	    (setf (elt sequence i) newitem)
	    (when (and count (zerop (decf count)))
	      (return))))
      (do ((i start (1+ i)))
	  ((= i end))
	(when (funcall test (funcall key (elt sequence i)))
	  (setf (elt sequence i) newitem)
	  (when (and count (zerop (decf count)))
	    (return)))))
    sequence))

(defun nsubstitute-if-not (newitem test sequence &rest rest)
  (apply #'nsubstitute-if newitem (complement test) sequence rest))

(defun count (item sequence &key from-end test test-not (start 0) end (key #'identity))
  (count-if (seq-testfn item test test-not) sequence
	    :from-end from-end
	    :start start
	    :end end
	    :key key))

(defun count-if (test sequence &key from-end (start 0) end (key #'identity))
  (with-sequence-start-end start end sequence
    (do ((i start)
	 (count 0))
	((= i end) count)
      (let ((x (position-if test sequence :start i :end end :key key)))
	(unless x
	  (return count))
	(setq i (+ x 1))
	(incf count)))))

(defun count-if-not (test sequence &rest rest)
  (apply #'count-if (complement test) sequence rest))

(defun mismatch (sequence1 sequence2 &key from-end test test-not (key #'identity)
			   (start1 0) (start2 0) end1 end2)
  (unless key (setq key #'identity))
  (with-sequence-start-end start1 end1 sequence1
    (with-sequence-start-end start2 end2 sequence2
      (and test test-not
	   (error ":testと:test-notが両方指定されています"))
      (if test-not
	  (setq test #'(lambda (x y) (not (funcall test-not x y))))
	(unless test
	  (setq test #'eql)))
      (do* ((l1 (- end1 start1))
	    (l2 (- end2 start2))
	    (end (+ start1 (min l1 l2)))
	    (i start1 (1+ i))
	    (j start2 (1+ j)))
	  ((= i end)
	   (if (= l1 l2) nil i))
	(unless (funcall test
			 (funcall key (elt sequence1 i))
			 (funcall key (elt sequence2 j)))
	  (return i))))))

(defun merge (result-type sequence1 sequence2 predicate &key (key #'identity))
  (unless key (setq key #'identity))
  (do* ((l1 (length sequence1))
	(l2 (length sequence2))
	(l (+ l1 l2))
	(new-sequence (make-sequence result-type l))
	(i1 0)
	(i2 0)
	(j 0 (+ j 1)))
      ((cond ((= i1 l1)
	      (do ()
		  ((= i2 l2) 't)
		(setf (elt new-sequence j) (elt sequence2 i2))
		(incf i2)
		(incf j)))
	     ((= i2 l2)
	      (do ()
		  ((= i1 l1) 't)
		(setf (elt new-sequence j) (elt sequence1 i1))
		(incf i1)
		(incf j)))
	     (t 'nil))
       new-sequence)
    (let* ((x1 (elt sequence1 i1))
	   (x2 (elt sequence2 i2))
	   (y1 (funcall key x1))
	   (y2 (funcall key x2)))
      (cond ((funcall predicate y1 y2)
	     (setf (elt new-sequence j) x1)
	     (incf i1))
	    ((funcall predicate y2 y1)
	     (setf (elt new-sequence j) x2)
	     (incf i2))
	    (t
	     (setf (elt new-sequence j) x1)
	     (incf i1))))))
