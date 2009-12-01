;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "cmdbar")

(in-package "editor")

(export '(find-command-bar define-command-bar
	  show-command-bar hide-command-bar
	  delete-command-bar
	  std-tool-bar buffer-bar))

(define-history-variable *command-bar-list* nil)
(defvar *defer-define-command-bar-list* nil)
(defvar *command-bar-loaded* nil)

;;  0        1        2         3         4    5 6
;; (pkg-name sym-name disp-name visible-p edge x y)
(defmacro command-bar-package (bar) `(car ,bar))
(defmacro command-bar-symbol-name (bar) `(cadr ,bar))
(defmacro command-bar-display-name (bar) `(caddr ,bar))
(defmacro command-bar-visible-p (bar) `(nth 3 ,bar))
(defmacro command-bar-position (bar) `(cdr (nthcdr 3 ,bar)))
(defmacro command-bar-edge (bar) `(nth 4 ,bar))
(defmacro command-bar-xy (bar) `(cdr (nthcdr 4 ,bar)))

(defun find-command-bar (sym)
  (let ((pkg (package-name (symbol-package sym)))
	(name (symbol-name sym)))
    (find-if #'(lambda (x)
		 (and (equal (car x) pkg)
		      (equal (cadr x) name)))
	     *command-bar-list*)))

(defun command-bar-update (bar)
  (if (command-bar-visible-p bar) :check))

(defun define-command-bar (sym name &optional edge x y w)
  (cond ((null *command-bar-loaded*)
	 (setq *defer-define-command-bar-list*
	       (nconc (delete sym *defer-define-command-bar-list*
			      :test #'eq :key #'car)
		      (list (list sym name edge x y w)))))
	((not (find-command-bar sym))
	 (let ((bar (list (package-name (symbol-package sym))
			  (symbol-name sym) name nil edge x y w)))
	   (setq *command-bar-list* (nconc *command-bar-list* (list bar)))
	   (add-menu-item *command-bar-popup-menu* bar name
			  #'(lambda () (interactive) (toggle-command-bar-1 bar sym))
			  #'(lambda () (command-bar-update bar))))))
  t)

(defun show-command-bar-1 (bar sym)
  (unless (tool-bar-exist-p sym)
    (funcall sym))
  (apply #'show-tool-bar sym (command-bar-position bar))
  (setf (command-bar-visible-p bar) t)
  t)

(defun hide-command-bar-1 (bar sym)
  (let ((r (multiple-value-list (hide-tool-bar sym))))
    (when (car r)
      (setf (command-bar-position bar) r)
      (setf (command-bar-visible-p bar) nil)))
  t)

(defun toggle-command-bar-1 (bar sym)
  (if (command-bar-visible-p bar)
      (hide-command-bar-1 bar sym)
    (show-command-bar-1 bar sym)))

(defun delete-command-bar-1 (bar sym)
  (when (tool-bar-exist-p sym)
    (delete-tool-bar sym))
  (delete-menu *command-bar-popup-menu* bar nil)
  (setq *command-bar-list* (delete bar *command-bar-list*))
  t)

(defun show-command-bar (sym)
  (let ((bar (find-command-bar sym)))
    (and bar (show-command-bar-1 bar sym))))

(defun hide-command-bar (sym)
  (let ((bar (find-command-bar sym)))
    (and bar (hide-command-bar-1 bar sym))))

(defun toggle-command-bar (sym)
  (let ((bar (find-command-bar sym)))
    (and bar (toggle-command-bar-1 bar sym))))

(defun named-command-bar-update (bar)
  (if (find-package (command-bar-package bar))
      (if (command-bar-visible-p bar) :check)
    :disable))

(defun toggle-named-command-bar (bar)
  (let ((pkg (find-package (command-bar-package bar))))
    (when pkg
      (toggle-command-bar-1 bar (intern (command-bar-symbol-name bar) pkg)))))

(defun delete-command-bar (sym)
  (let ((bar (find-command-bar sym)))
    (and bar (delete-command-bar-1 bar sym))))

(defun save-command-bar-info ()
  (mapc #'(lambda (edge list)
	    (mapc #'(lambda (info)
		      (let ((bar (find-command-bar (car info))))
			(when bar
			  (setf (command-bar-edge bar) edge)
			  (setf (command-bar-xy bar) (cdr info)))))
		  list))
	'(:left :top :right :bottom) (list-tool-bars)))

(defun load-command-bar-info ()
  (setq *command-bar-loaded* t)
  (or *command-bar-popup-menu*
      (setq *command-bar-popup-menu* (create-popup-menu)))
  (mapc #'(lambda (bar)
	    (ignore-errors
	     (let* ((pkg (find-package (command-bar-package bar)))
		    (sym (and pkg (intern (command-bar-symbol-name bar) pkg)))
		    (disp-name (command-bar-display-name bar)))
	       (cond (sym
		      (let ((x (find sym *defer-define-command-bar-list* :key #'car)))
			(when x
			  (setq disp-name (cadr x))))
		      (add-menu-item *command-bar-popup-menu* bar disp-name
				     #'(lambda ()
					 (interactive)
					 (toggle-command-bar-1 bar sym))
				     #'(lambda () (command-bar-update bar)))
		      (when (command-bar-visible-p bar)
			(setf (command-bar-visible-p bar) nil)
			(show-command-bar-1 bar sym)))
		     (t
		      (add-menu-item *command-bar-popup-menu* bar disp-name
				     #'(lambda ()
					 (interactive)
					 (toggle-named-command-bar bar))
				     #'(lambda () (named-command-bar-update bar))))))))
	*command-bar-list*)
  (mapc #'(lambda (x) (apply #'define-command-bar x))
	*defer-define-command-bar-list*))

(add-hook '*save-history-hook* #'save-command-bar-info)
(add-hook '*load-history-hook* #'load-command-bar-info)

(define-command-bar 'std-tool-bar "標準(&S)")
(define-command-bar 'buffer-bar "バッファ(&B)")

(setf (symbol-function 'buffer-bar) #'create-buffer-bar)

(defun tool-bar-update-search ()
  (or *last-search-p* :disable))

(defun std-tool-bar ()
  (create-tool-bar
   'std-tool-bar
   (merge-pathnames "toolbar.bmp" (etc-path))
   '(("新規作成" 24 new-file)
     ("開く" 22 open-file-dialog)
     ("上書き保存" 20 save-buffer-dialog :modified)
     ("全て保存" 75 save-all-buffers)
     :sep
     ("印刷" 29 print-selected-buffer-dialog)
     :sep
     ("切り取り" 7 kill-selection-to-clipboard :modify-any-selection)
     ("コピー" 9 copy-selection-to-clipboard :any-selection)
     ("貼り付け" 2 paste-from-clipboard :clipboard)
     :sep
     ("元に戻す" 0 undo :undo)
     ("やっぱり元に戻さない" 1 redo :redo)
     :sep
     ("検索" 5 search-dialog)
     ("前を検索" 3 repeat-backward-search tool-bar-update-search)
     ("次を検索" 4 repeat-forward-search tool-bar-update-search)
     :sep
     ("grep" 74 grep-dialog)
     :sep
     ("折り返さない" 76
      set-buffer-fold-type-none set-buffer-fold-type-none-update)
     ("指定位置で折り返す" 77
      set-buffer-fold-type-column set-buffer-fold-type-column-update)
     ("ウィンドウ幅で折り返す" 78
      set-buffer-fold-type-window set-buffer-fold-type-window-update)
    )))
