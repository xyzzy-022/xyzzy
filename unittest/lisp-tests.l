;; need docstring support!
(deftest fix-previous-pseudo-frame-test ()
  "(previous-pseudo-frame) caused \"appframe not found\" error."
  (previous-pseudo-frame)
  => nil)

(defun make-test-package (&rest options)
  (let ((name (format nil "temporary-package-for-test-~D" (test-uid))))
    (apply #'make-package name options)))

(deftest fix-list-all-packages-test ()
  "(list-all-packages) should return 'a copy of' system package list."
  (let* ((pkg (make-test-package))
         (name (package-name pkg))
	 (pkglist (list-all-packages)))
    (delete pkg pkglist)
    (eq pkg (find-package name)))
  => t)

;;; Test scroll-other-window or similar functions.
;;; When there is only one window, they should not raise an error.
(deftest fix-scroll-other-window-test ()
  "(scroll-other-window) shouldn't raise an error
even if there is only one window."
  (save-window-excursion
    (delete-other-windows)
    (handler-case
	(progn (scroll-other-window) t)
      (range-error (c) nil))))

(deftest fix-scroll-up-other-window-test ()
  "(scroll-up-other-window) shouldn't raise an error
even if there is only one window."
  (save-window-excursion
    (delete-other-windows)
    (handler-case
	(progn (scroll-up-other-window) t)
      (range-error (c) nil))))

(deftest fix-scroll-down-other-window-test ()
  "(scroll-down-other-window) shouldn't raise an error
even if there is only one window."
  (save-window-excursion
    (delete-other-windows)
    (handler-case
	(progn (scroll-down-other-window) t)
      (range-error (c) nil))))

(deftest fix-scroll-up-both-window-test ()
  "(scroll-up-both-window) shouldn't raise an error
even if there is only one window."
  (save-window-excursion
    (delete-other-windows)
    (handler-case
	(progn (scroll-up-both-window) t)
      (range-error (c) nil))))

(deftest fix-scroll-down-both-window-test ()
  "(scroll-down-both-window) shouldn't raise an error
even if there is only one window."
  (save-window-excursion
    (delete-other-windows)
    (handler-case
	(progn (scroll-down-both-window) t)
      (range-error (c) nil))))


(defmacro with-lispmode-bed (&body body)
  `(with-temp-buffer
     (lisp-mode)
     ,@body))

(defmacro user::smart-indentation-test (&whole whole &environment env vars &body body)
  `(prog ,vars ,@body))

(deftest lispmode-smart-indentation-for-macro-form-test ()
  "smart indentation for macro function symbol without
lisp-indent-hook property"
  (let ((*lisp-body-indent* 2)
	(*lisp-body-indention* 2))
    (with-lispmode-bed
      (setq *buffer-package* "user")
      (insert "(smart-indentation-test (a b)")
      (goto-char 23)
      #0=(lisp-newline-and-indent)
      (values #1=(current-column)
	      (and (goto-char (point-max)) #0# #1#))))
  => 4
  => 2)
