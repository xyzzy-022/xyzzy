;; need docstring support!
(deftest fix-previous-pseudo-frame-test ()
  "(previous-pseudo-frame) caused \"appframe not found\" error."
  (previous-pseudo-frame)
  => nil)

(deftest fix-list-all-packages-test ()
  "(list-all-packages) should return 'a copy of' system package list."
  (let* ((name :temporary-package-for-test)
	 (pkg (make-package name))
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
