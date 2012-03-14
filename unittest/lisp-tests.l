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

(let ((os-version (list (os-major-version) (os-minor-version)))
      (os-features '(:windows-8 :windows-7 :windows-vista :windows-xp :windows-2000)))
  ;; Windows Vista
  (when (equalp '(6 0) os-version)
    (deftest os-features-vista-test ()
      "*features* contains :windows-vista, :windows-xp, :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (nil nil t t t))
    (deftest os-platform-vista-test ()
      "(os-platform) should return :windows-vista"
      (os-platform)
      => windows-vista)
    (deftest user-config-path-and-dump-image-path-vista-test ()
      "(user-config-path) and (si:dump-image-path) ‚Í wxp ‚Ì‚Ü‚Ü"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "wxp"
      => "wxp"))
  ;; Windows 7
  (when (equalp '(6 1) os-version)
    (deftest os-features-7-test ()
      "*features* contains :windows-7, :windows-vista, :windows-xp, :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (nil t t t t))
    (deftest os-platform-7-test ()
      "(os-platform) should return :windows-7"
      (os-platform)
      => windows-7)
    (deftest user-config-path-and-dump-image-path-7-test ()
      "(user-config-path) and (si:dump-image-path) ‚Í wxp ‚Ì‚Ü‚Ü"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "wxp"
      => "wxp"))
  ;; Windows 8
  (when (equalp '(6 2) os-version)
    (deftest os-features-8-test ()
      "*features* contains :windows-8, :windows-7, :windows-vista, :windows-xp, :windows-2000, :windows-nt"
      (mapcar #'(lambda (k) (not (null (featurep k)))) os-features)
      => (t t t t t))
    (deftest os-platform-8-test ()
      "(os-platform) should return :windows-8"
      (os-platform)
      => windows-8)
    (deftest user-config-path-and-dump-image-path-8-test ()
      "(user-config-path) and (si:dump-image-path) ‚Í wxp ‚Ì‚Ü‚Ü"
      (values (pathname-name (user-config-path))
              (pathname-type (si:dump-image-path)))
      => "wxp"
      => "wxp"))
  )

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
