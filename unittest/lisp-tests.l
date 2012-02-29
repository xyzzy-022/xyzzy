;; need docstring support!
;;"(previous-pseudo-frame) caused \"appframe not found\" error." 
(deftest fix-previous-pseudo-frame-test ()
 (previous-pseudo-frame)
 => nil)

;; "(list-all-packages) should return 'a copy of' system package list."
(deftest fix-list-all-packages-test ()
  (let* ((name :temporary-package-for-test)
	 (pkg (make-package name))
	 (pkglist (list-all-packages)))
    (delete pkg pkglist)
    (eq pkg (find-package name)))
  => t)
