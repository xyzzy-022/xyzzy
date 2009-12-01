;; Conway's `Life' for GNU Emacs
;; Copyright (C) 1988 Free Software Foundation, Inc.
;; Contributed by Kyle Jones, talos!kjones@uunet.uu.net

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide "life")

(defconstant life-patterns
  #(("@@@" " @@" "@@@")
    ("@@@ @@@" "@@  @@ " "@@@ @@@")
    ("@@@ @@@" "@@   @@" "@@@ @@@")
    ("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    ("@@@@@@@@@@")
    ("   @@@@@@@@@@       "
     "     @@@@@@@@@@     "
     "       @@@@@@@@@@   "
     "@@@@@@@@@@          "
     "@@@@@@@@@@          ")
    ("@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@" "@")
    ("@               @" "@               @"  "@               @"
     "@               @" "@               @"  "@               @"
     "@               @" "@               @"  "@               @"
     "@               @" "@               @"  "@               @"
     "@               @" "@               @"  "@               @")
    ("@@               " " @@              " "  @@             "
     "   @@            " "    @@           " "     @@          "
     "      @@         " "       @@        " "        @@       "
     "         @@      " "          @@     " "           @@    "
     "            @@   " "             @@  " "              @@ "
     "               @@")
    ("@@@@@@@@@" "@   @   @" "@ @@@@@ @" "@ @   @ @" "@@@   @@@" 
     "@ @   @ @" "@ @@@@@ @" "@   @   @" "@@@@@@@@@"))
  "Vector of rectangles containing some Life startup patterns.")

;; Macros are used macros for manifest constants instead of variables
;; because the compiler will convert them to constants, which should
;; eval faster than symbols.
;;
;; The (require) wrapping forces the compiler to eval these macros at
;; compile time.  This would not be necessary if we did not use macros
;; inside of macros, which the compiler doesn't seem to check for.
;;
;; Don't change any of the life-* macro constants unless you thoroughly
;; understand the `life-grim-reaper' function.

(defconstant life-life-char (char-code #\@))
(defconstant life-death-char (char-code #\A))
(defconstant life-birth-char (char-code #\C-c))
(defconstant life-void-char (char-code #\SPC))

(defconstant life-life-string "@")
(defconstant life-death-string "A")
(defconstant life-birth-string "")
(defconstant life-void-string " ")
(defconstant life-not-void-regexp (compile-regexp "[^ \n]"))

(defmacro increment (variable)
  `(setq ,variable (1+ ,variable)))

;; list of numbers that tell how many characters to move to get to
;; each of a cell's eight neighbors.
(defvar life-neighbor-deltas nil)

;; window display always starts here.  Easier to deal with than
;; (scroll-up) and (scroll-down) when trying to center the display.
(defvar life-window-start nil)

;; For mode line
(defvar life-current-generation nil)

(defun life-setup ()
  (let (n)
    ;; make sure (life-life-char) is used throughout
    (goto-char (point-min))
    (perform-replace life-not-void-regexp life-life-string nil t nil t)
    ;; center the pattern horizontally
    (goto-char (point-min))
    (setq n (truncate (- fill-column (save-excursion
				       (goto-eol)
				       (current-column)))
		      2))
    (while (not (eobp))
      (indent-to n)
      (forward-line))
    ;; center the pattern vertically
    (setq n (truncate (- (1- (window-height))
			 (progn
			   (goto-char (point-max))
			   (current-line-number)))
		      2))
    (goto-char (point-min))
    (insert #\LFD n)
    (goto-char (point-max))
    (insert #\LFD n)
    ;; pad lines out to fill-column
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (indent-to fill-column)
      (goto-column fill-column)
      (delete-region (point) (progn (end-of-line) (point)))
      (forward-line))
    ;; expand tabs to spaces
    (untabify (point-min) (point-max))
    ;; before starting be sure the automaton has room to grow
    (life-expand-plane-if-needed)
    ;; compute initial neighbor deltas
    (life-compute-neighbor-deltas)))

(defun life-start (sleeptime)
  (long-operation
    (life-setup)
    (life-display-generation sleeptime)
    (unwind-protect
	(while t
	  (life-grim-reaper)
	  (life-expand-plane-if-needed)
	  (life-increment-generation)
	  (life-display-generation sleeptime))
      (goto-char (point-min))
      (perform-replace life-not-void-regexp life-life-string nil t nil t))))

(defun life-compute-neighbor-deltas ()
  (setq life-neighbor-deltas
	(list -1 (- fill-column)
	      (- (1+ fill-column)) (- (+ 2 fill-column))
	      1 fill-column (1+ fill-column)
	      (+ 2 fill-column))))

(defun life-insert-random-pattern ()
  (dolist (l (svref life-patterns (random (length life-patterns))))
    (insert l)
    (insert #\LFD)))

(defun life-increment-generation ()
  (increment life-current-generation)
  (setq mode-line-format
	(format nil "   Life     generation ~d   (%m)  %P"
		life-current-generation)))

(defun life-grim-reaper ()
  (goto-char (point-min))
  ;; For speed declare all local variable outside the loop.
  (let (char pivot list)
    (unless (scan-buffer life-life-string)
      (life-extinct-quit))
    (while t
      (setq pivot (point))
      (setq list life-neighbor-deltas)
      (setq living-neighbors 0)
      (dolist (l list)
	(forward-char l)
	(setq char (char-code (following-char)))
	(cond ((= char life-void-char)
	       (delete-char 1)
	       (insert "\001"))
	      ((< char 3)
	       (delete-char 1)
	       (insert (code-char (1+ char))))
	      ((< char 9)
	       (delete-char 1)
	       (insert "\011"))
	      ((>= char life-life-char)
	       (increment living-neighbors)))
	(goto-char pivot))
      (if (member living-neighbors '(2 3))
	  (forward-char 1)
	(progn
	  (goto-char pivot)
	  (delete-char 1)
	  (insert life-death-string)))
      (unless (scan-buffer life-life-string)
	(return))))
  (goto-char (point-min))
  (perform-replace "\011" life-void-string nil nil nil t)
  (goto-char (point-min))
  (perform-replace "\001" life-void-string nil nil nil t)
  (goto-char (point-min))
  (perform-replace "\002" life-void-string nil nil nil t)
  (goto-char (point-min))
  (perform-replace life-birth-string life-life-string nil nil nil t)
  (goto-char (point-min))
  (perform-replace life-death-string life-void-string nil nil nil t))


(defun life-expand-plane-if-needed ()
  (catch 'done
    (goto-char (point-min))
    (while (not (eobp))
      ;; check for life at beginning or end of line.  If found at
      ;; either end, expand at both ends,
      (cond ((or (looking-for life-life-string)
		 (progn
		   (goto-eol)
		   (forward-char -1)
		   (looking-for life-life-string)))
	     (goto-char (point-min))
	     (while (not (eobp))
	       (insert life-void-string)
	       (goto-eol)
	       (insert life-void-string)
	       (forward-char))
	     (setq fill-column (+ 2 fill-column))
	     (scroll-left 1)
	     (life-compute-neighbor-deltas)
	     (throw 'done t)))
      (forward-line)))
  (goto-char (point-min))
  ;; check for life within the first two lines of the buffer.
  ;; If present insert two lifeless lines at the beginning..
  (forward-line 2)
  (forward-char -1)
  (cond ((scan-buffer life-life-string :reverse t)
	 (goto-char (point-min))
	 (insert life-void-string fill-column)
	 (insert #\LFD)
	 (insert life-void-string fill-column)
	 (insert #\LFD)
	 (setq life-window-start (+ life-window-start 2))))
  (goto-char (point-max))
  ;; check for life within the last two lines of the buffer.
  ;; If present insert two lifeless lines at the end.
  (forward-line -2)
  (cond ((scan-buffer life-life-string)
	 (goto-char (point-max))
	 (insert life-void-string fill-column)
	 (insert #\LFD)
	 (insert life-void-string fill-column)
	 (insert #\LFD)
	 (setq life-window-start (+ life-window-start 2)))))

(defun life-display-generation (sleeptime)
  (goto-line life-window-start)
;  (recenter 0)
;  (clear-message-line)
  (message "C-gÇâüÇπÇŒèIÇÌÇÈ")
;  (refresh-screen)
  (sit-for sleeptime))

(defun life-extinct-quit ()
  (life-display-generation 0)
  (quit))

(defun life (&optional (sleeptime 0.1))
  (interactive "p")
  (life-mode t)
  (life-insert-random-pattern)
  (life-start sleeptime))

(defun life-mode (&optional non-interactive)
  (interactive)
  (switch-to-buffer "*Life*")
  (erase-buffer (selected-buffer))
  (kill-all-local-variables)
  (set-buffer-fold-width nil)
  (setq need-not-save t)
  (make-local-variable 'auto-save)
  (setq auto-save nil)
  (setq mode-name "Life")
  (setq buffer-mode 'life-mode)
  (setq life-current-generation 0)
  (setq fill-column (1- (window-width)))
  (setq life-window-start 1)
  (make-local-variable 'mode-line-format)
  (setq kept-undo-information nil)
  (setq indent-tabs-mode nil)
  (use-keymap life-mode-map)
  (unless non-interactive
    (setq buffer-read-only t)))

(defvar life-mode-map nil)
(unless life-mode-map
  (setq life-mode-map (make-sparse-keymap))
  (define-key life-mode-map #\C-p 'life-previous-line)
  (define-key life-mode-map #\C-n 'life-next-line)
  (define-key life-mode-map #\C-f 'life-forward-char)
  (define-key life-mode-map #\C-b 'life-backward-char)
  (define-key life-mode-map #\p 'life-previous-line)
  (define-key life-mode-map #\n 'life-next-line)
  (define-key life-mode-map #\f 'life-forward-char)
  (define-key life-mode-map #\b 'life-backward-char)
  (define-key life-mode-map #\Up 'life-previous-line)
  (define-key life-mode-map #\Down 'life-next-line)
  (define-key life-mode-map #\Right 'life-forward-char)
  (define-key life-mode-map #\Left 'life-backward-char)
  (define-key life-mode-map #\SPC 'life-plot-char)
  (define-key life-mode-map #\RET 'life-plot-char)
  (define-key life-mode-map #\DEL 'life-delete-char)
  (define-key life-mode-map #\C-d 'life-delete-char)
  (define-key life-mode-map #\Delete 'life-delete-char)
  (define-key life-mode-map #\C-h 'life-delete-backward-char)
  (define-key life-mode-map #\q 'life-end-edit)
  (define-key life-mode-map #\x 'life-end-edit))

(defun life-goto-column (column)
  (goto-column column)
  (unless (= (current-column) column)
    (let ((buffer-read-only nil))
      (declare (special buffer-read-only))
      (indent-to column))))

(defun life-previous-line ()
  (interactive)
  (let ((column (current-column)))
    (unless (forward-line -1)
      (goto-bol)
      (let ((buffer-read-only nil))
	(declare (special buffer-read-only))
        (open-line)))
    (life-goto-column column)))

(defun life-next-line ()
  (interactive)
  (let ((column (current-column)))
    (unless (forward-line 1)
      (goto-eol)
      (let ((buffer-read-only nil))
	(declare (special buffer-read-only))
        (insert #\LFD)))
    (life-goto-column column)))

(defun life-forward-char ()
  (interactive)
  (if (eolp)
      (let ((buffer-read-only nil))
	(declare (special buffer-read-only))
        (insert #\SPC))
    (forward-char 1)))

(defun life-backward-char ()
  (interactive)
  (if (bolp)
      (let ((buffer-read-only nil))
	(declare (special buffer-read-only))
        (save-excursion
          (indent-rigidly (point-min)(point-max)))
        (goto-bol))
    (forward-char -1)))

(defun life-plot-char ()
  (interactive)
  (let ((buffer-read-only nil))
    (declare (special buffer-read-only))
    (unless (eolp)
      (delete-char))
    (insert #\@)))

(defun life-delete-char ()
  (interactive)
  (unless (eolp)
    (let ((buffer-read-only nil))
      (declare (special buffer-read-only))
      (delete-char)
      (insert #\SPC))))

(defun life-delete-backward-char ()
  (interactive)
  (life-backward-char)
  (life-delete-char))

(defun life-end-edit (&optional (sleeptime 1))
  (interactive "p")
  (setq buffer-read-only nil)
  (goto-char (point-max))
  (insert #\LFD)
  (goto-char (point-min))
  (while (not (eobp))
    (goto-eol)
    (delete-horizontal-spaces)
    (forward-line))
  (goto-char (point-min))
  (while (not (eobp))
    (if (eolp)
	(delete-char 1)
      (return)))
  (goto-char (point-max))
  (forward-line -1)
  (while (not (bobp))
    (if (eolp)
	(progn
	  (delete-backward-char)
	  (goto-bol))
      (return)))
  (goto-char (point-min))
  (let (indent)
    (while (not (eobp))
      (skip-chars-forward " ")
      (setq indent (or (and indent
			    (min indent (current-column)))
		       (current-column)))
      (forward-line))
    (indent-rigidly (point-min) (point-max) (- indent)))
  (life-start sleeptime))

