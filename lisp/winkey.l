;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "winkey")

(in-package "editor")

(defvar *default-selection-keymap*
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap #\C-c 'copy-selection-to-clipboard)
    (define-key keymap #\C-x 'kill-selection-to-clipboard)
    keymap))

(setq-default selection-keymap *default-selection-keymap*)
(global-set-key #\C-z 'undo)
(global-set-key #\C-v 'paste-from-clipboard)
(define-key esc-map #\C-x #'ctl-x-prefix)
(define-key esc-map #\C-c #'specific-command-prefix)
