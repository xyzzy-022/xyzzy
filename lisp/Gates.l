;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

;;;
;;; Windows 風キーバインドサンプル
;;;

(provide "Gates")

(in-package "editor")

; files
(global-set-key #\C-n 'new-file)
(global-set-key #\C-o 'open-file-dialog)
(global-set-key #\C-s 'save-buffer-dialog)
(global-set-key #\C-p 'print-selected-buffer-dialog)
(global-set-key #\M-F4 'kill-xyzzy)      ; Alt+F4

; edit
(global-set-key #\C-z 'undo)
(global-set-key #\C-y 'redo)
(global-set-key #\C-x 'kill-selection-to-clipboard)
(global-set-key #\C-c 'copy-selection-to-clipboard)
(global-set-key #\C-v 'paste-from-clipboard)
(global-set-key #\C-a 'selection-whole-buffer)

; search
(global-set-key #\C-f 'search-dialog)
(global-set-key #\C-r 'replace-dialog)
(global-set-key #\F3 'repeat-forward-search)
(global-set-key #\S-F3 'repeat-backward-search)

; move
; chars
(global-set-key #\S-Left 'selection-backward-char)
(global-set-key #\S-Right 'selection-forward-char)
; lines
(global-set-key #\S-Up 'selection-previous-virtual-line)
(global-set-key #\S-Down 'selection-next-virtual-line)
; bol/eol
(global-set-key #\Home 'beginning-of-virtual-line)
(global-set-key #\End 'end-of-virtual-line)
(global-set-key #\S-Home 'selection-beginning-of-virtual-line)
(global-set-key #\S-End 'selection-end-of-virtual-line)
; words
(global-set-key #\C-Left 'backward-word)
(global-set-key #\C-Right 'forward-word)
(global-set-key #\S-C-Left 'selection-backward-word)
(global-set-key #\S-C-Right 'selection-forward-word)
; page
(global-set-key #\S-PageUp 'selection-previous-page)
(global-set-key #\S-PageDown 'selection-next-page)
; bob/eob
(global-set-key #\C-Home 'beginning-of-buffer)
(global-set-key #\C-End 'end-of-buffer)
(global-set-key #\S-C-Home 'selection-beginning-of-buffer)
(global-set-key #\S-C-End 'selection-end-of-buffer)

; buffers
(global-set-key #\C-F4 'close-selected-buffer)
(global-set-key #\S-C-F24 'previous-buffer)       ; Shift+Ctrl+Tab
(global-set-key #\C-F24 'next-buffer)             ; Ctrl+Tab
(set-extended-key-translate-table exkey-S-C-tab #\S-C-F24)
(set-extended-key-translate-table exkey-C-tab #\C-F24)
