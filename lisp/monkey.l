;;; -*-Lisp-*-
;;;
;;; This file is part of xyzzy.
;;;

;; ラベルの数を12個に設定(4, 5, 8, 10, 12, 15のどれか)
(set-number-of-function-bar-labels 12)

;; ラベルの文字を設定
(set-function-bar-label #\F1 "VCヘルプ")
(set-function-bar-label #\F2 "バッファ")
(set-function-bar-label #\F3 "InfoView")
(set-function-bar-label #\F4 "タグジャンプ")
(set-function-bar-label #\F5 "次検索/下")
(set-function-bar-label #\F6 "行ブロック")
(set-function-bar-label #\F7 "削除")
(set-function-bar-label #\F8 "コピー")
(set-function-bar-label #\F9 "ペースト")
(set-function-bar-label #\F10 "エラー")
(set-function-bar-label #\F11 "次エラー")
(set-function-bar-label #\F12 "Redo")

;; Shift+?
(set-function-bar-label #\S-F5 "次検索/上")
(set-function-bar-label #\S-F6 "文字ブロック")
(set-function-bar-label #\S-F9 "矩形ペースト")

;; Control+?
(set-function-bar-label #\C-F1 "ヘルプ")
(set-function-bar-label #\C-F2 "マーク")

;; おまけ
;; Shift+Control+?
(set-function-bar-label #\S-C-F1 "S-C-F1")
;; Alt+?
(set-function-bar-label #\M-F2 "M-F2")
;; Alt+Control+?
(set-function-bar-label #\C-M-F3 "C-M-F3")
;; Alt+Shift+?
(set-function-bar-label #\S-M-F4 "S-M-F4")
;; Alt+Control+Control+?
(set-function-bar-label #\S-C-M-F5 "S-M-C-F5")

;; nilで消える
(set-function-bar-label #\M-F2 nil)
