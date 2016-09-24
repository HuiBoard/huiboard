;; Copyright (c) 2016 Joshua Miller

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:huim)

(defvar *serial-port-path* #P"/dev/ttyACM0")

(defvar *dictionary-file-path* #P"/usr/share/dict/american-english")

;; one chords
;; all below are subject to change

(menu "p" #\+)
(menu "i" ".")
(menu "m" "-")
(menu "d" "/")
(menu "v" #\")
(menu "o" #\()
(menu "c" #\))
(menu "b" #\\)
(menu "h" "")
(menu "q" "?")
(menu "l" ":")

(menu "z" "0")
(menu "n" "1")
(menu "t" "2")
(menu "h" "3")
(menu "r" "4")
(menu "f" "5")
(menu "x" "6")
(menu "s" "7")
(menu "g" "8")
(menu "k" "9")

;; (defun upkey () (key "Up"))
;; (menu "u" 'upkey)
;; (defun downkey () (key "Down"))
;; (menu "d" 'downkey)
;; (defun rightkey () (key "Right"))
;; (menu "r" 'rightkey)
;; (defun leftkey () (key "Left"))
;; (menu "l" 'leftkey)

;; two chords
(menu "cm" ",")
(menu "at" "@")
(menu "ex" "!")
(menu "ht" "#")
(menu "td" "~")
(menu "bq" "`")
(menu "sq" "'")
(menu "lc" "<")
(menu "rc" ">")
(menu "sc" ";")
(menu "ds" "$")
(menu "eq" "=")
(menu "as" "*")
(menu "ca" "^")
(menu "pc" "%")
(menu "am" "&")
(menu "us" "_")
(menu "ql" "{")
(menu "qr" "}")
(menu "bl" "[")
(menu "br" "]")
(menu "sp" "|")

(defun tab () (key "Tab"))
(menu "tb" 'tab)

(defun caps-lock () (key "Caps_Lock"))
(menu "uc" 'caps-lock)

(defun ctrl+s () (key "ctrl+s"))
(menu "sr" 'ctrl+s)

(defun page-up () (key "Prior"))
(menu "pu" 'page-up)

(defun page-down () (key "Next"))
(menu "pd" 'page-down)

(defun doc-home () (key "ctrl+Home"))
(menu "tp" 'doc-home)

(defun doc-end () (key "ctrl+End"))
(menu "bm" 'doc-end)

(defun home () (key "Home"))
(menu "lh" 'home)

(defun end () (key "End"))
(menu "le" 'end)

(defun copy () (key "ctrl+c"))
(menu "cp" 'copy)

(defun paste () (key "ctrl+p"))
(menu "ps" 'paste)

(defun cut () (key "ctrl+x"))
(menu "xp" 'cut)

(defun trademark () (key "trademark"))
(menu "tm" 'trademark)

(defun select-line () (key "Home Shift+End"))
(menu "sl" 'select-line)

(defun kill-buffer-chromium () (key "ctrl+w"))
(menu "kb" 'kill-buffer-chromium "chromium")

(defun kill-buffer-emacs () (key "ctrl+x k"))
(menu "kb" 'kill-buffer-emacs "emacs")

;; three chords

(defun heart () (key "heart"))
(menu "hrt" 'heart)

;; four chords

(defun diamond () (key "diamond"))
(menu "dmnd" 'diamond)

(defun copyright () (key "copyright"))
(menu "cprt" 'copyright)

