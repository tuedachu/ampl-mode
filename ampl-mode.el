;;; ampl-mode.el --- Emacs major mode for AMPL files. -*- lexical-binding: t -*-

;; Copyright (C) Arnaud Hoffmann <tuedachu@gmail.com>

;; Author: Arnaud Hoffmann  <tuedachu@gmail.com>
;; URL: https://github.com/tuedachu/ampl-mode
;; Version: 1.0
;; Package-Requires:
;; Keywords: ampl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Emacs major mode for ampl files

;;; Code:
;;

(require 'rx)

(defcustom ampl-command "ampl"
  "Path to AMPL executable.")

(defvar ampl-mode-hooks nil
  "List of functions being executed after loading `ampl-mode'.")

(defvar ampl-mode-keymap
  (let ((map (make-keymap)))
    (define-key map (kbd "<f5>") 'ampl-mode-run-ampl)
    map)
  "Keymap for ampl major mode.")

(defun ampl-mode-run-ampl ()
  (interactive)
  (setq filename (read-from-minibuffer "ampl file:"
                                       (buffer-file-name)))
  (message (with-temp-buffer
             (call-process
              ampl-command
              nil t nil
              filename)
             (buffer-string))))

(defun ampl-create-regexp-main-keyword (keywords)
  "Create a regexp for a list of keywords KEYWORDS that meets
AMPL grammar rules."
  (mapconcat
   (lambda (keyword)
     (replace-regexp-in-string "keyword" keyword
                               (rx (and word-start "keyword"
                                        word-end
                                        (or (1+ space)
                                            line-end
                                            ";")))))
   keywords
   "\\|"))


(defconst ampl-mode-keywords-1
  (list `(,(ampl-create-regexp-main-keyword '("include"
                                              "data"
                                              "model"
                                              "reset"
                                              "solve"
                                              "option"
                                              "display"
                                              "for"
                                              "if"
                                              "repeat[[:space:]]+while"
                                              "let"
                                              "fix"
                                              "param"
                                              "set"
                                              "var"
                                              "integer"
                                              "binary"
                                              "dimen"
                                              "in"
                                              "maximize"
                                              "ordered[[:space:]]+by"
                                              "subject[[:space:]]+to"))
          . font-lock-keyword-face)
        `(,(regexp-opt '("hello" "MOLES" "MASS" "SPLIT" "STORE") 'symbols) .  font-lock-type-face)
        `(,(regexp-opt '("NAME" "Name" "COMP" "Comp") 'symbols) .  font-lock-function-name-face))
  "First order keywords in ampl- mode")

(defvar ampl-font-lock-keywords ampl-mode-keywords-1)

(defvar ampl-mode-syntax-table
  (let (( st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?' "|" st)
    st)
  "Syntax table for ampl mode.")

(defun ampl-mode ()
  "Major mode for editing ampl source code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ampl-mode-keymap)
  (set-syntax-table ampl-mode-syntax-table)
  (setq major-mode 'ampl-mode)
  (setq mode-name "AMPL")
  (run-hooks 'ampl-mode-hooks)
  (message "Opening AMPL mode..."))

(provide 'ampl-mode)

;;; ampl-mode.el ends here
