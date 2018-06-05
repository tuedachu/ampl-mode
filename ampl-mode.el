;;; ampl-mode.el --- Emacs major mode for AMPL files. -*- lexical-binding: t -*-

;; Copyright (c) Arnaud Hoffmann <tuedachu@gmail.com>

;; Author: Arnaud Hoffmann  <tuedachu@gmail.com>
;; URL: https://github.com/tuedachu/ampl-mode
;; Version: 1.0
;; Package-Requires: rx
;; Keywords: AMPL, major mode

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

(defgroup ampl-mode nil
  "Interface to ampl-mode."
  :group 'external)

(defcustom ampl-command "ampl"
  "Path to AMPL executable."
  :type 'string
  :group 'ampl-mode)

(defcustom ampl-indent-width 4
  "Indentation width used in function `ampl-indent-line' and
`ampl-indent-buffer'."
  :type 'integer
  :group 'ampl-mode)

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
    (modify-syntax-entry ?_ "w" st) ;; words can contain '_'
    (modify-syntax-entry ?# "<" st) ;; '#' starts a single line comment
    (modify-syntax-entry ?\n ">" st) ;; '\n' ends a single line comment
    (modify-syntax-entry ?' "|" st) ;; Single-quoted strings
    st)
  "Syntax table for ampl mode.")

(defun ampl-indent-line ()
  "Indent the current line based on the two previous lines in the
buffer."
  (interactive)
  (let ((indentation 0)
        current-line)
    (line-beginning-position)
    (if (bobp)
        (indent-line-to 0)
      (save-excursion
        (setq current-line (car (get-line-as-list)))
        (previous-line)
        (line-beginning-position)
        (while (and (not (bobp))
                    (not (get-line-as-list)))
          (previous-line)
          (line-beginning-position))
        (end-of-line)
        (setq indentation (ampl-find-indentation
                           (current-indentation)))
        (when (not (bobp))
          (previous-line)
          (when (ampl-remove-indent?)
            (setq indentation
                  (- indentation ampl-indent-width))))
        (when (string= current-line "}")
          (setq indentation
                (- indentation ampl-indent-width))))
      (save-excursion
        (indent-line-to (max 0 indentation))))))

(defun ampl-indent-buffer ()
  "Indent the entier AMPL buffer.

This function is slightly more efficient than `indent-region'
applied from `point-min' to `point-max'."
  (interactive)
  (with-timer "indentation"
    (save-excursion
      (goto-char (point-min))
      (indent-line-to 0)
      (end-of-line)
      (let (indentation
            current-line
            (remove-indent-after-next-line nil))
        (while (not (eobp))
          (setq indentation (current-indentation))
          (when remove-indent-after-next-line
            (setq indentation (- indentation ampl-indent-width)))
          (setq already-indented nil
                indentation (ampl-find-indentation indentation)
                remove-indent-after-next-line (ampl-remove-indent?))
          (next-line)
          (setq current-line (car (get-line-as-list)))
          (when (string= current-line "}")
            (setq indentation
                  (- indentation ampl-indent-width)))
          (indent-line-to (max 0 indentation))
          (end-of-line))))))

(defun ampl-remove-indent? ()
  (if (and (looking-back (rx (or
                              (and ":="
                                   (0+ space)
                                   line-end)
                              (and ":"
                                   (0+ space)
                                   line-end))))
           (not (search-backward
                 "#"
                 (line-beginning-position)
                 t)))
      t
    nil))

(defun ampl-find-indentation (current-indentation)
  "Find the indentation to apply to the next line based on the
the indentation of the current line CURRENT-INDENTATION."
  (let ((indentation current-indentation))
    (cond ((and (looking-back (rx (or
                                   (and ":="
                                        (0+ space)
                                        line-end)
                                   (and ":"
                                        (0+ space)
                                        line-end)
                                   (and "{"
                                        (0+ space)
                                        line-end))))
                (not (search-backward
                      "#"
                      (line-beginning-position)
                      t)))
           (setq indentation
                 (+ ampl-indent-width indentation)))
          ((and (looking-back (rx (and "}"
                                       (0+ space)
                                       line-end)))
                (not (string= (car (get-line-as-list))
                              "}"))
                (not (search-backward
                      "#"
                      (line-beginning-position)
                      t)))
           (setq indentation
                 (- indentation ampl-indent-width))))
    indentation))


(defun ampl-mode ()
  "Major mode for editing ampl source code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ampl-mode-keymap)
  (set (make-local-variable 'font-lock-defaults) '(ampl-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'ampl-indent-line)
  ;;  (add-hook 'before-save-hook 'ampl-indent-buffer nil 'local)
  (set-syntax-table ampl-mode-syntax-table)
  (setq major-mode 'ampl-mode)
  (setq mode-name "AMPL")
  (setq indent-tabs-mode nil)
  (run-hooks 'ampl-mode-hooks)
  (message "Opening AMPL mode..."))

(provide 'ampl-mode)

(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))

;;; ampl-mode.el ends here
