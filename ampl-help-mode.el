;;; ampl-help-mode.el -*- lexical-binding: t -*-

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

(defvar ampl-help-map (let ((map (make-keymap)))
                        (define-key map (kbd "q") 'phz-help-close)
                        (define-key map (kbd "n") 'phz-help-next-button)
                        (define-key map (kbd "p") 'phz-help-previous-button)
                        map)
  "Keybinding for AMPL help  mode")

(defun ampl-help-next-button ()
  (interactive)
  (let ((next (next-button (point))))
    (if next
	(goto-char (button-start next))
      (goto-char (point-min))
      (goto-char (button-start (next-button (point)))))))

(defun ampl-help-previous-button ()
  (interactive)
  (let ((previous (previous-button (point))))
    (if previous
	(goto-char (button-start previous))
      (goto-char (point-max))
      (goto-char (button-start (previous-button (point)))))))

(defun ampl-help-close ()
  (interactive)
  (kill-buffer)
  (delete-other-windows))

(defun ampl-help-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map ampl-help-map)
  (setq buffer-read-only t)
  (setq mode-name "AMPL help"))

(provide 'ampl-help-mode)

;; ampl-help-mode.el ends here
