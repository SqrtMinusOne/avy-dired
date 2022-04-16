;;; avy-dired.el --- The fastest file navigation in the wild west -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (avy "0.5.0"))
;; Homepage: https://github.com/SqrtMinusOne/avy-dired.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO

;;; Code:
(require 'avy)
(require 'dired)

(defgroup avy-dired nil
  "The fastest file navigation in the wild west"
  :group 'dired)

(defcustom avy-dired-delta -2
  "Delta for the candidate item. 0 is the start of the filename.

Relevant only if `dired-hide-details-mode' is inactive."
  :group 'avy-dired
  :type 'integer)

(defun avy-dired-cands ()
  (let (candidates
        eol
        (ws (window-start))
        (we (window-end (selected-window) t)))
    (save-excursion
      (save-restriction
        (narrow-to-region ws we)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (setq eol (line-end-position))
          (let ((change (next-single-property-change (point) 'dired-filename nil eol))
                (delta (if dired-hide-details-mode 0 avy-dired-delta)))
            (cond
             ((and change (< change eol))
              (goto-char change)
              (push (cons (+ delta (point)) (selected-window)) candidates))
             ((re-search-forward directory-listing-before-filename-regexp eol t)
              (goto-char (match-end 0))
              (push (cons (point) (selected-window)) candidates))))
          (forward-line 1))))
    (nreverse candidates)))

;;;###autoload
(defun avy-dired-goto-line ()
  "Jump to a line in dired buffer"
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (dired default-directory))
  (avy-with avy-dired-goto-line
    (let* ((avy-handler-old avy-handler-function)
           (avy-handler-function
            (lambda (char)
              (pcase char
                (?K (progn
                      (scroll-up-command)
                      (avy-dired-goto-line)))
                (?J (progn
                      (scroll-down-command)
                      (avy-dired-goto-line)))
                (?q (progn
                      (throw 'done 'abort)))
                (_ (funcall avy-handler-old char)))))
           (r (avy-process (avy-dired-cands))))
      (when (not (memq r '(t nil)))
        (avy-action-goto r)
        (let ((file (dired-get-file-for-visit)))
          (dired-open-file)
          (when (file-directory-p file)
            (avy-dired-goto-line)))))))

(provide 'avy-dired)
;;; avy-dired.el ends here
