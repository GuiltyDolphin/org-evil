;;; org-evil-table.el --- org-evil table table manipulation.

;; Copyright (C) 2016 Ben Moon
;; Author: Ben Moon <guiltydolphin@gmail.com>

;; This program is free software: you can redistribute it and/or modify
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
;;;
;;; Defines Evil functionality for working with tables in Org-mode.
;;;
;;; Code:

(require 'dash)
(require 'evil)
(require 'org-evil-core)
(require 'org-table)

(org-evil--define-regional-minor-mode org-evil-table-mode
  "Minor mode active when in an Org table."
  (org-at-table-p)
  :keymap (make-sparse-keymap)
  :lighter "<table>")

(defmacro org-evil-table--with-current-column (&rest body)
  "Execute BODY, but ensure the current table column is maintained."
  (let ((current-column (make-symbol "current-column")))
    `(let ((,current-column (org-table-current-column)))
       ,@body
       (when (not (= ,current-column (org-table-current-column)))
         (org-table-goto-column ,current-column)))))

(defun org-evil-table-insert-row-above ()
  "Insert a new row above the current row."
  (interactive)
  (org-evil-table--with-current-column
   (org-table-insert-row)))

(defun org-evil-table-insert-row-below ()
  "Insert a new row below the current row."
  (interactive)
  (org-evil-table--with-current-column
   (org-table-insert-row t)))

(evil-define-motion org-evil-table-goto-column (n)
  "Go to the Nth field in the current row.
By default the next field."
  :type exclusive
  (if n (org-table-goto-column n) (org-table-next-field))
  (point))

(evil-define-motion org-evil-table-forward-field (count)
  "Move COUNT fields forwards.
Default COUNT is 1."
  (let ((count (or count 1)))
    (if (< count 0) (org-evil-table-backward-field (abs count))
      (--dotimes count (org-table-next-field)))))

(evil-define-motion org-evil-table-backward-field (count)
  "Move COUNT fields backwards.
Default COUNT is 1."
  (let ((count (or count 1)))
    (if (< count 0) (org-evil-table-forward-field (abs count))
      (--dotimes count (org-table-previous-field)))))

(evil-define-motion org-evil-table-end-of-field (count)
  "Go to the end of the current field, move forward COUNT fields if specified."
  :type exclusive
  (org-evil-table-forward-field count)
  (let ((current-field (org-table-current-column)))
    (org-table-goto-column current-field)
    (let ((beg-point (point)))
      (org-table-end-of-field 0)
      (if (= (org-table-current-column) current-field)
          (point)
        (goto-char beg-point)))))

(evil-define-motion org-evil-table-beginning-of-field (count)
  "Go to the end of the current field, move backwards COUNT fields if specified."
  :type exclusive
  (org-evil-table-backward-field count)
  (let ((current-field (org-table-current-column)))
    (org-table-goto-column current-field)
    (let ((beg-point (point)))
      (org-table-beginning-of-field 0)
      (if (= (org-table-current-column) current-field)
          (point)
        (goto-char beg-point)))))

(evil-define-text-object org-evil-table-field (count &optional beg end type)
  "Select a field."
  (list (save-excursion (org-evil-table-beginning-of-field (1- count)))
        (save-excursion (org-evil-table-end-of-field (1- count)))))

(defun org-evil-table--last-line ()
  "Line number of final row in current table."
  (let* ((eot (org-table-end)))
    (save-excursion (goto-char eot)
      (if (org-at-table-p) (line-number-at-pos) (1- (line-number-at-pos))))))

(evil-define-operator org-evil-table-kill-row
  (beg end &optional count)
  "Delete the current row or horizonal line from the table.

When COUNT is specified delete COUNT rows (including the current).

Only delete up to the end of the table."
  :motion nil
  (interactive "<r><c>")
  (let* ((available-rows (1+ (- (org-evil-table--last-line) (line-number-at-pos))))
         (count (min (or count 1) available-rows))
         (col (org-table-current-column)))
    (--dotimes count (org-table-kill-row))
    (org-table-goto-column col)))

(evil-define-motion org-evil-table-next-row (count)
  "Move the cursor COUNT rows down."
  :type line
  (let (line-move-visual)
    (dotimes (n (or count 1)) (org-table-next-row))))

(defun org-evil-table--num-lines ()
  "Return the number of data lines in the current table."
  (save-excursion
    (goto-char (org-table-end))
    (org-table-current-line)))

(evil-define-motion org-evil-table-goto-line (count)
  "Go to the COUNTth data line in the current table.
By default the first line."
  :jump t
  :type line
  (org-evil-table--with-current-column
   (org-table-goto-line (or count 1))))

(evil-define-motion org-evil-table-goto-line-from-bottom (count)
  "Go to the COUNTth data line (counting from the last) in the current table.
By default the last line."
  :jump t
  :type line
  (org-evil-table--with-current-column
   (let ((num-lines (org-evil-table--num-lines)))
     (org-table-goto-line (- num-lines (1- (or count 1)))))))

(evil-define-operator org-evil-table-move-column-right
  (beg end &optional count)
  "Move the current column COUNT places to the right."
  :motion nil
  (interactive "<r><c>")
  (let ((count (or count 1)))
    (--dotimes count (org-table-move-column-right))))

(evil-define-operator org-evil-table-move-column-left
  (beg end &optional count)
  "Move the current column COUNT places to the left."
  :motion nil
  (interactive "<r><c>")
  (let ((count (or count 1)))
    (--dotimes count (org-table-move-column-left))))

(evil-define-minor-mode-key 'motion 'org-evil-table-mode
  "|" 'org-evil-table-goto-column)

(evil-define-minor-mode-key '(motion operator visual) 'org-evil-table-mode
  "gc" 'org-evil-table-goto-column
  "gr" 'org-evil-table-goto-line
  "gR" 'org-evil-table-goto-line-from-bottom)

(evil-define-minor-mode-key 'normal 'org-evil-table-mode
  "<" 'org-evil-table-move-column-left
  ">" 'org-evil-table-move-column-right
  "D" 'org-evil-table-kill-row
  "O" 'org-evil-table-insert-row-above
  "o" 'org-evil-table-insert-row-below)

(evil-define-minor-mode-key 'visual 'org-evil-table-mode
  "i|" 'org-evil-table-field)

(provide 'org-evil-table)
;;; org-evil-table.el ends here
