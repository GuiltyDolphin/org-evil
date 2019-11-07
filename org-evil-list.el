;;; org-evil-list.el --- org-evil list manipulation.

;; Copyright (C) 2017-2019 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>

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
;;; Defines Evil functionality for working with lists in Org-mode.
;;;
;;; Code:

(require 'evil)
(require 'org-evil-core)
(require 'org-list)

(org-evil--define-regional-minor-mode org-evil-list-mode
  "Minor mode active when in an Org list."
  (org-in-item-p)
  :keymap (make-sparse-keymap))

(defun org-evil-list-open-item-above ()
  "Insert a new item above the current item and switch to Insert state."
  (interactive)
  (org-beginning-of-item)
  (org-insert-item)
  (evil-insert-state 1))

(defun org-evil-list-open-item-or-insert-above (insert)
  "With prefix argument INSERT, perform `org-evil-list-open-item-above'.

Otherwise, perform `evil-open-above'."
  (interactive "P")
  (if insert
      (org-evil-list-open-item-above)
    (evil-open-above 1)))

(defun org-evil-list-open-item-below ()
  "Insert a new item below the current item and switch to Insert state."
  (interactive)
  (org-end-of-item)
  (org-insert-item)
  (evil-insert-state 1))

(defun org-evil-list-open-item-or-insert-below (insert)
  "With prefix argument INSERT, perform `org-evil-list-open-item-below'.

Otherwise, perform `evil-open-below'."
  (interactive "P")
  (if insert
      (org-evil-list-open-item-below)
    (evil-open-below 1)))

(evil-define-motion org-evil-list-beginning-of-item ()
  "Move to the beginning of the current item."
  :type exclusive
  (org-beginning-of-item)
  (re-search-forward
   (regexp-quote
    (org-list-get-bullet (point) (org-list-struct)))))

(evil-define-motion org-evil-list-beginning-of-next-item (count)
  "Move to the beginning of the next item.

If optional COUNT is specified then move that many items down."
  :type line
  (--dotimes (or count 1)
    (org-evil--save-point-on-error (org-next-item))
    (org-evil-list-beginning-of-item)))

(evil-define-motion org-evil-list-beginning-of-previous-item (count)
  "Move to the beginning of the previous item.

If optional COUNT is specified then move that many items up."
  :type line
  (--dotimes (or count 1)
    (org-evil--save-point-on-error (org-previous-item))
    (org-evil-list-beginning-of-item)))

(defun org-evil-list--full-item-region (beg end)
  "Return the start of the first item touched by BEG and the end of the last item touched by END."
  (list (save-excursion (goto-char beg) (org-list-get-item-begin))
        (save-excursion (goto-char end) (org-end-of-item))))

(defmacro org-evil-list--with-items-region (beg end &rest body)
  "With all items between BEG and END, execute BODY.

The current region is expanded to cover all items between BEG and END.

If BEG or END are NIL, no region is assumed and nothing happens."
  (declare (indent 2) (debug t))
  `(let ((beg ,beg) (end ,end))
     (if (and beg end)
         (-let* (((beg end) (org-evil-list--full-item-region beg end)))
           (evil-with-active-region beg end ,@body))
       ,@body)))

(evil-define-operator org-evil-list-outdent-item-tree
  (beg end &optional count)
  "Outdent the current list item and its children."
  :type block
  :motion nil
  (interactive "<r><c>")
  (org-evil-list--with-items-region beg end
  (let* ((count (or count 1))
         (indenter (if (>= count 0) 'org-outdent-item-tree 'org-indent-item-tree))
         (count (abs count)))
    (--dotimes count (funcall indenter)))))

(evil-define-operator org-evil-list-indent-item-tree
  (beg end &optional count)
  "Indent the current list item and its children."
  :type block
  :motion nil
  (interactive "<r><c>")
  (org-evil-list--with-items-region beg end
    (let* ((count (or count 1))
           (indenter (if (>= count 0) 'org-indent-item-tree 'org-outdent-item-tree))
           (count (abs count)))
      (--dotimes count (funcall indenter)))))

(org-evil--define-key 'motion 'org-evil-list-mode
  "(" 'org-evil-list-beginning-of-previous-item
  ")" 'org-evil-list-beginning-of-next-item
  "^" 'org-evil-list-beginning-of-item)

(org-evil--define-key 'normal 'org-evil-list-mode
  "<" 'org-evil-list-outdent-item-tree
  ">" 'org-evil-list-indent-item-tree
  "O" 'org-evil-list-open-item-or-insert-above
  "o" 'org-evil-list-open-item-or-insert-below)

(provide 'org-evil-list)
;;; org-evil-list.el ends here
