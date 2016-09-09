;;; evil-org-commands.el --- evil-org general commands.

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
;;; Defines evil-org general commands.
;;;
;;; Code:

(require 'dash)
(require 'evil)

(define-minor-mode evil-org-command-mode
  "Minor-mode for general evil-org commands."
  :keymap (make-sparse-keymap))

(evil-define-operator evil-org-promote
  (beg end &optional count)
  "Promote the current subtree.

With a visual selection, promote the selected headings.
Otherwise, act upon the current subtree."
  :type line
  :motion nil
  (interactive "<r><c>")
  (let* ((beg (set-marker (make-marker) beg))
         (end (set-marker (make-marker) end))
         (count (or count 1))
         (subtree-promoter (if (>= count 0) 'org-promote-subtree 'org-demote-subtree))
         (do-promoter (if (>= count 0) 'org-do-promote 'org-do-demote))
         (count (abs count)))
    (if (not (evil-visual-state-p))
        (--dotimes count (funcall subtree-promoter))
      (--dotimes count (funcall do-promoter)))))

(evil-define-operator evil-org-demote
  (beg end &optional count)
  "Demote the current subtree.

See also `evil-org-promote'."
  :type line
  :motion nil
  (interactive "<r><c>")
  (funcall 'evil-org-promote beg end (- (or count 1))))

(add-hook 'org-mode-hook 'evil-org-command-mode)

(evil-define-minor-mode-key 'normal 'evil-org-command-mode
  "<" 'evil-org-promote
  ">" 'evil-org-demote)

(provide 'evil-org-commands)
;;; evil-org-commands.el ends here
