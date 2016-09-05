;;; evil-org-motion.el --- evil-org general motion.

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
;;; Defines Evil functionality for moving around in general Org mode.
;;;
;;; Code:

(require 'dash)
(require 'evil)

(define-minor-mode evil-org-motion-mode
  "Minor-mode for moving around in Org files."
  :keymap (make-sparse-keymap))

(evil-define-motion evil-org-motion-forward-heading
  (count)
  "Move forward by COUNT headings at the same level (default 1).

If there are no more headings at the same level, attempt to move to
the next higher heading."
  (let ((count (or count 1)))
    (--dotimes count
      (if (and (evil-org-motion--last-heading-same-level-p) (evil-org-motion--heading-has-parent-p))
          (if (save-excursion (evil-org-motion-up-heading) (not (evil-org-motion--last-heading-same-level-p)))
              (progn (evil-org-motion-up-heading) (evil-org-motion-forward-heading))
            (error "No more forward headings"))
        (if (not (evil-org-motion--last-heading-same-level-p))
            (org-forward-heading-same-level 1)
          (error "No more forward headings"))))))

(defun evil-org-motion--last-heading-same-level-p ()
  "Return T if the current heading is the last child of its parents."
  (save-excursion
    (when (ignore-errors (org-back-to-heading))
      (let ((header-point (point)))
        (org-forward-heading-same-level 1 t)
        (= (point) header-point)))))

(defun evil-org-motion--heading-has-parent-p ()
  "Return non-NIL if the current heading has a parent."
  (save-excursion (ignore-errors (org-up-heading-safe))))

(evil-define-motion evil-org-motion-up-heading
  (count)
  "Move up COUNT parent headings.
Jump to the current heading if not already upon it."
  :type line
  (let ((count (or count 1)))
    (unless (org-at-heading-p) (progn (org-back-to-heading) (setq count (1- count))))
    (--dotimes count (org-up-heading-all 1))))

(evil-define-motion evil-org-motion-up-heading-top
  (count)
  "Move up to the COUNTth level parent heading.
Move to the parent-most heading by default.
Move to the current heading if COUNT is greater than the parent level."
  :type line
  (let ((count (or count 1))
        (level (org-current-level)))
    (when level
      (if (<= level count) (org-back-to-heading)
        (org-up-heading-all (- level count))))))

(evil-define-key 'motion evil-org-motion-mode-map
  "gh" 'evil-org-motion-up-heading
  "gH" 'evil-org-motion-up-heading-top)
(add-hook 'org-mode-hook 'evil-org-motion-mode)

(provide 'evil-org-motion)
;;; evil-org-motion.el ends here
