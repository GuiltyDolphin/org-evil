;;; org-evil-motion.el --- org-evil general motion.

;; Copyright (C) 2016-2019 Ben Moon
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
;;; Defines Evil functionality for moving around in general Org mode.
;;;
;;; Code:

(require 'dash)
(require 'evil)

(org-evil--define-minor-mode org-evil-motion-mode
  "Minor-mode for moving around in Org files."
  t
  :keymap (make-sparse-keymap))

(defun org-evil-motion--check-in-headings ()
  "Signal a user error if not within a heading hierarchy."
  (when (org-before-first-heading-p) (user-error "Before first heading")))

(evil-define-motion org-evil-motion-forward-heading
  (count)
  "Move forward by COUNT headings at the same level (default 1).

If there are no more headings at the same level, attempt to move to
the next higher heading."
  (let ((count (or count 1)))
    (--dotimes count
      (if (and (org-evil-motion--last-heading-same-level-p) (org-evil-motion--heading-has-parent-p))
          (if (save-excursion (org-evil-motion-up-heading) (not (org-evil-motion--last-heading-same-level-p)))
              (progn (org-evil-motion-up-heading) (org-evil-motion-forward-heading))
            (error "No more forward headings"))
        (if (not (org-evil-motion--last-heading-same-level-p))
            (org-forward-heading-same-level 1)
          (error "No more forward headings"))))))

(evil-define-motion org-evil-motion-backward-heading
  (count)
  "Move backward by COUNT headings at the same level (default 1).

If there are no previous headings at the same level, attempt to move to
the previous higher heading.

Move to the current heading if not on a heading."
  (org-evil-motion--check-in-headings)
  (let ((count (or count 1)))
    (--dotimes count
      (if (org-evil-motion--first-heading-same-level-p)
          (if (org-evil-motion--heading-has-parent-p)
              (org-evil-motion-up-heading)
            (if (org-at-heading-p)
                (error "Already at first heading")
              (org-evil-motion-up-heading)))
        (if (not (org-evil-motion--first-heading-same-level-p))
            (if (org-at-heading-p)
                (org-backward-heading-same-level 1)
              (org-evil-motion-up-heading))
          (error "No more previous headings"))))))

(defun org-evil-motion--last-heading-same-level-p ()
  "Return T if the current heading is the last child of its parents."
  (save-excursion
    (when (ignore-errors (org-back-to-heading))
      (let ((header-point (point)))
        (org-forward-heading-same-level 1 t)
        (= (point) header-point)))))

(defun org-evil-motion--first-heading-same-level-p ()
  "Return T if the current heading is the first child of its parents."
  (save-excursion
    (ignore-errors (progn (org-back-to-heading) (org-first-sibling-p)))))

(defun org-evil-motion--heading-has-parent-p ()
  "Return non-NIL if the current heading has a parent."
  (save-excursion (ignore-errors (org-up-heading-safe))))

(evil-define-motion org-evil-motion-up-heading
  (count)
  "Move up COUNT parent headings.
Jump to the current heading if not already upon it."
  :type line
  (let ((count (or count 1)))
    (unless (org-at-heading-p) (progn (org-back-to-heading) (setq count (1- count))))
    (--dotimes count (org-up-heading-all 1))))

(evil-define-motion org-evil-motion-up-heading-top
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

;;; Blocks

(evil-define-motion org-evil-motion-forward-block-begin
  (count)
  "Move forward to the beginning of the COUNTth next block."
  :type line
  (--dotimes (or count 1) (org-evil--save-point-on-error (org-next-block 1))))

(evil-define-motion org-evil-motion-backward-block-begin
  (count)
  "Move backward to the beginning of the COUNTth previous block."
  :type line
  (--dotimes (or count 1) (org-evil--save-point-on-error (org-previous-block 1))))

(defun org-evil-in-block-p ()
  "Non-nil when point belongs to a block."
  (let* ((case-fold-search t)
	 (blockp (org-between-regexps-p "^[ \t]*#\\+begin_.*"
					"^[ \t]*#\\+end_.*")))
    blockp))

(org-evil--define-regional-minor-mode org-evil-block-mode
  "Minor-mode active when in an Org block."
  (org-evil-in-block-p)
  :keymap (make-sparse-keymap))

(defun org-evil-block-boundaries ()
  "Return the '(START . END) position of the current block."
  (or (org-evil-in-block-p) (user-error "Not in a block")))

(defun org-evil-block-beginning-of-block ()
  "Go to the beginning of the current block."
  (interactive)
  (goto-char (car (org-evil-block-boundaries))))

(defun org-evil-block-end-of-block ()
  "Go to the end of the current block."
  (interactive)
  (goto-char (cdr (org-evil-block-boundaries))))

(defun org-evil-block-beginning-of-content ()
  "Go to the start of the current block's content."
  (interactive)
  (org-evil-block-beginning-of-block)
  (while (org-at-block-p)
    (forward-line))
  (point))

(defun org-evil-block-end-of-content ()
  "Go to the end of the current block's content."
  (interactive)
  (org-evil-block-end-of-block)
  (forward-line -1)
  (end-of-line)
  (point))

(defun org-evil-block-content-boundaries ()
  "Return the '(START . END) boundaries of the content for the current block."
  (cons (save-excursion (org-evil-block-beginning-of-content))
        (save-excursion (org-evil-block-end-of-content))))

(evil-define-text-object org-evil-block-inner-block (count)
  "Select inner block (the content)."
  :type 'line
  (interactive "<c>")
  (-cons-to-list (org-evil-block-content-boundaries)))

(evil-define-text-object org-evil-block-a-block (count)
  "Select a block."
  :type 'line
  (interactive "<c>")
  (-cons-to-list (org-evil-block-boundaries)))

(org-evil--define-key 'motion 'org-evil-block-mode
  "(" 'org-evil-block-beginning-of-block
  ")" 'org-evil-block-end-of-block)

;; Have to loop through as it looks like the text objects
;; don't configure correctly when binding multiple states
;; at once.
(dolist (mode '(operator visual))
  (org-evil--define-key mode 'org-evil-block-mode
    "ib" 'org-evil-block-inner-block
    "ab" 'org-evil-block-a-block))

(org-evil--define-key 'motion 'org-evil-motion-mode
  "[[" 'org-evil-motion-backward-block-begin
  "]]" 'org-evil-motion-forward-block-begin
  "gH" 'org-evil-motion-up-heading-top
  "gh" 'org-evil-motion-up-heading
  "{" 'org-evil-motion-backward-heading
  "}" 'org-evil-motion-forward-heading)

(provide 'org-evil-motion)
;;; org-evil-motion.el ends here
