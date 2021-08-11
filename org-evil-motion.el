;;; org-evil-motion.el --- org-evil general motion.

;; Copyright (C) 2016-2019, 2021 Ben Moon
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
(require 'org-element)

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

(defun org-evil--element-is-block-p (element)
  "Non-NIL if ELEMENT is a block element."
  (memq (org-element-type element)
        '(center-block comment-block dynamic-block example-block export-block
			           quote-block special-block src-block verse-block)))

(defun org-evil-in-block-p ()
  "Non-nil when point belongs to a block."
  (org-evil--element-is-block-p (org-element-at-point)))

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

(defvar org-evil-text-markup-types
  '(bold code italic strike-through underline verbatim)
  "Available markup types for Org.")

(defun org-evil--markup-to-markup-char (type)
  "Return the markup character associated with markup TYPE."
  (cond
   ((eq type 'bold) "*")
   ((eq type 'code) "~")
   ((eq type 'italic) "/")
   ((eq type 'strike-through) "+")
   ((eq type 'underline) "_")
   ((eq type 'verbatim) "=")
   (t (error "Invalid markup type: %s" type))))

(defun org-evil-text-markup--get-markup-object (type)
  "Get the Org object at point representing inline markup for TYPE."
  (let ((markup-char-re (regexp-quote (org-evil--markup-to-markup-char type))))
    (save-match-data
      (save-excursion
        (when (or (re-search-backward markup-char-re (line-beginning-position -2) t) (looking-at markup-char-re))
          (org-element--object-lex (list type)))))))

(defun org-evil-in-text-markup-p (type)
  "Non-NIL when point belongs to text markup of the given TYPE."
  (org-evil-text-markup--get-markup-object type))

(defun org-evil-text-markup-boundaries (type)
  "Return the '(START . END) position of the current text markup of TYPE.

TYPE is a markup symbol present in `org-evil-text-markup-types'."
  (-when-let ((_ (&plist :begin beg :end end :post-blank post-blank)) (org-evil-text-markup--get-markup-object type)) (cons beg (- end post-blank))))

(defun org-evil-text-markup--is-simple-text-markup (type)
  "Non-NIL if TYPE is simple text markup.

By default, Org treats code and verbatim as simple markup.

Simple text markup cannot contain other text markup."
  (memq type '(code verbatim)))

(defun org-evil-text-markup-content-boundaries (type)
  "Return the '(START . END) position of the contents of TYPE text markup."
  ;; simple text markup has no contents begin/end, so we need to treat it specially
  (if (org-evil-text-markup--is-simple-text-markup type)
      (-when-let ((beg . end) (org-evil-text-markup-boundaries type))
        (cons (1+ beg) (1- end)))
    (-when-let ((_ (&plist :contents-begin beg :contents-end end)) (org-evil-text-markup--get-markup-object type)) (cons beg end))))

(defun org-evil-text-markup--define-markup-text-object (type)
  "Define appropriate text objects for representing inline markup of TYPE."
  (let ((inner-name (intern (format "org-evil-text-markup-inner-%s" type)))
        (outer-name (intern (format "org-evil-text-markup-outer-%s" type)))
        (inner-desc (format "Select inside %s text markup." type))
        (outer-desc (format "Select around %s text markup." type))
        (char (make-symbol "char")))
    (eval `(progn
       (evil-define-text-object ,outer-name (count)
         ,outer-desc
         (interactive "<c>")
         (-cons-to-list (org-evil-text-markup-boundaries (quote ,type))))

       (evil-define-text-object ,inner-name (count)
         ,inner-desc
         :type exclusive
         (interactive "<c>")
         (-cons-to-list (org-evil-text-markup-content-boundaries (quote ,type))))

       (let ((,char (org-evil--markup-to-markup-char (quote ,type))))
         (dolist (mode '(operator visual))
           (org-evil--define-key mode 'org-evil-motion-mode
                                 (format "i%s" ,char) #',inner-name
                                 (format "a%s" ,char) #',outer-name)))))))

(-each org-evil-text-markup-types (lambda (type) (org-evil-text-markup--define-markup-text-object type)))

(org-evil--define-key 'motion 'org-evil-motion-mode
  "[[" 'org-evil-motion-backward-block-begin
  "]]" 'org-evil-motion-forward-block-begin
  "gH" 'org-evil-motion-up-heading-top
  "gh" 'org-evil-motion-up-heading
  "{" 'org-evil-motion-backward-heading
  "}" 'org-evil-motion-forward-heading)

(provide 'org-evil-motion)
;;; org-evil-motion.el ends here
