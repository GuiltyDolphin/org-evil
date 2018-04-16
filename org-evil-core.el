;;; org-evil-core.el --- org-evil core variables and functions.

;; Copyright (C) 2016-2018 Ben Moon
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
;;; Defines core org-evil variables and functions.
;;;
;;; Code:

(require 'monitor)

(defgroup org-evil nil
  "Evil extensions for Org."
  :group 'evil
  :group 'org
  :group 'convenience
  :prefix 'org-evil-)

(define-minor-mode org-evil-mode
  "Minor-mode for org-evil."
  :group 'org-evil
  (if org-evil-mode (org-evil--mode-initialise)
    (org-evil--mode-disable-internal)))

(add-hook 'org-mode-hook 'org-evil--org-mode-hook-fn)

(defun org-evil--mode-initialise ()
  "Perform additional initialisation for `org-evil-mode'."
  ;; enable default minor modes
  (mapc 'funcall org-evil--default-minor-modes)
  (monitor-enable 'org-evil-hook-monitor)
  (add-hook 'buffer-list-update-hook
            'org-evil--buffer-list-update-hook-fn)
  ;; need to perform an initial check to make sure the
  ;; correct regional modes activate, as point won't
  ;; have changed yet
  (org-evil--check-region))

;; NOTE: Until monitor supports buffer-local monitors, we need
;;       to handle ensuring the monitor doesn't become out-of-date.
(defun org-evil--buffer-list-update-hook-fn ()
  "Ensure state of org-evil is correct after a change of buffer."
  (if org-evil-mode
      (and (monitor--disabled-p 'org-evil-hook-monitor)
           (monitor-enable 'org-evil-hook-monitor))
    (and (monitor--enabled-p 'org-evil-hook-monitor)
         (monitor-disable 'org-evil-hook-monitor))))

(defun org-evil--org-mode-hook-fn ()
  "Ensure `org-evil-mode' is kept up-to-date with `org-mode'."
  (if (eq major-mode 'org-mode)
      (org-evil-mode t)
    (org-evil-mode -1)))

(defun org-evil--disable-all-org-evil-minor-modes ()
  "Disable all org-evil minor modes for the current buffer."
  (mapc (lambda (mode) (funcall mode -1)) org-evil--minor-modes))

(defun org-evil--mode-disable-internal ()
  "Clean up after org-evil."
  (org-evil--disable-all-org-evil-minor-modes)
  (monitor-disable 'org-evil-hook-monitor))

(defvar org-evil--minor-modes nil
  "Minor modes for org-evil.")

(defvar org-evil--default-minor-modes nil
  "Org-evil minor modes that should be enabled with `org-evil-mode'.")

(defmacro org-evil--define-minor-mode
    (mode doc &optional enabled-by-default &rest args)
  "Define an org-evil minor mode MODE.
DOC is the documentation as in `define-minor-mode'.

ENABLED-BY-DEFAULT (if non-NIL) specifies that MODE should
be enabled whenever `org-evil-mode' is enabled.

ARGS should be the same as in `define-minor-mode' (bar MODE and DOC)."
  (declare (doc-string 2)
           (debug (&define name string-or-null-p
                           [&rest [keywordp sexp]]
                           def-body)))
  `(progn
     (define-minor-mode ,mode ,doc ,@args)
     (unless (member ',mode org-evil--minor-modes)
       (push ',mode org-evil--minor-modes))
     (unless (and ,(not enabled-by-default)
                  (member ',mode org-evil--minor-modes))
       (push ',mode org-evil--default-minor-modes))))
(put 'org-evil--define-minor-mode 'lisp-indent-function 'defun)

(defvar org-evil--regional-checkers nil
  "Functions to be run when checking the current region.")

(defmacro org-evil--define-regional-minor-mode (mode doc pred &rest args)
  "Define an org-evil minor mode MODE that is active when PRED is non-NIL.
DOC is the documentation as in `define-minor-mode'.

PRED is checked after moving `point', and should be an un-quoted expression.
ARGS should be the same as in `define-minor-mode' (bar MODE and DOC)."
  (declare (doc-string 2)
           (debug (&define name string-or-null-p sexp
                           [&rest [keywordp sexp]]
                           def-body)))
  (let ((check-fn (intern (format "org-evil--check-%s" mode))))
    `(progn
       (org-evil--define-minor-mode ,mode ,doc nil ,@args)
       (defun ,check-fn ()
         ,(format "Check whether %s should be activated in the current location." mode)
         (if ,pred (,mode) (when ,mode (,mode -1))))
       (unless (member ',check-fn org-evil--regional-checkers)
         (push ',check-fn org-evil--regional-checkers)))))
(put 'org-evil--define-regional-minor-mode 'lisp-indent-function 'defun)

(defvar org-evil--hook-ivar nil)

(define-monitor 'org-evil-hook-monitor 'hook
  "Org-evil monitor for hooks."
  :hook-ivar 'org-evil--hook-ivar)

(defvar org-evil--post-command-instance
  (monitor 'org-evil-hook-monitor
    :hook 'post-command-hook
    :trigger 'org-evil--check-point))

(defvar org-evil--point-check-instance
  (monitor 'expression-value
    :expr '(point)
    :pred '/=
    :trigger 'org-evil--check-region))

(defun org-evil--check-point ()
  "Check the current point for region change."
  (monitor-run-monitor-option 'expression-value :check org-evil--point-check-instance))

(defun org-evil--check-region ()
  "Check the current region with `org-evil--regional-checkers'."
  (-each org-evil--regional-checkers 'funcall))

(defmacro org-evil--save-point-on-error (&rest body)
  "Execute BODY, but reset the position of point if an error is raised."
  `(goto-char (save-excursion ,@body (point))))

(provide 'org-evil-core)
;;; org-evil-core.el ends here
