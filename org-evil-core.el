;;; org-evil-core.el --- org-evil core variables and functions.

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
;;; Defines core org-evil variables and functions.
;;;
;;; Code:

(require 'org)

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
  (org-evil--init-hooks)
  ;; need to perform an initial check to make sure the correct
  ;; regional modes are active when the mode loads
  (org-evil--check-region))

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
  (org-evil--disable-all-org-evil-minor-modes))

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
     (add-to-list 'org-evil--minor-modes ',mode)
     (unless ,(not enabled-by-default)
       (add-to-list 'org-evil--default-minor-modes ',mode))))
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
       (add-to-list 'org-evil--regional-checkers ',check-fn))))
(put 'org-evil--define-regional-minor-mode 'lisp-indent-function 'defun)

(defun org-evil--check-region ()
  "Check the current region with `org-evil--regional-checkers'."
  (-each org-evil--regional-checkers 'funcall))

(defmacro org-evil--save-point-on-error (&rest body)
  "Execute BODY, but reset the position of point if an error is raised."
  `(goto-char (save-excursion ,@body (point))))

(defun org-evil--define-key (state mode key def &rest bindings)
  "Create a STATE binding in MODE from KEY to DEF.
BINDINGS should be a list of additional bindings.

This wrapper ensures that keybindings are inserted
into (and can thus be viewed from) their respective keymaps.

See also `evil-define-key' and `evil-define-minor-mode-key'."
  (apply 'evil-define-minor-mode-key state mode key def bindings)
  (apply 'evil-define-key* state (symbol-value (intern (format "%s-map" mode))) key def bindings))

(defun org-evil--post-command ()
  "Perform regional checking after running commands with `org-evil-mode' enabled."
  (if (not (derived-mode-p 'org-mode))
      (org-evil-mode -1)
    (org-evil--check-region)))

(defun org-evil--init-hooks ()
  "Initialize hooks for `org-evil-mode'."
  (add-hook 'post-command-hook #'org-evil--post-command nil t))

(provide 'org-evil-core)
;;; org-evil-core.el ends here
