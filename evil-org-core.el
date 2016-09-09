;;; evil-org-core.el --- evil-org core variables and functions.

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
;;; Defines core evil-org variables and functions.
;;;
;;; Code:

(require 'hook)

(defgroup evil-org nil
  "Evil extensions for Org."
  :group 'evil
  :prefix 'evil-org-)

(defmacro evil-org--define-regional-minor-mode (mode doc pred &rest args)
  "Define an evil-org minor mode MODE that is active when PRED is non-NIL.
DOC is the documentation as in `define-minor-mode'.

PRED is checked after moving `point', and should be an un-quoted expression.
ARGS should be the same as in `define-minor-mode' (bar MODE and DOC)."
  (declare (doc-string 2)
           (debug (&define name string-or-null-p sexp
			   [&rest [keywordp sexp]]
			   def-body)))
  (let ((check-fn (intern (format "evil-org--check-%s" mode))))
    `(progn
       (define-minor-mode ,mode ,doc ,@args)
       (defun ,check-fn ()
         ,(format "Check whether %s should be activated in the current location." mode)
         (if ,pred (,mode) (when ,mode (,mode -1))))
       (hook--monitor-expression-value '(point) ',check-fn 'org-mode t))))
(put 'evil-org--define-regional-minor-mode 'lisp-indent-function 'defun)

(provide 'evil-org-core)
;;; evil-org-core.el ends here
