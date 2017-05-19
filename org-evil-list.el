;;; org-evil-list.el --- org-evil list manipulation.

;; Copyright (C) 2017 Ben Moon
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

(provide 'org-evil-list)
;;; org-evil-list.el ends here
