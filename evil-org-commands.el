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

(define-minor-mode evil-org-command-mode
  "Minor-mode for general evil-org commands."
  :keymap (make-sparse-keymap))

(add-hook 'org-mode-hook 'evil-org-command-mode)
(provide 'evil-org-commands)
;;; evil-org-commands.el ends here
