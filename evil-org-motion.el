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

(evil-define-motion evil-org-motion-up-heading
  (count)
  "Move up COUNT parent headings.
Jump to the current heading if not already upon it."
  (let ((count (or count 1)))
    (unless (org-at-heading-p) (progn (org-back-to-heading) (setq count (1- count))))
    (--dotimes count (org-up-heading-all 1))))

(provide 'evil-org-motion)
;;; evil-org-motion.el ends here
