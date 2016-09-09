;;; evil-org.el --- Evil extensions for Org.

;; Copyright (C) 2016 Ben Moon
;; Author: Ben Moon <guiltydolphin@gmail.com>
;; URL: https://github.com/guiltydolphin/evil-org
;; Git-Repository: git://github.com/guiltydolphin/evil-org.git
;; Created: 2016-08-21
;; Version: 0.1.0
;; Keywords: evil org
;; Package-Requires: ((dash "2.13.0") (evil "0") (hook "0") (emaps "0"))

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
;;; Evil extensions for Org.
;;;
;;; Code:

(require 'evil-org-commands)
(require 'evil-org-motion)
(require 'evil-org-table)

(provide 'evil-org)
;;; evil-org.el ends here
