;;; org-evil.el --- Evil extensions for Org.

;; Copyright (C) 2016-2019, 2021 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>
;; URL: https://github.com/guiltydolphin/org-evil
;; Git-Repository: git://github.com/guiltydolphin/org-evil.git
;; Created: 2016-08-21
;; Version: 0.6.0
;; Keywords: convenience, evil, org
;; Package-Requires: ((dash "2.19.0") (evil "0") (org "9.4.4"))

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

;; Org-evil provides integration between Evil and Org through
;; various means such as contextual keybindings.
;;
;; Org-evil has a standard set of keybindings, with additional
;; bindings that vary depending on the location and type of
;; Org file being edited, for example some bindings are only
;; active within a table.
;;
;; All the bindings aim to be fairly intuitive for Evil users,
;; so there shouldn't be any huge surprises.
;;
;; For more information see the README.

;;; Code:

(require 'org-evil-commands)
(require 'org-evil-list)
(require 'org-evil-motion)
(require 'org-evil-table)

(provide 'org-evil)
;;; org-evil.el ends here
