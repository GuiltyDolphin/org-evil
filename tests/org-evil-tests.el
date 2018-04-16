;;; org-evil-tests.el --- unit tests for org-evil
;;;
;;; Commentary:
;;;
;;; Tests for org-evil.
;;;
;;; Code:

(require 'ert)

(defmacro org-evil--test-def-init-test (name descr init-text mode)
  "Test activation of an `org-evil' mode when enabling `org-mode'.

NAME is used to provide a meaningful name for the test.
DESCR is a short description of the starting condition of the test.
INIT-TEXT is the initial text of the buffer.
MODE is the org-evil mode that needs to be active at the end of the
test for the test to pass."
  (declare (indent 1)
           (debug (&define name string-p string-p name)))
  `(ert-deftest ,(intern (concat "org-evil-test-switch-to-org-"
                                 (symbol-name name)))
       ()
     ,(format "Test for switching to org-mode when %s.

Switching to org-mode when %s should cause
`%s' to become active." descr descr mode)
     (with-temp-buffer
       (insert ,init-text)
       (org-mode)
       (should (eq t ,mode)))))

(mapc
 (lambda (tc) (eval `(org-evil--test-def-init-test ,@tc)))
 '((on-heading
    "on a heading" "* Foo" org-evil-heading-mode)
   (at-list
    "on a list item" "+ Foo" org-evil-list-mode)
   (at-table
    "at the beginning of a table" "| table |"  org-evil-table-mode)))

(provide 'org-evil-tests)
;;; org-evil-tests.el ends here
