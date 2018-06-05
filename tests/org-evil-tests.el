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

(defmacro org-evil--test-with-buffer-text (text &rest body)
  "In a buffer containing TEXT, run BODY, using the result of BODY as the result."
  (declare (indent 1)
           (debug (&define string-p def-body)))
  `(with-temp-buffer
     ;; make sure we have a consistent environment for the test
     (let ((evil-auto-indent nil))
       (insert ,text)
       ;; Need to enable org-mode so some (org-mode) local variables get set
       (org-mode)
       ,@body)))

(defmacro org-evil--test-with-expected-buffer-text
    (initial expected &rest body)
  "`buffer-string' of buffer with INITIAL text is EXPECTED after running BODY.

INITIAL is the text that will initially be inserted into the buffer.

EXPECTED is the text that should be in the buffer after running BODY with the buffer current."
  `(org-evil--test-with-buffer-text ,initial
     ,@body (should (equal ,expected (buffer-string)))))

(ert-deftest org-evil-list-test-open-item-or-insert-below ()
  "Tests for `org-evil-list-open-item-or-insert-below'."
  :tags '(org-evil org-evil-list)
  ;; without prefix
  (org-evil--test-with-expected-buffer-text "+ X" "+ X\n"
    (org-evil-list-open-item-or-insert-below nil))
  ;; with prefix
  (org-evil--test-with-expected-buffer-text "+ X" "+ X\n+ "
    (org-evil-list-open-item-or-insert-below t)))

(ert-deftest org-evil-heading-test-open-sibling-or-insert-below ()
  "Tests for `org-evil-heading-open-sibling-or-insert-below'."
  :tags '(org-evil org-evil-heading)
  ;; without prefix
  (org-evil--test-with-expected-buffer-text "* X" "* X\n"
    (org-evil-heading-open-sibling-or-insert-below nil))
  ;; with prefix
  (org-evil--test-with-expected-buffer-text "* X" "* X\n* \n"
    (org-evil-heading-open-sibling-or-insert-below t))
  ;; in subheading
  (org-evil--test-with-expected-buffer-text "* X\n\n** Y" "* X\n\n** Y\n\n** \n"
    (org-evil-heading-open-sibling-or-insert-below t))
  ;; higher-level heading after subheading
  (org-evil--test-with-expected-buffer-text "* X\n\n** Y\n\n* Z" "* X\n\n** Y\n\n* Z\n\n* \n"
    (org-evil-heading-open-sibling-or-insert-below t)))

(provide 'org-evil-tests)
;;; org-evil-tests.el ends here
