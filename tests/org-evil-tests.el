;;; org-evil-tests.el --- unit tests for org-evil
;;;
;;; Commentary:
;;;
;;; Tests for org-evil.
;;;
;;; Code:

(require 'ert)

(require 'org-evil)

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
     (let ((evil-auto-indent nil)
           (org-startup-folded nil))
       (insert ,text)
       (switch-to-buffer (current-buffer))
       ;; Need to enable org-mode so some (org-mode) local variables get set
       (evil-mode)
       (org-mode)
       ,@body)))

(defmacro org-evil--test-with-expected-buffer-text
    (initial expected &rest body)
  "`buffer-string' of buffer with INITIAL text is EXPECTED after running BODY.

INITIAL is the text that will initially be inserted into the buffer.

EXPECTED is the text that should be in the buffer after running BODY with the buffer current."
  `(org-evil--test-with-buffer-text ,initial
     ,@body (should (equal ,expected (buffer-string)))))
(put 'org-evil--test-with-expected-buffer-text 'lisp-indent-function 2)

(ert-deftest org-evil-test-setup-teardown ()
  "Test that `org-evil-mode' correctly sets up and cleans up when enabled/disabled."
  (with-temp-buffer
    (org-mode)
    (should (memq #'org-evil--post-command post-command-hook))
    (fundamental-mode)
    (should-not (memq #'org-evil--post-command post-command-hook))))

(ert-deftest org-evil-list-test-open-item-or-insert-above ()
  "Tests for `org-evil-list-open-item-or-insert-above'."
  :tags '(org-evil org-evil-list)
  ;; without prefix
  (org-evil--test-with-expected-buffer-text "+ X" "\n+ X"
    (org-evil-list-open-item-or-insert-above nil))
  ;; with prefix
  (org-evil--test-with-expected-buffer-text "+ X" "+ \n+ X"
    (org-evil-list-open-item-or-insert-above t)))

(ert-deftest org-evil-list-test-open-item-or-insert-below ()
  "Tests for `org-evil-list-open-item-or-insert-below'."
  :tags '(org-evil org-evil-list)
  ;; without prefix
  (org-evil--test-with-expected-buffer-text "+ X" "+ X\n"
    (org-evil-list-open-item-or-insert-below nil))
  ;; with prefix
  (org-evil--test-with-expected-buffer-text "+ X" "+ X\n+ "
    (org-evil-list-open-item-or-insert-below t)))

(ert-deftest org-evil-heading-test-open-sibling-or-insert-above ()
  "Tests for `org-evil-heading-open-sibling-or-insert-above'."
  :tags '(org-evil org-evil-heading)
  ;; without prefix
  (org-evil--test-with-expected-buffer-text "* X" "\n* X"
    (org-evil-heading-open-sibling-or-insert-above nil))
  ;; with prefix
  (org-evil--test-with-expected-buffer-text "* X" "* \n* X"
    (org-evil-heading-open-sibling-or-insert-above t))
  ;; in subheading
  (org-evil--test-with-expected-buffer-text "* X\n\n** Y" "* X\n\n** \n\n** Y"
    (org-evil-heading-open-sibling-or-insert-above t))
  ;; higher-level heading after subheading
  (org-evil--test-with-expected-buffer-text "* X\n\n** Y\n\n* Z" "* X\n\n** Y\n\n* \n\n* Z"
    (org-evil-heading-open-sibling-or-insert-above t)))

(ert-deftest org-evil-heading-test-open-sibling-or-insert-below ()
  "Tests for `org-evil-heading-open-sibling-or-insert-below'."
  :tags '(org-evil org-evil-heading)
  ;; without prefix
  (org-evil--test-with-expected-buffer-text "* X" "* X\n"
    (org-evil-heading-open-sibling-or-insert-below nil))
  ;; with prefix
  (org-evil--test-with-expected-buffer-text "* X" "* X\n* "
    (org-evil-heading-open-sibling-or-insert-below t))
  ;; in subheading
  (org-evil--test-with-expected-buffer-text "* X\n\n** Y" "* X\n\n** Y\n\n** "
    (org-evil-heading-open-sibling-or-insert-below t))
  ;; higher-level heading after subheading
  (org-evil--test-with-expected-buffer-text "* X\n\n** Y\n\n* Z" "* X\n\n** Y\n\n* Z\n\n* "
    (org-evil-heading-open-sibling-or-insert-below t))
  ;; with content
  (org-evil--test-with-expected-buffer-text "* X\nContent" "* X\nContent\n* "
    (goto-char (point-min))
    (org-evil-heading-open-sibling-or-insert-below t)))

(ert-deftest org-evil-heading-test-mode ()
  "Tests for `org-evil-heading-mode'."
  :tags '(org-evil org-evil-heading)

  (org-evil--test-with-buffer-text "* X"
    ;; starting in heading is in heading mode
    (should org-evil-heading-mode)
    ;; disabling org mode disables the heading mode too
    (fundamental-mode)
    (should-not org-evil-heading-mode))

  (org-evil--test-with-buffer-text "* X"
    (should org-evil-heading-mode)
    ;; moving to different buffer
    (org-evil--test-with-buffer-text ""
      (should-not org-evil-heading-mode)))

  ;; moving out of heading
  (org-evil--test-with-buffer-text "Test\n* X"
    (should org-evil-heading-mode)
    (execute-kbd-macro "gg")
    (should-not org-evil-heading-mode))

  ;; moving into heading
  (org-evil--test-with-buffer-text "* X\nTest"
    (should-not org-evil-heading-mode)
    (execute-kbd-macro "gg")
    (should org-evil-heading-mode)))

(ert-deftest org-evil-table-test-mode ()
  "Tests for `org-evil-table-mode'."
  :tags '(org-evil org-evil-table)

  ;; starting in table is in table mode
  (org-evil--test-with-buffer-text "| Test |"
    (should org-evil-table-mode)

    ;; disabling org mode when in table
    (fundamental-mode)
    (should-not org-evil-table-mode))

  ;; moving out of table
  (org-evil--test-with-buffer-text "Test\n| Table |"
    (should org-evil-table-mode)
    (execute-kbd-macro "gg")
    (should-not org-evil-table-mode))

  ;; moving into table
  (org-evil--test-with-buffer-text "| Table |\nTest"
    (should-not org-evil-table-mode)
    (execute-kbd-macro "gg")
    (should org-evil-table-mode)))

(ert-deftest org-evil-block-test-mode ()
  "Tests for `org-evil-block-mode'."
  :tags '(org-evil org-evil-block)

  (org-evil--test-with-buffer-text "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE"
    ;; inside block
    (execute-kbd-macro "k")
    (should org-evil-block-mode)

    ;; end of block
    (execute-kbd-macro "j$")
    (should org-evil-block-mode)

    ;; at start of block
    (execute-kbd-macro "gg")
    (should org-evil-block-mode))

  ;; disabling org mode when in block
  (org-evil--test-with-buffer-text "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE"
    (execute-kbd-macro "k")
    (should org-evil-block-mode)
    (fundamental-mode)
    (should-not org-evil-block-mode))

  ;; moving out of block
  (org-evil--test-with-buffer-text "Test\n#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE"
    (execute-kbd-macro "gg")
    (should-not org-evil-block-mode))

  ;; moving into block
  (org-evil--test-with-buffer-text "#+BEGIN_EXAMPLE\nTest\n#+END_EXAMPLE\nTest"
    (should-not org-evil-block-mode)
    (execute-kbd-macro "gg")
    (should org-evil-block-mode)))

(ert-deftest org-evil-table-test-number-of-columns ()
  "Tests for `org-evil-table-number-of-columns'."
  :tags '(org-evil org-evil-table)
  (org-evil--test-with-buffer-text "| |"
    (should (equal 1 (org-evil-table-number-of-columns))))
  (org-evil--test-with-buffer-text "| | | | | |"
    (should (equal 5 (org-evil-table-number-of-columns)))))

(provide 'org-evil-tests)
;;; org-evil-tests.el ends here
