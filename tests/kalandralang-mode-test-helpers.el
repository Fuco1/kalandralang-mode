;; -*- lexical-binding: t -*-

(require 'kalandralang-mode)
(require 'shut-up)
(require 'buttercup)

(add-to-list 'warning-suppress-log-types '(smie))
(add-to-list 'warning-suppress-types '(smie))

(defmacro kalandralang-test-with-buffer (initial &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (kalandralang-mode)
     (insert ,initial)
     (goto-char (point-min))
     (when (search-forward "|" nil t)
       (delete-char -1))
     ,@body))

(defun kalandralang-test--indent (a b)
  (kalandralang-test-with-buffer a
    (shut-up (indent-region (point-min) (point-max)))
    (equal (buffer-string) b)))

(buttercup-define-matcher-for-binary-function :to-indent-as kalandralang-test--indent
  :expect-match-phrase "Expected %A to indent as %B, was %a."
  :expect-mismatch-phrase "Expected %A not to indent as %B, was %a")

(provide 'kalandralang-mode-test-helpers)
