;;; python-btw-test.el --- Tests of custom modifications for Python.
;;
;; Copyright (c) 2018-2021 Brandon T. Willard
;;
;; Author: Brandon T. Willard
;; URL: https://github.com/brandonwillard/python-btw
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'python)
(require 'python-btw)

(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))

(defmacro python-tests-with-temp-buffer (contents &rest body)
  "Create a `python-mode' enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((python-indent-guess-indent-offset nil))
       (python-mode)
       (python-btw-mode +1)
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))

;;; Tests

;; (ert-deftest test-python-send-shell-multiline ()
;;   "Handle substring with coding cookie in the second line."
;;   (python-tests-with-temp-buffer
;;    ""
;;    (let* ((python-shell-interpreter (executable-find "ipython"))
;;           (comint-process-name (python-shell-get-process-name nil))
;;           (comint-process (python-shell-get-or-create-process))
;;           (comint-shell-buffer (process-buffer comint-process)))
;;      (unwind-protect
;;          (progn
;;            (set-process-query-on-exit-flag comint-process nil)
;;            (python-btw//python-shell-send-string
;;             "
;; print('''
;; this is a multi-line
;; string
;;
;; ''')
;; " comint-process)
;;            (let* ((comint-result nil))
;;              (with-current-buffer comint-shell-buffer
;;                (setq comint-result
;;                      (buffer-substring-no-properties
;;                       comint-last-output-start
;;                       (point))))
;;              (message "---> %s <--" comint-result)
;;              ;; (should (equal comint-result "blah"))
;;              )
;;            (kill-buffer comint-shell-buffer))
;;        (ignore-errors (kill-buffer comint-shell-buffer))))))

;;; python-btw-test.el ends here
