;;; python-btw.el --- Custom modifications for Python.
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
;; Custom modifications for Python.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'f)
(require 'python)

(defvar python-btw--python-help-setup-code
  "
try:
   get_ipython().inspector.pinfo(%1$s, detail_level=0)
except Exception:
   help(%1$s)
"
  "Code used to extract help information from a comint REPL.")


(defun python-btw//python-shell-send-string (string &optional process msg)
  "Send STRING to inferior IPython PROCESS.  Uses %cpaste for multiline input.

When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list (read-string "Python command: ") nil t))
  (let* ((process (or process (python-shell-get-process-or-error msg)))
         (process-executable (car (process-command process))))
    (if (string-match ".\n+." string)
        ;; Handle multi-line strings
        (if (or (s-contains? "jupyter" process-executable t)
                (s-contains? "ipython" process-executable t))
            (comint-send-string process
                                (format "get_ipython().run_line_magic('cpaste', '-q')\n%s\n--\n" string))
          (let* ((temp-file-name (python-shell--save-temp-file string))
                 (file-name (or (buffer-file-name) temp-file-name)))
            (python-shell-send-file file-name process temp-file-name t)))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n")))))

(defun python-btw//python-shell-append-to-output (string)
  "Append STRING to `comint' display output."
  (let ((buffer (current-buffer))
        (py-buffer (process-buffer (python-shell-get-process))))
    (unless (eq buffer py-buffer)
      (save-mark-and-excursion
        (with-current-buffer py-buffer
          (let ((oldpoint (point)))
            (goto-char (process-mark (python-shell-get-process)))
            (insert (propertize string 'read-only t))
            (set-marker (process-mark (python-shell-get-process)) (point))
            (goto-char oldpoint))
          ))
      )))

(defun python-btw//python-shell-send-string-echo (string &optional process msg)
  (python-btw//python-shell-append-to-output string)
  (python-shell-send-string string process msg))

(defun python-btw//add-missing-newline (line)
  (if (s-ends-with? "\n" line)
      line
    (concat line "\n")))

(defun python-btw//python-shell-send-line-echo ()
  "Send and echo a literal line to the `comint' buffer.
Ignores beginning white-space."
  (interactive)
  (let (start end line)
    (save-excursion
      (end-of-line)
      ;; or `forward-line'?
      (setq end (point))
      (beginning-of-line-text)
      (setq start (point)))
    (setq line (buffer-substring-no-properties start end))
    (python-btw//python-shell-send-string-echo (python-btw//add-missing-newline line))))

(defun python-btw//python-shell-send-syntax-line-echo (&optional send-main msg)
  "Send and echo a \"syntactical\" line to the `comint' buffer."
  ;; (interactive)
  ;; (let (start end line)
  ;;   (save-excursion
  ;;     (python-nav-end-of-statement)
  ;;     (setq end (point))
  ;;     (python-nav-beginning-of-statement)
  ;;     (setq start (point)))
  ;;   (setq line (buffer-substring-no-properties start end))
  ;;   (python-btw//python-shell-send-string-echo line))
  (interactive (list current-prefix-arg t))
  (if (region-active-p)
      (python-btw//python-shell-send-region-echo (region-beginning) (region-end) send-main msg)
    (python-btw//python-shell-send-region-echo
     (save-excursion (python-nav-beginning-of-statement))
     (save-excursion (python-nav-end-of-statement))
     send-main msg t)))

(defalias #'python-btw//python-shell-send-syntax-line-echo #'python-btw//python-shell-send-statement-echo "docstring")

(defun python-btw//python-shell-send-region-echo (start end &optional send-main msg no-cookie)
  "Send the selected region of text to the Python process.  A new line will be appended if one
isn't already present in the text region.

This is a replacement for `python-shell-send-region'.
"
  ;; (interactive
  ;;   (list (region-beginning) (region-end) current-prefix-arg t))
  ;; (let* ((substring (buffer-substring-no-properties start end))
  ;;        (process (python-shell-get-process-or-error msg))
  ;;        (send-string (if (s-ends-with? "\n" substring)
  ;;                         substring
  ;;                       (s-append "\n" substring))))
  ;;   (python-btw//python-shell-send-string-echo send-string process msg))
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (let* ((string (buffer-substring-no-properties start end)
          ;; (python-shell-buffer-substring start end (not send-main) no-cookie)
          )
         (process (python-shell-get-process-or-error msg))
         (original-string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    ;; (message "Sent: %s..." (match-string 1 original-string))
    ;; Recalculate positions to avoid landing on the wrong line if
    ;; lines have been removed/added.
    (with-current-buffer (process-buffer process)
      (compilation-forget-errors))
    (python-btw//python-shell-send-string-echo string process msg)
    (deactivate-mark)))

(defun python-btw//python-help--display-for-string (proc string)
  "Originally from `python-x.el'"
  (let ((buffer (help-buffer))
        (output (python-shell-send-string-no-output
                 (format python-btw--python-help-setup-code string) proc)))
    (unless (s-blank? output)
      (with-current-buffer buffer
        ;; (special-mode)
        (help-mode)
        (buffer-disable-undo)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output)
          (ansi-color-apply-on-region (point-min) (point-max))
          (whitespace-cleanup)
          (goto-char (point-min)))
        (set-buffer-modified-p nil)
        (setq truncate-lines nil)
        (setq word-wrap t)
        ;; (setq font-lock-defaults python-font-lock-keywords)
        (setq python-help--parent-proc proc))
      (display-buffer buffer))))

(defun python-btw//python-help-for-region-or-symbol (string)
  "Display documentation for the current region or symbol at point. If a prefix
  argument is given, prompt for a statement to inspect.
  Originally from `python-x.el'"
  (interactive (let* ((string (if (use-region-p)
                                  (buffer-substring-no-properties (region-beginning)
                                                                  (region-end))
                                (python-info-current-symbol))))
                  (list (if current-prefix-arg
                            (read-string "Help for: " string t)
                          string))))
  (unless (s-blank? string)
    (python-btw//python-help--display-for-string (python-shell-get-process)
                                      string)))

(defun python-btw//noop (&rest r)
  nil)

(defun python-btw//comint-output-turn-buffer-read-only (&rest r)
  (add-text-properties comint-last-output-start (line-end-position 0)
                       '(read-only "Process output is read-only."
                                   rear-nonsticky (inhibit-line-move-field-capture))))

(defun python-btw//setup-readonly-comint-output ()
  ;; (advice-add #'comint-output-filter :after #'python-btw//comint-output-turn-buffer-read-only)
  (add-hook 'comint-output-filter-functions #'python-btw//comint-output-turn-buffer-read-only 'append t))

;;;###autoload
(define-minor-mode python-btw-mode
  "Activate `python-btw' mode."
  :group 'python-btw
  :require 'python
  :init-value nil
  :global t
  (if python-btw-mode
      (progn
        ;; Make comint output read-only
        (add-hook 'inferior-python-mode-hook #'python-btw//setup-readonly-comint-output t)

        ;; This calls `ipython --version', which is costly, so disable it.
        (advice-add #'python-btw//python-setup-shell :override #'python-btw//noop)

        (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
        (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")

        (when (fboundp #'spacemacs//python-setup-company)
          ;; `python-mode-local-vars-hook' is supposed to do this, but I think
          ;; `hack-local-variables-hook' isn't being called (or is locally
          ;; overwritten, causing the global spacemacs
          ;; `spacemacs//run-local-vars-mode-hook' to be skipped).
          (spacemacs//python-setup-company))

        (advice-add #'python-shell-send-string :override
                    #'python-btw//python-shell-send-string)

        (when (featurep 'lsp-ui)
          (defun python-btw//python-help-for-region-or-symbol (&rest r)
            (interactive)
            (lsp-ui-doc--make-request)))

        (when (fboundp #'spacemacs/set-leader-keys-for-major-mode)
          (spacemacs/set-leader-keys-for-major-mode 'python-mode
            "hh" #'python-btw//python-help-for-region-or-symbol
            "sr" #'python-btw//python-shell-send-region-echo
            "se" #'python-btw//python-shell-send-syntax-line-echo
            "sl" #'python-btw//python-shell-send-line-echo))

        (when (featurep 'company)
          (defun python--private-lessp (x y)
            (cond
             ((and (string-prefix-p "_" x)
                   (not (string-prefix-p "_" y))) nil)
             ((and (string-prefix-p "_" y)
                   (not (string-prefix-p "_" x))) t)
             (t (string-lessp x y))))

          (defun python-extras/company-transform-python (candidates)
            "De-prioritize internal/private Python variables (e.g. '_blah') in completion list ordering.

See `company-transformers'."
            ;; TODO: Find a replacement for `seq-sort-by'.
            ;; (seq-sort-by #'company-strip-prefix #'python--private-lessp
            ;;              candidates)
            (seq-sort-by #'identity #'python--private-lessp candidates))

          (defun python-extras/python-company-conf ()
            (add-to-list 'company-transformers #'python-extras/company-transform-python t))

          (add-hook 'python-mode-hook #'python-extras/python-company-conf)
          (add-hook 'inferior-python-mode-hook #'python-extras/python-company-conf)))
    (progn
      (remove-hook 'inferior-python-mode-hook #'python-btw//setup-readonly-comint-output t)

      (advice-remove #'python-btw//python-setup-shell #'python-btw//noop)

      (setq python-shell-completion-native-disabled-interpreters
            (delete 'python-shell-completion-native-disabled-interpreters "jupyter"))
      (setq python-shell-completion-native-disabled-interpreters
            (delete 'python-shell-completion-native-disabled-interpreters "ipython"))

      (advice-remove #'python-shell-send-string #'python-btw//python-shell-send-string)

      (when (featurep 'company)
        (remove-hook 'python-mode-hook #'python-extras/python-company-conf)
        (remove-hook 'inferior-python-mode-hook #'python-extras/python-company-conf)))))

(provide 'python-btw)
