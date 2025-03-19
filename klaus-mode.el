;; Emacs Mode for klaus

(define-derived-mode klaus-mode prog-mode "klaus"
  "A simple major mode for klaus."

  ;; Define Keywords for highlighting
  (setq klaus-font-lock-keywords
        '(("\\<\\(if\\|else\\|loop\\|puts\\|read\\|dup\\|swap\\)\\>" . font-lock-keyword-face))) ; keywords

  ;; Apply the syntax highlighting
  (setq font-lock-defaults '((klaus-font-lock-keywords)))

  ;; Ensure that comment rule takes priority
  (font-lock-add-keywords 'klaus-mode
                          '(("#.*" . (0 font-lock-comment-face prepend))))

  ;; Local variables for comment handling
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-line-function #'indent-relative))

;; Provide the feature
(provide 'klaus-mode)

;; Associate the mode with .kl files
(add-to-list 'auto-mode-alist '("\\.kl\\'" . klaus-mode))
