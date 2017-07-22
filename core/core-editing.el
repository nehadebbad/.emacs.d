(global-prettify-symbols-mode t)

;; Matching paranthesis
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (message "rainbow-delimeters loaded"))

;; Multiple cusors as in Sublime Text
(use-package multiple-cursors
  :defer t
  :config
  (message "multiple-cursors loaded")
  :bind (
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Line editing
(defun quick-copy-line ()
      "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
      (interactive)
      (let ((beg (line-beginning-position 1))
            (end (line-beginning-position 2)))
        (if (eq last-command 'quick-copy-line)
            (kill-append (buffer-substring beg end) (< end beg))
          (kill-new (buffer-substring beg end))))
      (beginning-of-line 2))
(global-set-key (kbd "C-S-C") 'quick-copy-line)

(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))
(global-set-key (kbd "C-S-D") 'quick-cut-line)

(defun move-line-up ()
  "Move the line up and place the point at the beginning of the line"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the line down and place the point at the beginning of the line"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "C-S-K") 'move-line-down)
(global-set-key (kbd "C-S-I") 'move-line-up)

;; Be smart about paranthesis
(use-package smartparens
  :defer t
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  ;; TODO check this
  (defadvice evil-delete-backward-char-and-join
        (around spacemacs/evil-delete-backward-char-and-join activate)
      (if (bound-and-true-p smartparens-strict-mode)
          (call-interactively 'sp-backward-delete-char)
        ad-do-it))
  (message "smartparens loaded"))


;; Flycheck for on the fly syntax checking
(use-package flycheck
  :defer t
  :init
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config
  (with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (message "flycheck loaded"))

;; Insert current date and time
(defvar current-date-time-format "%d-%b-%Y %k:%M"
  "Format of date to insert with `insert-current-date-time' func See help of `format-time-string' for possible replacements")

(defvar current-time-format "%k:%M:%S"
  "Format of date to insert with `insert-current-time' func.Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time)))
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       )

(global-set-key (kbd "C-x C-d") 'insert-current-date-time)
(global-set-key (kbd "C-x C-t") 'insert-current-time)

(defun split-window-right-and-move-cursor ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-below-and-move-cursor ()
  (interactive)
  (split-window-below)
  (other-window 1))

(substitute-key-definition 'split-window-right 'split-window-right-and-move-cursor global-map)
(substitute-key-definition 'split-window-below 'split-window-below-and-move-cursor global-map)

(use-package aggressive-indent
  :defer t
  :init
  (progn
    (my|add-toggle aggressive-indent
      :mode aggressive-indent-mode
      :documentation "Always keep code indented."
      :evil-leader "tI")
    (my|add-toggle aggressive-indent-globally
      :mode aggressive-indent-mode
      :documentation "Always keep code indented globally."
      :evil-leader "t C-I"))
  :config
  (progn
    (add-hook 'diff-auto-refine-mode-hook 'my/toggle-aggressive-indent-off)))

(use-package avy
  :defer t
  :commands (my/avy-open-url my/avy-goto-url avy-pop-mark)
  :init
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)
    (my/set-leader-keys
     "jb" 'avy-pop-mark
     "jj" 'evil-avy-goto-char
     "jJ" 'evil-avy-goto-char-2
     "jl" 'evil-avy-goto-line
     "ju" 'my/avy-goto-url
     "jw" 'evil-avy-goto-word-or-subword-1
     "xo" 'my/avy-open-url))
  :config
  (progn
    (defun my/avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy--generic-jump "https?://" nil 'pre))
    (defun my/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (my/avy-goto-url)
        (browse-url-at-point)))))

(use-package bracketed-paste
  :defer t
  :init
  ;; Enable bracketed-paste for tty
  (add-hook 'tty-setup-hook 'bracketed-paste-enable))

(use-package clean-aindent-mode
  :config (clean-aindent-mode))

;; ignore obsolete function warning generated on startup
(let ((byte-compile-not-obsolete-funcs (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
  (require 'eval-sexp-fu))

(use-package expand-region
    :defer t
    :init (my/set-leader-keys "v" 'er/expand-region)
    :config
    (progn
      ;; add search capability to expand-region
      (when (require 'helm-ag nil t)
        (defadvice er/prepare-for-more-expansions-internal
            (around helm-ag/prepare-for-more-expansions-internal activate)
          ad-do-it
          (let ((new-msg (concat (car ad-return-value)
                                 ", / to search in project, "
                                 "f to search in files, "
                                 "b to search in opened buffers"))
                (new-bindings (cdr ad-return-value)))
            (cl-pushnew
             '("/" (lambda ()
                     (call-interactively
                      'my/helm-project-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("f" (lambda ()
                     (call-interactively
                      'my/helm-files-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("b" (lambda ()
                     (call-interactively
                      'my/helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r")))

(use-package hexl
  :defer t
  :init
  (progn
    (my/set-leader-keys "fh" 'hexl-find-file)
    (my/set-leader-keys-for-major-mode 'hexl-mode
                                       "d" 'hexl-insert-decimal-char
                                       "c" 'hexl-insert-octal-char
                                       "x" 'hexl-insert-hex-char
                                       "X" 'hexl-insert-hex-string
                                       "g" 'hexl-goto-address)
    (evil-define-key 'motion hexl-mode-map
      "]]" 'hexl-end-of-1k-page
      "[[" 'hexl-beginning-of-1k-page
      "h" 'hexl-backward-char
      "l" 'hexl-forward-char
      "j" 'hexl-next-line
      "k" 'hexl-previous-line
      "$" 'hexl-end-of-line
      "^" 'hexl-beginning-of-line
      "0" 'hexl-beginning-of-line)))

(use-package link-hint
  :defer t
  :init
  (my/set-leader-keys
   "xo" 'link-hint-open-link
   "xO" 'link-hint-open-multiple-links))

(use-package move-text
  :defer t
  :init
  (my|define-transient-state move-text
                             :title "Move Text Transient State"
                             :bindings
                               ("J" move-text-down "move down")
                               ("K" move-text-up "move up"))
  (my/set-leader-keys
   "xJ" 'my/move-text-transient-state/move-text-down
   "xK" 'my/move-text-transient-state/move-text-up))

(use-package origami
    :defer t
    :init
    (progn
      (global-origami-mode)
      (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
      (define-key evil-normal-state-map "zc" 'origami-close-node)
      (define-key evil-normal-state-map "zC" 'origami-close-node-recursively)
      (define-key evil-normal-state-map "zO" 'origami-open-node-recursively)
      (define-key evil-normal-state-map "zo" 'origami-open-node)
      (define-key evil-normal-state-map "zr" 'origami-open-all-nodes)
      (define-key evil-normal-state-map "zm" 'origami-close-all-nodes)
      (define-key evil-normal-state-map "zs" 'origami-show-only-node)
      (define-key evil-normal-state-map "zn" 'origami-next-fold)
      (define-key evil-normal-state-map "zp" 'origami-previous-fold)
      (define-key evil-normal-state-map "zR" 'origami-reset)
      (define-key evil-normal-state-map (kbd "z <tab>") 'origami-recursively-toggle-node)
      (define-key evil-normal-state-map (kbd "z TAB") 'origami-recursively-toggle-node)

      (my|define-transient-state fold
        :title "Code Fold Transient State"
        :doc "
 Close^^            Open^^             Toggle^^         Goto^^         Other^^
 ───────^^───────── ─────^^─────────── ─────^^───────── ──────^^────── ─────^^─────────
 [_c_] at point     [_o_] at point     [_a_] at point   [_n_] next     [_s_] single out
 [_C_] recursively  [_O_] recursively  [_A_] all        [_p_] previous [_R_] reset
 [_m_] all          [_r_] all          [_TAB_] like org ^^             [_q_] quit"
        :foreign-keys run
        :on-enter (unless (bound-and-true-p origami-mode) (origami-mode 1))
        :bindings
        ("a" origami-forward-toggle-node)
        ("A" origami-toggle-all-nodes)
        ("c" origami-close-node)
        ("C" origami-close-node-recursively)
        ("o" origami-open-node)
        ("O" origami-open-node-recursively)
        ("r" origami-open-all-nodes)
        ("m" origami-close-all-nodes)
        ("n" origami-next-fold)
        ("p" origami-previous-fold)
        ("s" origami-show-only-node)
        ("R" origami-reset)
        ("TAB" origami-recursively-toggle-node)
        ("<tab>" origami-recursively-toggle-node)
        ("q" nil :exit t)
        ("C-g" nil :exit t)
        ("<SPC>" nil :exit t))
      ;; Note: The key binding for the fold transient state is defined in
      ;; evil config
      ))

(use-package smartparens
    :defer t
    :commands (sp-split-sexp sp-newline sp-up-sexp)
    :init
    (progn
      ;; functions
      (defun my//adaptive-smartparent-pair-overlay-face ()
        (set-face-attribute 'sp-pair-overlay-face nil
                            :inherit 'lazy-highlight
                            :background nil
                            :foreground nil))
      (defun my/smartparens-pair-newline (id action context)
        (save-excursion
          (newline)
          (indent-according-to-mode)))

      (defun my/smartparens-pair-newline-and-indent (id action context)
        (my/smartparens-pair-newline id action context)
        (indent-according-to-mode))

      (defun my/smart-closing-parenthesis ()
        (interactive)
        (let* ((sp-navigate-close-if-unbalanced t)
               (current-pos (point))
               (current-line (line-number-at-pos current-pos))
               (next-pos (save-excursion
                           (sp-up-sexp)
                           (point)))
               (next-line (line-number-at-pos next-pos)))
          (cond
           ((and (= current-line next-line)
                 (not (= current-pos next-pos)))
            (sp-up-sexp))
           (t
            (insert-char ?\))))))
      ;; settings
      (setq sp-show-pair-delay 0.2
            ;; fix paren highlighting in normal mode
            sp-show-pair-from-inside t
            sp-cancel-autoskip-on-backward-movement nil
            sp-highlight-pair-overlay nil
            sp-highlight-wrap-overlay nil
            sp-highlight-wrap-tag-overlay nil)
      (my/add-to-hooks (if dotspacemacs-smartparens-strict-mode
                                  'smartparens-strict-mode
                                'smartparens-mode)
                              '(prog-mode-hook comint-mode-hook))
      ;; enable smartparens-mode in `eval-expression'
      (defun my//conditionally-enable-smartparens-mode ()
        "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
        (if (eq this-command 'eval-expression)
            (smartparens-mode)))
      (add-hook 'minibuffer-setup-hook 'my//conditionally-enable-smartparens-mode)
      ;; toggles
      (my|add-toggle smartparens
        :mode smartparens-mode
        :documentation "Enable smartparens."
        :evil-leader "tp")
      (my|add-toggle smartparens-globally
        :mode smartparens-mode
        :documentation "Enable smartparens globally."
        :evil-leader "t C-p")
      ;; key bindings
      (my/set-leader-keys
        "js" 'sp-split-sexp
        "jn" 'sp-newline))
    :config
    (progn
      (require 'smartparens-config)
      (my//adaptive-smartparent-pair-overlay-face)
      (show-smartparens-global-mode +1)
      ;; don't create a pair with single quote in minibuffer
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
      (sp-pair "{" nil :post-handlers
               '(:add (my/smartparens-pair-newline-and-indent "RET")))
      (sp-pair "[" nil :post-handlers
               '(:add (my/smartparens-pair-newline-and-indent "RET")))
      (when dotspacemacs-smart-closing-parenthesis
        (define-key evil-insert-state-map ")"
          'my/smart-closing-parenthesis))))

(use-package my-whitespace-cleanup
    :commands (my-whitespace-cleanup-mode
               global-my-whitespace-cleanup-mode)
    :init
    (progn
      (my|add-toggle whitespace-cleanup
        :mode my-whitespace-cleanup-mode
        :documentation "Automatic whitespace clean up."
        :on-message (my-whitespace-cleanup/on-message)
        :evil-leader "tW")
      (my|add-toggle global-whitespace-cleanup
        :mode global-my-whitespace-cleanup-mode
        :status my-whitespace-cleanup-mode
        :on (let ((my-whitespace-cleanup-globally t))
              (my-whitespace-cleanup-mode))
        :off (let ((my-whitespace-cleanup-globally t))
               (my-whitespace-cleanup-mode -1))
        :on-message (my-whitespace-cleanup/on-message t)
        :documentation "Global automatic whitespace clean up."
        :evil-leader "t C-S-w")
      (with-eval-after-load 'ws-butler
        (when dotspacemacs-whitespace-cleanup
          (my/toggle-global-whitespace-cleanup-on)))))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package ws-butler)

(provide 'core-editing)
