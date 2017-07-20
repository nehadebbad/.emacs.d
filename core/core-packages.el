(use-package ace-window
  :defer t
  :init
  (progn
    (my/set-leader-keys
     "bD" 'my/ace-kill-this-buffer
     ;; FIXME: Needs new binding.
     ;; "wC" 'spacemacs/ace-center-window
     "wD" 'my/ace-delete-window
     "wM" 'ace-swap-window
     "wW" 'ace-window)
    ;; set ace-window keys to home-row
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(evilified-state-evilify-map archive-mode-map
  :mode archive-mode
  :eval-after-load archive-mode)

(use-package bookmark
  :defer t
  :init
  (progn
    (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")
          ;; autosave each change
          bookmark-save-flag 1)
    (my/set-leader-keys "fb" 'bookmark-jump)))

;; explicitly derive conf-mode from text-mode major-mode
(add-hook 'conf-mode-hook 'my/run-text-mode-hooks)

;; Dired
(my/set-leader-keys
 "ad" 'dired
 "fj" 'dired-jump
 "jd" 'dired-jump
 "jD" 'dired-jump-other-window)

(use-package dired-x
  :commands (dired-jump
             dired-jump-other-window
             dired-omit-mode))

(electric-indent-mode)

(use-package ediff
  :defer t
  :init
  (progn
    ;; first we set some sane defaults
    (setq-default
     ediff-window-setup-function 'ediff-setup-windows-plain
     ;; emacs is evil and decrees that vertical shall henceforth be horizontal
     ediff-split-window-function 'split-window-horizontally
     ediff-merge-split-window-function 'split-window-horizontally)
    ;; show org ediffs unfolded
    (require 'outline)
    (add-hook 'ediff-prepare-buffer-hook #'show-all)
    ;; restore window layout when done
    (add-hook 'ediff-quit-hook #'winner-undo)))

(use-package eldoc
  :defer t
  :config
  (progn
    ;; enable eldoc in `eval-expression'
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    ;; enable eldoc in IELM
    (add-hook 'ielm-mode-hook #'eldoc-mode)))

(use-package evil-escape
  :init (evil-escape-mode)
  :config (message "evil-escape loaded"))

(define-key evil-evilified-state-map (kbd dotspacemacs-leader-key) my-default-map)

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (progn
    (define-key evil-visual-state-map (kbd "*")
      'evil-visualstar/begin-search-forward)
    (define-key evil-visual-state-map (kbd "#")
      'evil-visualstar/begin-search-backward))
  :config
  (message "evil-visualstar loaded"))

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize)
  (message "exec-path-from-shell loaded"))

(use-package help-fns+
  :commands (describe-keymap)
  :init (my/set-leader-keys "hdK" 'describe-keymap)
  :config (message "help-fns+ loaded"))

(use-package image-mode
  :config (evilified-state-evilify-map image-mode-map :mode image-mode))

(use-package imenu
  :defer t
  :init (my/set-leader-keys "ji" 'imenu))

(use-package linum
  :init
  (progn
    (setq linum-format "%4d")
    (my|add-toggle line-numbers
                          :mode linum-mode
                          :documentation "Show the line numbers."
                          :evil-leader "tn")
    (advice-add #'linum-update-window
                :after #'my//linum-update-window-scale-fix)
    (advice-add #'linum-on
                :around #'my//linum-on))
  :config
  (progn
    (when (my//linum-backward-compabitility)
      (add-hook 'prog-mode-hook 'linum-mode)
      (add-hook 'text-mode-hook 'linum-mode))
    (when dotspacemacs-line-numbers
      (global-linum-mode))))

(evilified-state-evilify-map occur-mode-map
  :mode occur-mode)

(evilified-state-evilify-map package-menu-mode-map
  :mode package-menu-mode)

(use-package page-break-lines
  :init
  (global-page-break-lines-mode t)
  :config
  (message "page-break-lines loaded"))

(use-package pcre2el
  :defer t
  :init
  (progn
    (my/declare-prefix "xr" "regular expressions")
    (my/declare-prefix "xre" "elisp")
    (my/declare-prefix "xrp" "pcre")
    (my/set-leader-keys
     "xr/"  'rxt-explain
     "xr'"  'rxt-convert-to-strings
     "xrt"  'rxt-toggle-elisp-rx
     "xrx"  'rxt-convert-to-rx
     "xrc"  'rxt-convert-syntax
     "xre/" 'rxt-explain-elisp
     "xre'" 'rxt-elisp-to-strings
     "xrep" 'rxt-elisp-to-pcre
     "xret" 'rxt-toggle-elisp-rx
     "xrex" 'rxt-elisp-to-rx
     "xrp/" 'rxt-explain-pcre
     "xrp'" 'rxt-pcre-to-strings
     "xrpe" 'rxt-pcre-to-elisp
     "xrpx" 'rxt-pcre-to-rx)))

(evilified-state-evilify process-menu-mode process-menu-mode-map)

(use-package projectile
    :commands (projectile-ack
               projectile-ag
               projectile-compile-project
               projectile-dired
               projectile-find-dir
               projectile-find-file
               projectile-find-tag
               projectile-test-project
               projectile-grep
               projectile-invalidate-cache
               projectile-kill-buffers
               projectile-multi-occur
               projectile-project-p
               projectile-project-root
               projectile-recentf
               projectile-regenerate-tags
               projectile-replace
               projectile-replace-regexp
               projectile-run-async-shell-command-in-root
               projectile-run-shell-command-in-root
               projectile-switch-project
               projectile-switch-to-buffer
               projectile-vc)
    :init
    (progn
      ;; note for Windows: GNU find or Cygwin find must be in path to enable
      ;; fast indexing
      (when (and (my/system-is-mswindows) (executable-find "find"))
        (setq  projectile-indexing-method 'alien
               projectile-generic-command "find . -type f"))
      (setq projectile-sort-order 'recentf
            projectile-cache-file (concat spacemacs-cache-directory
                                          "projectile.cache")
            projectile-known-projects-file (concat spacemacs-cache-directory
                                                   "projectile-bookmarks.eld"))
      (my/set-leader-keys
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "p%" 'projectile-replace-regexp
        "pa" 'projectile-toggle-between-implementation-and-test
        "pb" 'projectile-switch-to-buffer
        "pc" 'projectile-compile-project
        "pd" 'projectile-find-dir
        "pD" 'projectile-dired
        "pf" 'projectile-find-file
        "pF" 'projectile-find-file-dwim
        "pg" 'projectile-find-tag
        "pG" 'projectile-regenerate-tags
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "pp" 'projectile-switch-project
        "pr" 'projectile-recentf
        "pR" 'projectile-replace
        "pT" 'projectile-test-project
        "pv" 'projectile-vc))
    :config
    (progn
      (projectile-global-mode)))

(use-package recentf
  :defer t
  :init
  (progn
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
    (setq recentf-save-file (concat spacemacs-cache-directory "recentf")
          recentf-max-saved-items 1000
          recentf-auto-cleanup 'never
          recentf-auto-save-timer (run-with-idle-timer 600 t
                                                       'recentf-save-list)))
  :config
  (progn
    (add-to-list 'recentf-exclude
                 (file-truename spacemacs-cache-directory))
    (add-to-list 'recentf-exclude (file-truename package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

(use-package savehist
  :init
  (progn
    ;; Minibuffer history
    (setq savehist-file (concat spacemacs-cache-directory "savehist")
          enable-recursive-minibuffers t ; Allow commands in minibuffers
          history-length 1000
          savehist-additional-variables '(mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 60)
    (savehist-mode t)))

(use-package saveplace
  :init
  (progn
    (if (fboundp 'save-place-mode)
        ;; Emacs 25 has a proper mode for `save-place'
        (save-place-mode)
      (setq save-place t))
    ;; Save point position between sessions
    (setq save-place-file (concat spacemacs-cache-directory "places"))))

(use-package subword
    :defer t
    :init
    (progn
      (unless (category-docstring ?U)
        (define-category ?U "Uppercase")
        (define-category ?u "Lowercase"))
      (modify-category-entry (cons ?A ?Z) ?U)
      (modify-category-entry (cons ?a ?z) ?u)
      (make-variable-buffer-local 'evil-cjk-word-separating-categories)
      (defun my//subword-enable-camel-case ()
        "Add support for camel case to subword."
        (if subword-mode
            (push '(?u . ?U) evil-cjk-word-separating-categories)
          (setq evil-cjk-word-separating-categories
                (default-value 'evil-cjk-word-separating-categories))))
      (add-hook 'subword-mode-hook 'my//subword-enable-camel-case)
      (my|add-toggle camel-case-motion
        :mode subword-mode
        :documentation "Toggle CamelCase motions."
        :evil-leader "tc")
      (my|add-toggle camel-case-motion-globally
        :mode global-subword-mode
        :documentation "Globally toggle CamelCase motions."
        :evil-leader "t C-c")))

(evilified-state-evilify-map tar-mode-map
  :mode tar-mode
  :eval-after-load tar-mode)

(require 'uniquify)
;; When having windows with repeated filenames, uniquify them
;; by the folder they are in rather those annoying <2>,<3>,.. etc
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      ;; don't screw special buffers
      uniquify-ignore-buffers-re "^\\*")

;; gravatars from magit use this to store their cache
(setq url-configuration-directory (concat spacemacs-cache-directory "url/"))

(use-package whitespace
    :defer t
    :init
    (progn
      (setq my-show-trailing-whitespace t)
      (defun my//show-trailing-whitespace ()
        (when my-show-trailing-whitespace
          (set-face-attribute 'trailing-whitespace nil
                              :background
                              (face-attribute 'font-lock-comment-face
                                              :foreground))
          (setq show-trailing-whitespace 1)))
      (add-hook 'prog-mode-hook 'my//show-trailing-whitespace)

      (my|add-toggle whitespace
        :mode whitespace-mode
        :documentation "Display whitespace."
        :evil-leader "tw")
      (my|add-toggle whitespace-globally
        :mode global-whitespace-mode
        :documentation "Display whitespace globally."
        :evil-leader "t C-w")

      (defun my//set-whitespace-style-for-diff ()
        "Whitespace configuration for `diff-mode'"
        (setq-local whitespace-style '(face
                                       tabs
                                       tab-mark
                                       spaces
                                       space-mark
                                       trailing
                                       indentation::space
                                       indentation::tab
                                       newline
                                       newline-mark)))
      (add-hook 'diff-mode-hook 'whitespace-mode)
      (add-hook 'diff-mode-hook 'my//set-whitespace-style-for-diff))
    :config
    (progn
      (set-face-attribute 'whitespace-space nil
                          :background nil
                          :foreground (face-attribute 'font-lock-warning-face
                                                      :foreground))
      (set-face-attribute 'whitespace-tab nil
                          :background nil)
      (set-face-attribute 'whitespace-indentation nil
                          :background nil)))

(use-package winner
  :init
  (progn
    (winner-mode t)
    (setq my/winner-boring-buffers '("*Completions*"
                                     "*Compile-Log*"
                                     "*inferior-lisp*"
                                     "*Fuzzy Completions*"
                                     "*Apropos*"
                                     "*Help*"
                                     "*cvs*"
                                     "*Buffer List*"
                                     "*Ibuffer*"
                                     "*esh command on file*"))
    (setq winner-boring-buffers
          (append winner-boring-buffers my/winner-boring-buffers))
    (winner-mode t)))

(provide 'core-packages)
