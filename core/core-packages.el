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
  :init (spacemacs/set-leader-keys "hdK" 'describe-keymap)
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



(provide 'core-packages)
