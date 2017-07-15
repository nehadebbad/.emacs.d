(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-delay 1.0)
 '(company-quickhelp-max-lines 10)
 '(company-quickhelp-use-propertized-text t)
 '(custom-safe-themes
   (quote
    ("c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" default)))
 '(ein:complete-on-dot nil)
 '(ein:jupyter-server-args (quote ("--no-browser")))
 '(ein:notebook-modes (quote (ein:notebook-python-mode)))
 '(elpy-company-post-completion-function (quote elpy-company-post-complete-parens))
 '(package-selected-packages (quote (py-autopep8 org-plus-contrib use-package)))
 '(persp-auto-resume-time -1.0)
 '(persp-keymap-prefix "w"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:background "#eee8d5" :foreground "#6c71c4"))))
 '(company-tooltip-annotation ((t (:background "#eee8d5" :foreground "#d33682"))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :background "dark orange" :foreground "black"))))
 '(company-tooltip-common ((t (:background "#eee8d5" :foreground "#2aa198" :underline nil))))
 '(company-tooltip-common-selection ((t (:background "dark orange" :foreground "black" :underline nil))))
 '(company-tooltip-selection ((t (:background "dark orange" :foreground "black"))))
 '(ein:cell-input-prompt ((t (:inherit header-line :background "#3e3d31" :foreground "DarkGoldenrod2"))))
 '(ivy-minibuffer-match-face-1 ((t (:background "navajo white"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "violet" :foreground "black" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "LightSteelBlue1" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:weight bold))))
 '(mode-line ((t (:background "light gray" :foreground "black" :inverse-video nil :box nil))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray30" :foreground "white"))))
 '(powerline-active2 ((t (:inherit mode-line :background "dark gray" :foreground "black"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "DarkGoldenrod2"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit outline-1))))
 '(sp-pair-overlay-face ((t (:background "#fdf6e3"))))
 '(spaceline-flycheck-error ((t (:foreground "red"))))
 '(spaceline-flycheck-info ((t (:foreground "dark green"))))
 '(spaceline-flycheck-warning ((t (:foreground "dark orange")))))
