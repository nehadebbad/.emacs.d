;; Theme
(use-package solarized-theme
  :demand t
  :config
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line t)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; Use more italics
  (setq solarized-use-more-italic t)

  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-light t))

;; Disable toolbar
(tool-bar-mode 0)

;; Disable menu bar
(menu-bar-mode 0)

;; Diable scroll bar
(when window-system
  (scroll-bar-mode -1))

;; Modeline
(use-package spaceline
  :init
  (setq powerline-default-separator 'arrow)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off))

;; Icon pack for neotree
(use-package all-the-icons
  :defer t
  :config
  (message "all the icons loaded"))

(setq my/default-font "Monaco")
(setq my/default-font-size 13)
(setq my/current-font-size my/default-font-size)

(setq my/font-change-increment 1.1)

(defun my/set-font-size ()
  "Set the font to `my/default-font' at `my/current-font-size'."
  (set-frame-font
   (concat my/default-font "-" (number-to-string my/current-font-size))))

(defun my/reset-font-size ()
  "Change font size back to `my/default-font-size'."
  (interactive)
  (setq my/current-font-size my/default-font-size)
  (my/set-font-size))

(defun my/increase-font-size ()
  "Increase current font size by a factor of `my/font-change-increment'."
  (interactive)
  (setq my/current-font-size
        (ceiling (* my/current-font-size my/font-change-increment)))
  (my/set-font-size))

(defun my/decrease-font-size ()
  "Decrease current font size by a factor of `my/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq my/current-font-size
        (max 1
             (floor (/ my/current-font-size my/font-change-increment))))
  (my/set-font-size))

(define-key global-map (kbd "C-)") 'my/reset-font-size)
(define-key global-map (kbd "C-+") 'my/increase-font-size)
(define-key global-map (kbd "C-=") 'my/increase-font-size)
(define-key global-map (kbd "C-_") 'my/decrease-font-size)
(define-key global-map (kbd "C--") 'my/decrease-font-size)

(my/reset-font-size)



(provide 'core-ui)
