(use-package counsel
  :defer t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (message "ivy,swiper,counsel loaded")
  :bind (
         ("M-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-x l" . counsel-locate)
         ("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)))

(use-package company
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.3)
  (setq company-dabbrev-downcase 0)
  (setq company-minimum-prefix-length 3)
  (message "company-mode loaded"))

(use-package pos-tip
  :config
  (message "pos-tip loaded"))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1)
  (message "company-quickhelp loaded"))

(provide 'core-search-and-completion)
