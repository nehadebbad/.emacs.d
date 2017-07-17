(use-package magit
  :defer t
  :config
  ;;open magit-status in a fullframe buffer
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (setq magit-completing-read-function 'ivy-completing-read)
  (message "magit loaded")
  :bind (
         ("C-x g" . magit-status)))

(provide 'core-version-control)
