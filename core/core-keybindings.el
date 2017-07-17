(global-set-key (kbd "C-x C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-;") 'set-mark-command)

;; easy navigation
(global-set-key (kbd "C-i") 'previous-line)
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "C-k") 'next-line)
(global-set-key (kbd "C-l") 'forward-char)

(global-set-key (kbd "M-k") 'scroll-up-command)
(global-set-key (kbd "M-i") 'scroll-down-command)
(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "M-l") 'forward-word)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") (lambda () (interactive) (recenter-top-bottom 0)))

(provide 'core-keybindings)
