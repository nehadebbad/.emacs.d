(global-prettify-symbols-mode t)

;; Matching paranthesis
(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (message "rainbow-delimeters loaded"))

;; Some useful utility functions
(load-file "~/.emacs.d/resources/misc-cmds.el")

;; Wrap lines
(global-visual-line-mode)

;; Kill buffer and its window
(substitute-key-definition 'kill-buffer 'kill-buffer-and-its-windows global-map)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

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


;; Smooth Scrolling
(load-file "~/.emacs.d/resources/smooth-scrolling.el")
(require 'smooth-scrolling)
(setq linum-delay t)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 10000)
(setq auto-save-interval 500)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Easy navigation
(defhydra hydra-navigation (:hint nil
                                :color pink)
("l" forward-char)
("j" backward-char)
("k" next-line)
("i" previous-line)
("w" scroll-down-command)
("s" scroll-up-command)
("a" backward-word)
("d" forward-word)
("q" nil "quit"))
(global-set-key (kbd "C-n") 'hydra-navigation/body)

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

;; Treat terms in camel case as seprate words globally
(global-subword-mode 1)

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

(provide 'core-editing)
