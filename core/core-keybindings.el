(defvar my-default-map (make-sparse-keymap)
  "Base keymap for all my leader key commands.")

;; TODO find out where this variable is used
(defvar my-prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defun my/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command.
LONG-NAME if given is stored in `spacemacs/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat dotspacemacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-add-key-based-replacements
      full-prefix (cons name long-name))))
(put 'my/declare-prefix 'lisp-indent-function 'defun)

(defun my/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
         (full-prefix (concat dotspacemacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat dotspacemacs-major-mode-leader-key
                                    " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-add-major-mode-key-based-replacements mode
        full-prefix prefix-name)
      (when (and is-major-mode-prefix dotspacemacs-major-mode-leader-key)
        (which-key-add-major-mode-key-based-replacements mode major-mode-prefix prefix-name)))))
(put 'my/declare-prefix-for-mode 'lisp-indent-function 'defun)

(defun my/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-leader-key' and `dotspacemacs-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.
For convenience, this function will accept additional KEY DEF
pairs. For example,
\(spacemacs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key my-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'my/set-leader-keys 'lisp-indent-function 'defun)

(defalias 'evil-leader/set-key 'set-leader-keys)

(defun my//acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

(defun my//init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `dotspacemacs-major-mode-leader-key'
and `dotspacemacs-major-mode-emacs-leader-key'. If MODE is a
minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (my//acceptable-leader-p
                         dotspacemacs-major-mode-leader-key)
                    dotspacemacs-major-mode-leader-key))
         (leader2 (when (my//acceptable-leader-p
                         dotspacemacs-leader-key)
                    (concat dotspacemacs-leader-key " m")))
         (leaders (delq nil (list leader1 leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :evil-keys ,leaders
              :evil-states (normal motion visual evilified)))
          (boundp prefix)))))

(defun my/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-major-mode-leader-key' and
`dotspacemacs-major-mode-emacs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `spacemacs/set-leader-keys'."
  (let* ((map (intern (format "my-%s-map" mode))))
    (when (my//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'my/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defalias
  'evil-leader/set-key-for-mode
  'my/set-leader-keys-for-major-mode)

(defun my/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-major-mode-leader-key' and
`dotspacemacs-major-mode-emacs-leader-key' for the minor-mode
MODE. MODE should be a quoted symbol corresponding to a valid
minor mode. The rest of the arguments are treated exactly like
they are in `spacemacs/set-leader-keys'."
  (let* ((map (intern (format "my-%s-map" mode))))
    (when (my//init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'my/set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)


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
