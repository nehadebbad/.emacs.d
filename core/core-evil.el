;; Load async
(use-package async
  :demand t
  :config
  (message "async loaded"))

;; Configure bind-map
(use-package bind-map
  :demand t
  :config
  (bind-map my-default-map
    :prefix-cmd spacemacs-cmds
    :evil-keys (dotspacemacs-leader-key)
    :override-minor-modes t
    :override-mode-name spacemacs-leader-override-mode)
  (message "bind-map loaded"))

;; Configure evil
(use-package evil
  :demand t
  :init
  ; TODO Customize these to my liking
  (defvar my-evil-cursors '(("normal" "DarkGoldenrod2" box)
                       ("insert" "chartreuse3" (bar . 2))
                       ("emacs" "Skyblue" box)
                       ("replace" "chocolate" (hbar . 2))
                       ; TODO include this face after defining evilified state
                       ;("evilified" "LightGoldenrod3" box)
                       ("visual" "gray" (hbar . 2))
                       ("motion" "plum3" box)
                       ; TODO install evil-iedit for these faces later
                       ;("iedit" "firebrick1" box)
                       ;("iedit-insert" "firebrick1" (bar . 2))
                       )
    "Colors assigned to evil states with cursor definitions.")
  :config
  ;; Set evil-search-module to evil-search
  (evil-select-search-module 'evil-search-module 'evil-search)
  (cl-loop for (state color cursor) in my-evil-cursors
           do
           (set (intern (format "evil-%s-state-cursor" state))
                (list (when dotspacemacs-colorize-cursor-according-to-state color)
                      cursor)))
  (evil-mode 1)
  ;; evil ex-command
  (define-key evil-normal-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
  (define-key evil-visual-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
  (define-key evil-motion-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
  (setq evil-ex-substitute-global dotspacemacs-ex-substitute-global)

  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect
  (customize-set-variable 'evil-want-Y-yank-to-eol dotspacemacs-remap-Y-to-y$)

  ;; bind evil-jump-forward for GUI only.
  (define-key evil-motion-state-map [C-i] 'evil-jump-forward)

  ;; Make the current definition and/or comment visible.
  (define-key evil-normal-state-map "zf" 'reposition-window)
  ;; toggle maximize buffer
  (define-key evil-window-map (kbd "o") 'spacemacs/toggle-maximize-buffer)
  (define-key evil-window-map (kbd "C-o") 'spacemacs/toggle-maximize-buffer)
  ;; make cursor keys work
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)
  (my/set-leader-keys "re" 'evil-show-registers)

  ;; motions keys for help buffers
  ;; TODO check if these bindings reflect for helm.Do it when
  ;; configuring helm
  (evil-define-key 'motion help-mode-map (kbd "<escape>") 'quit-window)
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
  (evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)
  (setq-default evil-shift-width 2)
  (add-hook 'after-change-major-mode-hook 'my//set-evil-shift-width)

  ;; Keep the region active when shifting
  (when dotspacemacs-retain-visual-state-on-shift
    (evil-map visual "<" "<gv")
    (evil-map visual ">" ">gv"))

  ;; move selection up and down
  (when dotspacemacs-visual-line-move-text
    (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
    (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

  (evil-ex-define-cmd "enew" 'my/new-empty-buffer)

  (define-key evil-normal-state-map (kbd "K") 'my/evil-smart-doc-lookup)
  (define-key evil-normal-state-map (kbd "gd") 'my/jump-to-definition)
  (define-key evil-normal-state-map (kbd "gD") 'my/jump-to-definition-other-window)

  ;; scrolling transient state
  (my|define-transient-state scroll
    :title "Scrolling Transient State"
    :doc "
 Buffer^^^^              Full page^^^^     Half page^^^^        Line/column^^^^
 ──────^^^^───────────── ─────────^^^^──── ─────────^^^^─────── ───────────^^^^─────
 [_<_/_>_] beginning/end [_f_/_b_] down/up [_J_/_K_] down/up    [_j_/_k_] down/up
  ^ ^ ^ ^                 ^ ^ ^ ^          [_H_/_L_] left/right [_h_/_l_] left/right
  ^ ^ ^ ^                 ^ ^ ^ ^          [_d_/_u_] down/up     ^ ^ ^ ^"
    :bindings
    ;; buffer
    ("<" evil-goto-first-line)
    (">" evil-goto-line)
    ;; full page
    ("f" evil-scroll-page-down)
    ("b" evil-scroll-page-up)
    ;; half page
    ("d" evil-scroll-down)
    ("u" evil-scroll-up)
    ("J" evil-scroll-down)
    ("K" evil-scroll-up)
    ("H" evil-scroll-left)
    ("L" evil-scroll-right)
    ;; lines and columns
    ("j" evil-scroll-line-down)
    ("k" evil-scroll-line-up)
    ("h" evil-scroll-column-left)
    ("l" evil-scroll-column-right))

  (my/set-leader-keys
    ;; buffer
    "N<" 'my/scroll-transient-state/evil-goto-first-line
    "N>" 'my/scroll-transient-state/evil-goto-line
    ;; full page
    "Nf" 'mc/scroll-transient-state/evil-scroll-page-down
    "Nb" 'mc/scroll-transient-state/evil-scroll-page-up
    ;; half page
    "Nd" 'my/scroll-transient-state/evil-scroll-down
    "Nu" 'my/scroll-transient-state/evil-scroll-up
    "NJ" 'my/scroll-transient-state/evil-scroll-down
    "NK" 'my/scroll-transient-state/evil-scroll-up
    "NH" 'my/scroll-transient-state/evil-scroll-left
    "NL" 'my/scroll-transient-state/evil-scroll-right
    ;; lines and columns
    "Nj" 'my/scroll-transient-state/evil-scroll-line-down
    "Nk" 'my/scroll-transient-state/evil-scroll-line-up
    "Nh" 'my/scroll-transient-state/evil-scroll-column-left
    "Nl" 'my/scroll-transient-state/evil-scroll-column-right))

(provide 'core-evil)
