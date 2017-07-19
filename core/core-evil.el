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
    "Nl" 'my/scroll-transient-state/evil-scroll-column-right)

  ;; pasting transient-state
  (evil-define-command my//transient-state-0 ()
    :keep-visual t
    :repeat nil
    (interactive)
    (if current-prefix-arg
        (progn
          (setq this-command #'digit-argument)
          (call-interactively #'digit-argument))
      (setq this-command #'evil-beginning-of-line
            hydra-deactivate t)
      (call-interactively #'evil-beginning-of-line)))

  (my|define-transient-state paste
    :title "Pasting Transient State"
    :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
    :bindings
    ("C-j" evil-paste-pop)
    ("C-k" evil-paste-pop-next)
    ("p" evil-paste-after)
    ("P" evil-paste-before)
    ("0" my//transient-state-0))

  (when dotspacemacs-enable-paste-transient-state
    (define-key evil-normal-state-map
      "p" 'my/paste-transient-state/evil-paste-after)
    (define-key evil-normal-state-map
      "P" 'my/paste-transient-state/evil-paste-before))

  ;; fold transient state
  (when (eq 'evil dotspacemacs-folding-method)
    (my|define-transient-state fold
      :title "Code Fold Transient State"
      :doc "
 Close^^          Open^^              Toggle^^             Other^^
 ───────^^──────  ─────^^───────────  ─────^^────────────  ─────^^───
 [_c_] at point   [_o_] at point      [_a_] around point   [_q_] quit
 ^^               [_O_] recursively   ^^
 [_m_] all        [_r_] all"
      :foreign-keys run
      :bindings
      ("a" evil-toggle-fold)
      ("c" evil-close-fold)
      ("o" evil-open-fold)
      ("O" evil-open-fold-rec)
      ("r" evil-open-folds)
      ("m" evil-close-folds)
      ("q" nil :exit t)
      ("C-g" nil :exit t)
      ("<SPC>" nil :exit t)))
  (my/set-leader-keys "z." 'my/fold-transient-state/body)

  ;; define text objects
  (my|define-text-object "$" "dollar" "$" "$")
  (my|define-text-object "*" "star" "*" "*")
  (my|define-text-object "8" "block-star" "/*" "*/")
  (my|define-text-object "|" "bar" "|" "|")
  (my|define-text-object "%" "percent" "%" "%")
  (my|define-text-object "/" "slash" "/" "/")
  (my|define-text-object "_" "underscore" "_" "_")
  (my|define-text-object "-" "hyphen" "-" "-")
  (my|define-text-object "~" "tilde" "~" "~")
  (my|define-text-object "=" "equal" "=" "=")
  (my|define-text-object "«" "double-angle-bracket" "«" "»")
  (my|define-text-object "｢" "corner-bracket" "｢" "｣")
  (my|define-text-object "‘" "single-quotation-mark" "‘" "’")
  (my|define-text-object "“" "double-quotation-mark" "“" "”")
  (evil-define-text-object evil-pasted (count &rest args)
    (list (save-excursion (evil-goto-mark ?\[) (point))
          (save-excursion (evil-goto-mark ?\]) (point))))
  (define-key evil-inner-text-objects-map "P" 'evil-pasted)
  ;; define text-object for entire buffer
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

  ;; turn off evil in corelv buffers
  (push '("\\*LV\\*") evil-buffer-regexps))

(provide 'core-evil)
