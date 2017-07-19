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
  (push '("\\*LV\\*") evil-buffer-regexps)

  ;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
  ;; can do the same thing and with fuzzy matching and other features.
  (with-eval-after-load 'dired
    ;; TODO define this function when configuring helm
    (evil-define-key 'normal dired-mode-map "J" 'my/helm-find-files)
    (define-key dired-mode-map "j" 'my/helm-find-files)
    (evil-define-key 'normal dired-mode-map (kbd dotspacemacs-leader-key)
      my-default-map))

  ;; Define history commands for comint
  (when (eq dotspacemacs-editing-style 'vim)
    (evil-define-key 'insert comint-mode-map
      (kbd "C-k") 'comint-previous-input
      (kbd "C-j") 'comint-next-input))
  (evil-define-key 'normal comint-mode-map
    (kbd "C-k") 'comint-previous-input
    (kbd "C-j") 'comint-next-input)

  ;; ignore repeat
  (evil-declare-ignore-repeat 'my/next-error)
  (evil-declare-ignore-repeat 'my/previous-error)
  (message "evil loaded"))

(use-package hydra
  :demand t
  :config
  (setq hydra-key-doc-function 'my//hydra-key-doc-function
        hydra-head-format "[%s] ")
  (message "hydra loaded"))

(use-package which-key
  :demand t
  :config
  (my|add-toggle which-key
    :mode which-key-mode
    :documentation
    "Display a buffer with available key bindings."
    :evil-leader "tK")

  (my/set-leader-keys "hk" 'which-key-show-top-level)

  ;; Needed to avoid nil variable error before update to recent which-key
  (defvar which-key-replacement-alist nil)
  ;; Replace rules for better naming of functions
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(
           ("spacemacs/\\(.+\\)" . "\\1")
           ("spacemacs/toggle-\\(.+\\)" . "\\1")
           ("spacemacs/alternate-buffer" . "last buffer")
           ("spacemacs/toggle-mode-line-\\(.+\\)" . "\\1")
           ("avy-goto-word-or-subword-1" . "avy word")
           ("shell-command" . "shell cmd")
           ("spacemacs/default-pop-shell" . "open shell")
           ("spacemacs/helm-project-smart-do-search-region-or-symbol" . "smart search w/input")
           ("spacemacs/helm-project-smart-do-search" . "smart search")
           ("spacemacs/search-project-auto-region-or-symbol" . "search project w/input")
           ("spacemacs/search-project-auto" . "search project")
           ("helm-descbinds" . "show keybindings")
           ("sp-split-sexp" . "split sexp")
           ("avy-goto-line" . "avy line")
           ("universal-argument" . "universal arg")
           ("er/expand-region" . "expand region")
           ("helm-apropos" . "apropos")
           ("spacemacs/toggle-hybrid-mode" . "hybrid (hybrid-mode)")
           ("spacemacs/toggle-holy-mode" . "emacs (holy-mode)")
           ("evil-lisp-state-\\(.+\\)" . "\\1")
           ("spacemacs/\\(.+\\)-transient-state/\\(.+\\)" . "\\2")
           ("spacemacs/\\(.+\\)-transient-state/body" . "\\1-transient-state")
           ("helm-mini\\|ivy-switch-buffer" . "list-buffers")
           ("spacemacs-layouts/non-restricted-buffer-list-\\(helm\\|ivy\\)" . "global-list-buffers"))))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
            which-key-replacement-alist)))

  ;; Group together sequence and identical key entries in the which-key popup
  ;; SPC h k- Top-level bindings
  ;; Remove spaces around the two dots ".."
  (push '(("\\(.*\\)1 .. 9" . "digit-argument") .
          ("\\11..9" . "digit-argument"))
        which-key-replacement-alist)

  ;; And remove the modifier key(s) before the last nr in the sequence
  (push '(("\\(.*\\)C-0 .. C-5" . "digit-argument") .
          ("\\1C-0..5" . "digit-argument"))
        which-key-replacement-alist)

  (push '(("\\(.*\\)C-7 .. C-9" . "digit-argument") .
          ("\\1C-7..9" . "digit-argument"))
        which-key-replacement-alist)

  (push '(("\\(.*\\)C-M-0 .. C-M-9" . "digit-argument") .
          ("\\1C-M-0..9" . "digit-argument"))
        which-key-replacement-alist)

  ;; Rename the entry for M-0 in the SPC h k Top-level bindings,
  ;; and for 0 in the SPC- Spacemacs root
  (push '(("\\(.*\\)0" . "winum-select-window-0-or-10") .
          ("\\10" . "select window 0 or 10"))
        which-key-replacement-alist)

  ;; Rename the entry for M-1 in the SPC h k Top-level bindings,
  ;; and for 1 in the SPC- Spacemacs root, to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") .
          ("\\11..9" . "select window 1..9"))
        which-key-replacement-alist)

  ;; Hide the entries for M-[2-9] in the SPC h k Top-level bindings,
  ;; and for [2-9] in the SPC- Spacemacs root
  (push '((nil . "winum-select-window-[2-9]") . t)
        which-key-replacement-alist)

  ;; SPC- Spacemacs root
  ;; Combine the ` (backtick) and ² (superscript 2) key entries
  (push '(("\\(.*\\)`" . "winum-select-window-by-number") .
          ("\\1`,²" . "select window by number"))
        which-key-replacement-alist)

  ;; hide the "² -> winum-select-window-by-number" entry
  (push '(("\\(.*\\)²" . nil) . t)
        which-key-replacement-alist)

  ;; SPC b- buffers
  ;; rename the buffer-to-window-1 entry, to 1..9
  (push '(("\\(.*\\)1" . "buffer-to-window-1") .
          ("\\11..9" . "buffer to window 1..9"))
        which-key-replacement-alist)

  ;; hide the "[2-9] -> buffer-to-window-[2-9]" entries
  (push '((nil . "buffer-to-window-[2-9]") . t)
        which-key-replacement-alist)

  ;; SPC k- lisp
  ;; rename "1 .. 9 -> digit-argument" to "1..9 -> digit-argument"
  (push '(("\\(.*\\)1 .. 9" . "evil-lisp-state-digit-argument") .
          ("\\11..9" . "digit-argument"))
        which-key-replacement-alist)

  ;; SPC x i- inflection
  ;; rename "k -> string-inflection-kebab-case"
  ;; to "k,- -> string-inflection-kebab-case"
  (push '(("\\(.*\\)k" . "string-inflection-kebab-case") .
          ("\\1k,-" . "string-inflection-kebab-case"))
        which-key-replacement-alist)

  ;; hide the "- -> string-inflection-kebab-case" entry
  (push '(("\\(.*\\)-" . "string-inflection-kebab-case") . t)
        which-key-replacement-alist)

  ;; rename "u -> string-inflection-underscore"
  ;; to "u,_ -> string-inflection-underscore"
  (push '(("\\(.*\\)u" . "string-inflection-underscore") .
          ("\\1u,_" . "string-inflection-underscore"))
        which-key-replacement-alist)

  ;; hide the "_ -> string-inflection-underscore" entry
  (push '(("\\(.*\\)_" . "string-inflection-underscore") . t)
        which-key-replacement-alist)

  ;; C-c C-w-
  ;; rename the eyebrowse-switch-to-window-config-0 entry, to 0..9
  (push '(("\\(.*\\)0" . "eyebrowse-switch-to-window-config-0") .
          ("\\10..9" . "eyebrowse-switch-to-window-config-0..9"))
        which-key-replacement-alist)

  ;; hide the "[1-9] -> eyebrowse-switch-to-window-config-[1-9]" entries
  (push '((nil . "eyebrowse-switch-to-window-config-[1-9]") . t)
        which-key-replacement-alist)

  ;; Combine the c and C-c key entries
  (push '(("\\(.*\\)C-c C-w c" . "eyebrowse-create-window-config") .
          ("\\1c,C-c" . "eyebrowse-create-window-config"))
        which-key-replacement-alist)

  ;; hide the "C-c -> eyebrowse-create-window-config" entry
  (push '(("\\(.*\\)C-c C-w C-c" . "eyebrowse-create-window-config") . t)
          which-key-replacement-alist)

  ;; C-c C-d-
  ;; Combine the d and C-d key entries
  (push '(("\\(.*\\)C-c C-d d" . "elisp-slime-nav-describe-elisp-thing-at-point") .
          ("\\1d,C-d" . "elisp-slime-nav-describe-elisp-thing-at-point"))
        which-key-replacement-alist)

  ;; hide the "C-d -> elisp-slime-nav-describe-elisp-thing-at-point" entry
  (push '(("\\(.*\\)C-c C-d C-d" . "elisp-slime-nav-describe-elisp-thing-at-point") . t)
        which-key-replacement-alist)

  (dolist (leader-key `(,dotspacemacs-leader-key))
    (which-key-add-key-based-replacements
      (concat leader-key " m")    "major mode commands"))

  (which-key-add-key-based-replacements
    dotspacemacs-leader-key '("root" . "Spacemacs root")
    (concat dotspacemacs-leader-key " m")
    '("major-mode-cmd" . "Major mode commands"))

  ;; disable special key handling for spacemacs, since it can be
  ;; disorienting if you don't understand it
  (pcase dotspacemacs-which-key-position
    (`right (which-key-setup-side-window-right))
    (`bottom (which-key-setup-side-window-bottom))
    (`right-then-bottom (which-key-setup-side-window-right-bottom)))

  (setq which-key-special-keys nil
        which-key-use-C-h-for-paging t
        which-key-prevent-C-h-from-cycling t
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay dotspacemacs-which-key-delay
        which-key-allow-evil-operators t)

  (which-key-mode)
  (message "which-key loaded"))
(provide 'core-evil)
