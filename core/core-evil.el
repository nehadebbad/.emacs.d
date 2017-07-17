
(defvar dotspacemacs-leader-key "SPC"
  "The leader key.")

(defvar dotspacemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotspacemacs-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-ex-command-key ":"
  "The key used for Vim Ex commands.")

(defvar dotspacemacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

(defvaralias 'dotspacemacs-emacs-command-key 'dotspacemacs-command-key
  "New official name for `dotspacemacs-command-key'")

(defvar dotspacemacs-editing-style 'vim
  "One of `vim', `emacs' or `hybrid'.
`hybrid' is like `vim' except that `insert state' is replaced by the
`hybrid state' with `emacs' key bindings. The value can also be a list
 with `:variables' keyword (similar to layers). Check the editing styles
 section of the documentation for details on available variables.")

(defvar dotspacemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

(defvar dotspacemacs-ex-substitute-global nil
  "If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.")

(defvar dotspacemacs-remap-Y-to-y$ nil
  "If non nil `Y' is remapped to `y$' in Evil states.")

(defvar dotspacemacs-retain-visual-state-on-shift t
  "If non-nil, the shift mappings `<' and `>' retain visual state
if used there.")

(defvar dotspacemacs-visual-line-move-text nil
  "If non-nil, J and K move lines up and down when in visual mode.")

(defvar dotspacemacs-which-key-position 'bottom
  "Location of the which-key popup buffer. Possible choices are bottom,
right, and right-then-bottom. The last one will display on the
right if possible and fallback to bottom if not.")

(defvar dotspacemacs-which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence. Setting this variable is equivalent to setting
`which-key-idle-delay'.")
;; Configure bind map
(require 'bind-map)
(bind-map spacemacs-default-map
    :prefix-cmd spacemacs-cmds
    :keys (dotspacemacs-emacs-leader-key)
    :evil-keys (dotspacemacs-leader-key)
    :override-minor-modes t
    :override-mode-name spacemacs-leader-override-mode)

;; Configure evil
(defun spacemacs/set-evil-search-module (style)
  "Set the evil search module depending on STYLE."
  (cond
   ((eq 'vim style)
    ;; if Evil is loaded already, just setting `evil-search-module' isn't
    ;; enough, we need to call `evil-select-search-module' as well (this is done
    ;; automatically when `evil-search-module' is changed via customize)
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'evil-search)
      (setq-default evil-search-module 'evil-search)))
   (t
    (if (featurep 'evil-search)
        (evil-select-search-module 'evil-search-module 'isearch)
      (setq-default evil-search-module 'isearch)))))

(spacemacs/set-evil-search-module dotspacemacs-editing-style)
(require 'evil)
(evil-mode 1)

;; Use evil as a default jump handler
;; TODO configure this at the time of implementing core-jump.el
;;(push 'evil-goto-definition spacemacs-default-jump-handlers)

(require 'cl)

(defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                   ("insert" "chartreuse3" (bar . 2))
                                   ("emacs" "SkyBlue2" box)
                                   ("hybrid" "SkyBlue2" (bar . 2))
                                   ("replace" "chocolate" (hbar . 2))
                                   ("evilified" "LightGoldenrod3" box)
                                   ("visual" "gray" (hbar . 2))
                                   ("motion" "plum3" box)
                                   ("lisp" "HotPink1" box)
                                   ("iedit" "firebrick1" box)
                                   ("iedit-insert" "firebrick1" (bar . 2)))
    "Colors assigned to evil states with cursor definitions.")

(cl-loop for (state color cursor) in spacemacs-evil-cursors
           do
           (eval `(defface ,(intern (format "spacemacs-%s-face" state))
                    `((t (:background ,color
                                      :foreground ,(face-background 'mode-line)
                                      :inherit 'mode-line)))
                    (format "%s state face." state)
                    :group 'spacemacs))
           (set (intern (format "evil-%s-state-cursor" state))
                (list (when dotspacemacs-colorize-cursor-according-to-state color)
                      cursor)))

(defun spacemacs/set-state-faces ()
  (cl-loop for (state color cursor) in spacemacs-evil-cursors
           do
           (set-face-attribute (intern (format "spacemacs-%s-face" state))
                               nil
                               :foreground (face-background 'mode-line))))
(spacemacs/set-state-faces)

(define-key evil-normal-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
(define-key evil-visual-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
(define-key evil-motion-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
(setq evil-ex-substitute-global dotspacemacs-ex-substitute-global)

;; evil-want-Y-yank-to-eol must be set via customize to have an effect
(customize-set-variable 'evil-want-Y-yank-to-eol dotspacemacs-remap-Y-to-y$)
; bind evil-jump-forward for GUI only.
(define-key evil-motion-state-map [C-i] 'evil-jump-forward)
; Make the current definition and/or comment visible.
(define-key evil-normal-state-map "zf" 'reposition-window)
;; toggle maximize buffer
(define-key evil-window-map (kbd "o") 'spacemacs/toggle-maximize-buffer)
(define-key evil-window-map (kbd "C-o") 'spacemacs/toggle-maximize-buffer)
;; make cursor keys work
(define-key evil-window-map (kbd "<left>") 'evil-window-left)
(define-key evil-window-map (kbd "<right>") 'evil-window-right)
(define-key evil-window-map (kbd "<up>") 'evil-window-up)
(define-key evil-window-map (kbd "<down>") 'evil-window-down)
(spacemacs/set-leader-keys "re" 'evil-show-registers)
;; motions keys for help buffers
(evil-define-key 'motion help-mode-map (kbd "<escape>") 'quit-window)
(evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
(evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
(evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
(evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
(evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)
;; It's better that the default value is too small than too big
(setq-default evil-shift-width 2)
(defvar spacemacs--indent-variable-alist
  ;; Note that derived modes must come before their sources
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
      idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (rust-mode . rust-indent-offset)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (typescript-mode . typescript-indent-level)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")
;; After major mode has changed, reset evil-shift-width
(defun spacemacs//set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test spacemacs--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))
(add-hook 'after-change-major-mode-hook 'spacemacs//set-evil-shift-width 'append)

;; Keep the region active when shifting
(when dotspacemacs-retain-visual-state-on-shift
  (evil-map visual "<" "<gv")
  (evil-map visual ">" ">gv"))
; move selection up and down
(when dotspacemacs-visual-line-move-text
  (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

;; TODO write function definitions in other files
(evil-ex-define-cmd "enew" 'spacemacs/new-empty-buffer)
(define-key evil-normal-state-map (kbd "K") 'spacemacs/evil-smart-doc-lookup)
(define-key evil-normal-state-map (kbd "gd") 'spacemacs/jump-to-definition)
(define-key evil-normal-state-map (kbd "gD") 'spacemacs/jump-to-definition-other-window)

;; TODO copy core-transient-state.el for macros
;; scrolling transient state
(spacemacs|define-transient-state scroll
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

;; TODO copy core-keybindings.el
(spacemacs/set-leader-keys
 ;; buffer
 "N<" 'spacemacs/scroll-transient-state/evil-goto-first-line
 "N>" 'spacemacs/scroll-transient-state/evil-goto-line
 ;; full page
 "Nf" 'spacemacs/scroll-transient-state/evil-scroll-page-down
 "Nb" 'spacemacs/scroll-transient-state/evil-scroll-page-up
 ;; half page
 "Nd" 'spacemacs/scroll-transient-state/evil-scroll-down
 "Nu" 'spacemacs/scroll-transient-state/evil-scroll-up
 "NJ" 'spacemacs/scroll-transient-state/evil-scroll-down
 "NK" 'spacemacs/scroll-transient-state/evil-scroll-up
 "NH" 'spacemacs/scroll-transient-state/evil-scroll-left
 "NL" 'spacemacs/scroll-transient-state/evil-scroll-right
 ;; lines and columns
 "Nj" 'spacemacs/scroll-transient-state/evil-scroll-line-down
 "Nk" 'spacemacs/scroll-transient-state/evil-scroll-line-up
 "Nh" 'spacemacs/scroll-transient-state/evil-scroll-column-left
 "Nl" 'spacemacs/scroll-transient-state/evil-scroll-column-right)

;; pasting transient-state
(evil-define-command spacemacs//transient-state-0 ()
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

(spacemacs|define-transient-state paste
    :title "Pasting Transient State"
    :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
    :bindings
    ("C-j" evil-paste-pop)
    ("C-k" evil-paste-pop-next)
    ("p" evil-paste-after)
    ("P" evil-paste-before)
    ("0" spacemacs//transient-state-0))

(defvar dotspacemacs-enable-paste-transient-state nil
  "If non nil the paste transient-state is enabled. While enabled pressing `p`
several times cycle between the kill ring content.'")

(when dotspacemacs-enable-paste-transient-state
    (define-key evil-normal-state-map
      "p" 'spacemacs/paste-transient-state/evil-paste-after)
    (define-key evil-normal-state-map
      "P" 'spacemacs/paste-transient-state/evil-paste-before))

(defvar dotspacemacs-folding-method 'evil
  "Code folding method. Possible values are `evil' and `origami'.")

;; fold transient state
(when (eq 'evil dotspacemacs-folding-method)
   (spacemacs|define-transient-state fold
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
(spacemacs/set-leader-keys "z." 'spacemacs/fold-transient-state/body)

;; TODO define the following macro
;; define text objects
(spacemacs|define-text-object "$" "dollar" "$" "$")
(spacemacs|define-text-object "*" "star" "*" "*")
(spacemacs|define-text-object "8" "block-star" "/*" "*/")
(spacemacs|define-text-object "|" "bar" "|" "|")
(spacemacs|define-text-object "%" "percent" "%" "%")
(spacemacs|define-text-object "/" "slash" "/" "/")
(spacemacs|define-text-object "_" "underscore" "_" "_")
(spacemacs|define-text-object "-" "hyphen" "-" "-")
(spacemacs|define-text-object "~" "tilde" "~" "~")
(spacemacs|define-text-object "=" "equal" "=" "=")
(spacemacs|define-text-object "«" "double-angle-bracket" "«" "»")
(spacemacs|define-text-object "｢" "corner-bracket" "｢" "｣")
(spacemacs|define-text-object "‘" "single-quotation-mark" "‘" "’")
(spacemacs|define-text-object "“" "double-quotation-mark" "“" "”")
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
  (evil-define-key 'normal dired-mode-map "J" 'spacemacs/helm-find-files)
  (define-key dired-mode-map "j" 'spacemacs/helm-find-files)
  (evil-define-key 'normal dired-mode-map (kbd dotspacemacs-leader-key)
    spacemacs-default-map))

;; TODO replace with own implemenatation
;; support smart 1parens-strict-mode
(when (configuration-layer/package-used-p 'smartparens)
  (defadvice evil-delete-backward-char-and-join
      (around spacemacs/evil-delete-backward-char-and-join activate)
    (if (bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char)
      ad-do-it)))

;; Define history commands for comint
(when (eq dotspacemacs-editing-style 'vim)
  (evil-define-key 'insert comint-mode-map
    (kbd "C-k") 'comint-previous-input
    (kbd "C-j") 'comint-next-input))
(evil-define-key 'normal comint-mode-map
  (kbd "C-k") 'comint-previous-input
  (kbd "C-j") 'comint-next-input)

;; ignore repeat
(evil-declare-ignore-repeat 'spacemacs/next-error)
(evil-declare-ignore-repeat 'spacemacs/previous-error)

;; evil mode config ends here

;; hydra config
(require 'hydra)
(setq hydra-key-doc-function 'spacemacs//hydra-key-doc-function
      hydra-head-format "[%s] ")
;; hydra config ends here

;;use-package config
(require 'use-package)
(setq use-package-verbose init-file-debug
      ;; inject use-package hooks for easy customization of stock package
      ;; configuration
      use-package-inject-hooks t)
;; use-package config ends here

;; which-key config
(defun spacemacs-bootstrap/init-which-key ()
  (require 'which-key)

  ;; TODO define macro
  (spacemacs|add-toggle which-key
    :mode which-key-mode
    :documentation
    "Display a buffer with available key bindings."
    :evil-leader "tK")

  (spacemacs/set-leader-keys "hk" 'which-key-show-top-level)

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

  (dolist (leader-key `(,dotspacemacs-leader-key ,dotspacemacs-emacs-leader-key))
    (which-key-add-key-based-replacements
      (concat leader-key " m")    "major mode commands"
      (concat leader-key " " dotspacemacs-emacs-command-key) "M-x"))

  (which-key-add-key-based-replacements
    dotspacemacs-leader-key '("root" . "Spacemacs root")
    dotspacemacs-emacs-leader-key '("root" . "Spacemacs root")
    (concat dotspacemacs-leader-key " m")
    '("major-mode-cmd" . "Major mode commands")
    (concat dotspacemacs-emacs-leader-key " m")
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

  (which-key-mode))
