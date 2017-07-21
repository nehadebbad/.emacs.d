;; Prefixes

;; We define prefix commands only for the sake of which-key
(setq my/key-binding-prefixes '(("a"   "applications")
                                ("A"   "other applications")
                                ("ai"  "irc")
                                ("as"  "shells")
                                ("ay"  "ipython notebook")
                                ("b"   "buffers")
                                ("c"   "compile/comments")
                                ("C"   "capture/colors")
                                ("e"   "errors")
                                ("f"   "files")
                                ("fC"  "files/convert")
                                ("fe"  "emacs(spacemacs)")
                                ("fv"  "variables")
                                ("F"   "frame")
                                ("g"   "git/versions-control")
                                ("h"   "help")
                                ("hd"  "help-describe")
                                ("i"   "insertion")
                                ("j"   "jump/join/split")
                                ("k"   "lisp")
                                ("kd"  "delete")
                                ("kD"  "delete-backward")
                                ("k`"  "hybrid")
                                ("n"   "narrow/numbers")
                                ("N"   "navigation")
                                ("p"   "projects")
                                ("p$"  "projects/shell")
                                ("q"   "quit")
                                ("r"   "registers/rings/resume")
                                ("Re"  "elisp")
                                ("Rp"  "pcre")
                                ("s"   "search/symbol")
                                ("sa"  "ag")
                                ("sg"  "grep")
                                ("sk"  "ack")
                                ("sr"  "ripgrep")
                                ("st"  "pt")
                                ("sw"  "web")
                                ("t"   "toggles")
                                ("tC"  "colors")
                                ("tE"  "editing-styles")
                                ("th"  "highlight")
                                ("tm"  "modeline")
                                ("T"   "UI toggles/themes")
                                ("C-t" "other toggles")
                                ("w"   "windows")
                                ("wp"  "popup")
                                ("x"   "text")
                                ("xa"  "align")
                                ("xd"  "delete")
                                ("xg"  "google-translate")
                                ("xl"  "lines")
                                ("xm"  "move")
                                ("xt"  "transpose")
                                ("xw"  "words")
                                ("z"   "zoom")))

(mapc (lambda (x) (apply #'my/declare-prefix x))
      my/key-binding-prefixes)

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'my/toggle-maximize-buffer)

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; linum margin bindings-------------------------------------------------------
(global-set-key (kbd "<left-margin> <down-mouse-1>") 'my/md-select-linum)
(global-set-key (kbd "<left-margin> <mouse-1>") 'my/mu-select-linum)
(global-set-key (kbd "<left-margin> <double-mouse-1>") 'my/select-current-block)
(global-set-key (kbd "<left-margin> <drag-mouse-1>") 'my/mu-select-linum)

;; Leader keys

;; Universal argument ---------------------------------------------------------
(my/set-leader-keys "u" 'universal-argument)
(define-key universal-argument-map
  (kbd (concat dotspacemacs-leader-key " u"))
  'universal-argument-more)

;; shell command  -------------------------------------------------------------
(my/set-leader-keys "!" 'shell-command)
;; applications ---------------------------------------------------------------
(my/set-leader-keys
  "ac"  'calc-dispatch
  "ap"  'list-processes
  "aP"  'proced
  "au"  'undo-tree-visualize)
;; buffers --------------------------------------------------------------------
(my/set-leader-keys
  "TAB"   'my/alternate-buffer
  "bd"    'my/kill-this-buffer
  "be"    'my/safe-erase-buffer
  "bh"    'my/home
  "b C-d" 'my/kill-other-buffers
  "b C-S-d" 'my/kill-matching-buffers-rudely
  "bn"    'next-buffer
  "bm"    'my/switch-to-messages-buffer
  "b N h" 'my/new-empty-buffer-left
  "b N j" 'my/new-empty-buffer-below
  "b N k" 'my/new-empty-buffer-above
  "b N l" 'my/new-empty-buffer-right
  "b N n" 'my/new-empty-buffer
  "bP"    'my/copy-clipboard-to-whole-buffer
  "bp"    'previous-buffer
  "bR"    'my/safe-revert-buffer
  "bs"    'my/switch-to-scratch-buffer
  "bu"    'my/reopen-killed-buffer
  "bY"    'my/copy-whole-buffer-to-clipboard
  "bw"    'read-only-mode)

(dotimes (i 9)
  (let ((n (+ i 1)))
    (my/set-leader-keys (format "b%i" n)
                               (intern (format "buffer-to-window-%s" n)))))

;; errors ---------------------------------------------------------------------
(my/set-leader-keys
 "en" 'my/next-error
 "eN" 'my/previous-error
 "ep" 'my/previous-error)

(my|define-transient-state error
  :title "Error transient state"
  :hint-is-doc t
  :dynamic-hint
  (let ((sys (my/error-delegate)))
    (cond
     ((eq 'flycheck sys)
      "\nBrowsing flycheck errors from this buffer.")
     ((eq 'emacs sys)
      (let ((buf (next-error-find-buffer)))
        (if buf
            (concat "\nBrowsing entries from \""
                    (buffer-name buf)
                    "\""
                    (with-current-buffer buf
                      (when my--gne-line-func
                        (format " (%d of %d)"
                                (max 1 (1+ (- my--gne-cur-line
                                              my--gne-min-line)))
                                (1+ (- my--gne-max-line
                                       my--gne-min-line))))))
          "\nNo next-error capable buffer found.")))))
  :bindings
  ("n" my/next-error "next")
  ("p" my/previous-error "prev")
  ("N" my/previous-error "prev")
  ("q" nil "quit" :exit t)
  :evil-leader "e.")

;; file -----------------------------------------------------------------------
(my/set-leader-keys
  "fc" 'my/copy-file
  "fD" 'my/delete-current-buffer-file
  "fei" 'my/find-user-init-file
  "fCd" 'my/unix2dos
  "fCu" 'my/dos2unix
  "fg" 'rgrep
  "fl" 'find-file-literally
  "fE" 'my/sudo-edit
  "fo" 'my/open-file-or-directory-in-external-app
  "fR" 'my/rename-current-buffer-file
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fvd" 'add-dir-local-variable
  "fvf" 'add-file-local-variable
  "fvp" 'add-file-local-variable-prop-line
  "fy" 'my/show-and-copy-buffer-filename)
;; frame ----------------------------------------------------------------------
(my/set-leader-keys
  "Ff" 'find-file-other-frame
  "Fc" 'delete-frame
  "FC" 'delete-other-frames
  "Fb" 'switch-to-buffer-other-frame
  "FB" 'display-buffer-other-frame
  "Fd" 'dired-other-frame
  "Fo" 'other-frame
  "Fn" 'make-frame)
;; help -----------------------------------------------------------------------
(my/set-leader-keys
  "hdb" 'describe-bindings
  "hdc" 'describe-char
  "hdf" 'describe-function
  "hdk" 'describe-key
  "hdp" 'describe-package
  "hdt" 'describe-theme
  "hdv" 'describe-variable
  "hn"  'view-emacs-news)
;; insert stuff ---------------------------------------------------------------
(my/set-leader-keys
  "iJ" 'my/insert-line-below-no-indent
  "iK" 'my/insert-line-above-no-indent
  "ik" 'my/evil-insert-line-above
  "ij" 'my/evil-insert-line-below)
;; format ---------------------------------------------------------------------
(my/set-leader-keys
  "jo" 'open-line
  "j=" 'my/indent-region-or-buffer
  "jS" 'my/split-and-new-line
  "jk" 'my/evil-goto-next-line-and-indent)

;; navigation/jumping ---------------------------------------------------------
(my/set-leader-keys
  "j0" 'my/push-mark-and-goto-beginning-of-line
  "j$" 'my/push-mark-and-goto-end-of-line
  "jf" 'find-function
  "jv" 'find-variable)

;; Compilation ----------------------------------------------------------------
(my/set-leader-keys
  "cC" 'compile
  "ck" 'kill-compilation
  "cr" 'recompile
  "cd" 'my/close-compilation-window)
(with-eval-after-load 'compile
  (evil-define-key 'motion compilation-mode-map (kbd "gf") 'find-file-at-point)
  (define-key compilation-mode-map "r" 'recompile)
  (define-key compilation-mode-map "g" nil))
;; narrow & widen -------------------------------------------------------------
(my/set-leader-keys
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)

;; toggle ---------------------------------------------------------------------
(my|add-toggle highlight-current-line-globally
  :mode global-hl-line-mode
  :documentation "Globally highlight the current line."
  :evil-leader "thh")
(my|add-toggle truncate-lines
  :status truncate-lines
  :on (toggle-truncate-lines)
  :off (toggle-truncate-lines -1)
  :documentation "Truncate long lines (no wrap)."
  :evil-leader "tl")
(my|add-toggle visual-line-navigation
  :status visual-line-mode
  :on
  (progn
    (visual-line-mode)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
    (when (bound-and-true-p evil-escape-mode)
      (evil-escape-mode -1)
      (setq evil-escape-motion-state-shadowed-func nil)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
      (evil-escape-mode))
    (evil-normalize-keymaps))
  :off
  (progn
    (visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")

(my|add-toggle auto-fill-mode
  :status auto-fill-function
  :on (auto-fill-mode)
  :off (auto-fill-mode -1)
  :documentation "Break line beyond `current-fill-column` while editing."
  :evil-leader "tF")
(my|add-toggle debug-on-error
  :status debug-on-error
  :on (setq debug-on-error t)
  :off (setq debug-on-error nil)
  :documentation "Toggle display of backtrace when an error happens."
  :evil-leader "tD")
(my|add-toggle fringe
  :if (fboundp 'fringe-mode)
  :status (not (equal fringe-mode 0))
  :on (call-interactively 'fringe-mode)
  :off (fringe-mode 0)
  :documentation "Display the fringe in GUI mode."
  :evil-leader "Tf")
(my|add-toggle fullscreen-frame
  :status (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
  :on (my/toggle-frame-fullscreen)
  :off (my/toggle-frame-fullscreen)
  :documentation "Display the current frame in full screen."
  :evil-leader "TF")
(my|add-toggle maximize-frame
  :status (eq (frame-parameter nil 'fullscreen) 'maximized)
  :on (toggle-frame-maximized)
  :off (toggle-frame-maximized)
  :documentation "Maximize the current frame."
  :evil-leader "TM")
(my|add-toggle mode-line
  :status (not hidden-mode-line-mode)
  :on (hidden-mode-line-mode -1)
  :off (hidden-mode-line-mode)
  :documentation "Toggle the visibility of modeline."
  :evil-leader "tmT")
(my|add-toggle display-time
  :mode display-time-mode
  :documentation "Display time in modeline."
  :evil-leader "tmt")
(my|add-toggle syntax-highlighting
  :mode font-lock-mode
  :documentation "Toggle syntax highlighting."
  :evil-leader "ths")
(my|add-toggle transparent-frame
  :status nil
  :on (my/toggle-transparency)
  :documentation "Make the current frame non-opaque."
  :evil-leader "TT")
(my|add-toggle tool-bar
  :if window-system
  :mode tool-bar-mode
  :documentation "Display the tool bar in GUI mode."
  :evil-leader "Tt")
(my|add-toggle menu-bar
  :if window-system
  :mode menu-bar-mode
  :documentation "Display the menu bar."
  :evil-leader "Tm")
(my|add-toggle semantic-stickyfunc
  :mode semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc."
  :evil-leader "TS")
(my|add-toggle semantic-stickyfunc-globally
  :mode global-semantic-stickyfunc-mode
  :documentation "Enable semantic-stickyfunc globally."
  :evil-leader "T C-S")

;; quit -----------------------------------------------------------------------
(my/set-leader-keys
  "qs" 'my/save-buffers-kill-emacs
  "qq" 'my/prompt-kill-emacs
  "qQ" 'my/kill-emacs
  "qf" 'my/frame-killer)
;; window ---------------------------------------------------------------------
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(my/set-leader-keys
  "w TAB"  'my/alternate-window
  "w2"  'my/layout-double-columns
  "w3"  'my/layout-triple-columns
  "wb"  'my/switch-to-minibuffer-window
  "wd"  'my/delete-window
  "wt"  'my/toggle-current-window-dedication
  "wf"  'follow-mode
  "wF"  'make-frame
  "wH"  'evil-window-move-far-left
  "w <S-left>"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "w <left>"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "w <S-down>"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "w <down>"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "w <S-up>"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "w <up>"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "w <S-right>"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "w <right>"  'evil-window-right
  "wm"  'my/toggle-maximize-buffer
  ;"wc"  'my/toggle-centered-buffer-mode ; TODO centered buffer mode
  ;"wC"  'my/toggle-centered-buffer-mode-frame
  "wo"  'other-frame
  "wr"  'my/rotate-windows-forward
  "wR"  'my/rotate-windows-backward
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "ww"  'other-window
  "w/"  'split-window-right
  "w="  'balance-windows
  "w+"  'my/window-layout-toggle
  "w_"  'my/maximize-horizontally)
;; text -----------------------------------------------------------------------
(defalias 'count-region 'count-words-region)

(my/set-leader-keys
  "xa&" 'my/align-repeat-ampersand
  "xa(" 'my/align-repeat-left-paren
  "xa)" 'my/align-repeat-right-paren
  "xa{" 'my/align-repeat-left-curly-brace
  "xa}" 'my/align-repeat-right-curly-brace
  "xa[" 'my/align-repeat-left-square-brace
  "xa]" 'my/align-repeat-right-square-brace
  "xa," 'my/align-repeat-comma
  "xa." 'my/align-repeat-decimal
  "xa:" 'my/align-repeat-colon
  "xa;" 'my/align-repeat-semicolon
  "xa=" 'my/align-repeat-equal
  "xa\\" 'my/align-repeat-backslash
  "xaa" 'align
  "xac" 'align-current
  "xam" 'my/align-repeat-math-oper
  "xar" 'my/align-repeat
  "xa|" 'my/align-repeat-bar
  "xc"  'count-region
  "xd SPC" 'just-one-space
  "xdw" 'delete-trailing-whitespace
  "xjc" 'set-justification-center
  "xjf" 'set-justification-full
  "xjl" 'set-justification-left
  "xjn" 'set-justification-none
  "xjr" 'set-justification-right
  "xlc" 'my/sort-lines-by-column
  "xlC" 'my/sort-lines-by-column-reverse
  "xld" 'my/duplicate-line-or-region
  "xls" 'my/sort-lines
  "xlS" 'my/sort-lines-reverse
  "xlu" 'my/uniquify-lines
  "xtc" 'transpose-chars
  "xtl" 'transpose-lines
  "xtw" 'transpose-words
  "xU"  'upcase-region
  "xu"  'downcase-region
  "xwc" 'my/count-words-analysis
  "x TAB" 'indent-rigidly)

(define-key indent-rigidly-map "h" 'indent-rigidly-left)
(define-key indent-rigidly-map "l" 'indent-rigidly-right)
(define-key indent-rigidly-map "H" 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map "L" 'indent-rigidly-right-to-tab-stop)

;; shell ----------------------------------------------------------------------
(with-eval-after-load 'shell
  (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
  (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

;; ---------------------------------------------------------------------------
;; Transient-states
;; ---------------------------------------------------------------------------

;; Buffer transient state

(my|define-transient-state buffer
  :title "Buffer Selection Transient State"
  :doc (concat "
 [_C-1_.._C-9_] goto nth window            [_n_]^^   next buffer
 [_1_.._9_]     move buffer to nth window  [_N_/_p_] previous buffer
 [_M-1_.._M-9_] swap buffer w/ nth window  [_d_]^^   kill buffer
 ^^^^                                      [_q_]^^   quit")
  :bindings
  ("n" next-buffer)
  ("N" previous-buffer)
  ("p" previous-buffer)
  ("d" my/kill-this-buffer)
  ("q" nil :exit t)
  ("1" move-buffer-window-no-follow-1)
  ("2" move-buffer-window-no-follow-2)
  ("3" move-buffer-window-no-follow-3)
  ("4" move-buffer-window-no-follow-4)
  ("5" move-buffer-window-no-follow-5)
  ("6" move-buffer-window-no-follow-6)
  ("7" move-buffer-window-no-follow-7)
  ("8" move-buffer-window-no-follow-8)
  ("9" move-buffer-window-no-follow-9)
  ("M-1" swap-buffer-window-no-follow-1)
  ("M-2" swap-buffer-window-no-follow-2)
  ("M-3" swap-buffer-window-no-follow-3)
  ("M-4" swap-buffer-window-no-follow-4)
  ("M-5" swap-buffer-window-no-follow-5)
  ("M-6" swap-buffer-window-no-follow-6)
  ("M-7" swap-buffer-window-no-follow-7)
  ("M-8" swap-buffer-window-no-follow-8)
  ("M-9" swap-buffer-window-no-follow-9)
  ("C-1" winum-select-window-1)
  ("C-2" winum-select-window-2)
  ("C-3" winum-select-window-3)
  ("C-4" winum-select-window-4)
  ("C-5" winum-select-window-5)
  ("C-6" winum-select-window-6)
  ("C-7" winum-select-window-7)
  ("C-8" winum-select-window-8)
  ("C-9" winum-select-window-9))
(my/set-leader-keys "b." 'my/buffer-transient-state/body)

;; end of Buffer transient state

;; Window Manipulation Transient State

(defun my/shrink-window-horizontally (delta)
  "Wrap `my/shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t))

(defun my/shrink-window (delta)
  "Wrap `my/shrink-window'."
  (interactive "p")
  (shrink-window delta))

(defun my/enlarge-window (delta)
  "Wrap `my/enlarge-window'."
  (interactive "p")
  (enlarge-window delta))

(defun my/enlarge-window-horizontally (delta)
  "Wrap `my/enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t))

(my|define-transient-state window-manipulation
  :title "Window Manipulation Transient State"
  :doc (concat "
 Select^^^^               Move^^^^              Split^^               Resize^^             Other^^
 ──────^^^^─────────────  ────^^^^────────────  ─────^^─────────────  ──────^^───────────  ─────^^──────────────────
 [_j_/_k_]  down/up       [_J_/_K_] down/up     [_s_] vertical        [_[_] shrink horiz   [_u_] restore prev layout
 [_h_/_l_]  left/right    [_H_/_L_] left/right  [_S_] verti & follow  [_]_] enlarge horiz  [_U_] restore next layout
 [_0_.._9_] window 0..9   [_r_]^^   rotate fwd  [_v_] horizontal      [_{_] shrink verti   [_d_] close current
 [_w_]^^    other window  [_R_]^^   rotate bwd  [_V_] horiz & follow  [_}_] enlarge verti  [_D_] close other
 [_o_]^^    other frame   ^^^^                  ^^                    ^^                   "
               (if (require 'golden-ratio nil t)
                   "[_g_] golden-ratio %`golden-ratio-mode"
                 "")
               "\n ^^^^                     ^^^^                  ^^                    ^^                   [_q_] quit")
  :bindings
  ("q" nil :exit t)
  ("0" winum-select-window-0)
  ("1" winum-select-window-1)
  ("2" winum-select-window-2)
  ("3" winum-select-window-3)
  ("4" winum-select-window-4)
  ("5" winum-select-window-5)
  ("6" winum-select-window-6)
  ("7" winum-select-window-7)
  ("8" winum-select-window-8)
  ("9" winum-select-window-9)
  ("-" split-window-below-and-focus)
  ("/" split-window-right-and-focus)
  ("[" my/shrink-window-horizontally)
  ("]" my/enlarge-window-horizontally)
  ("{" my/shrink-window)
  ("}" my/enlarge-window)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
  ("H" evil-window-move-far-left)
  ("<S-left>" evil-window-move-far-left)
  ("J" evil-window-move-very-bottom)
  ("<S-down>" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("<S-up>" evil-window-move-very-top)
  ("L" evil-window-move-far-right)
  ("<S-right>" evil-window-move-far-right)
  ("o" other-frame)
  ("r" my/rotate-windows-forward)
  ("R" my/rotate-windows-backward)
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("w" other-window))
(my/set-leader-keys "w."
  'my/window-manipulation-transient-state/body)

;; end of Window Manipulation Transient State

;; text Manipulation Transient State

(defun my/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun my/scale-up-font ()
  "Scale up the font."
  (interactive)
  (my/scale-up-or-down-font-size 1))

(defun my/scale-down-font ()
  "Scale up the font."
  (interactive)
  (my/scale-up-or-down-font-size -1))

(defun my/reset-font-size ()
  "Reset the font size."
  (interactive)
  (my/scale-up-or-down-font-size 0))

(my|define-transient-state scale-font
  :title "Font Scaling Transient State"
  :doc "\n[_+_/_=_] scale up [_-_] scale down [_0_] reset font [_q_] quit"
  :bindings
  ("+" my/scale-up-font)
  ("=" my/scale-up-font)
  ("-" my/scale-down-font)
  ("0" my/reset-font-size)
  ("q" nil :exit t))
(my/set-leader-keys "zx" 'my/scale-font-transient-state/body)

;; end of Text Manipulation Transient State

;; Transparency transient-state

(defun my/toggle-transparency (&optional frame)
  "Toggle between transparent and opaque state for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let ((alpha (frame-parameter frame 'alpha))
        (dotfile-setting (cons dotspacemacs-active-transparency
                               dotspacemacs-inactive-transparency)))
    (if (equal alpha dotfile-setting)
        (my/disable-transparency frame)
      (my/enable-transparency frame dotfile-setting))))

(defun my/enable-transparency (&optional frame alpha)
  "Enable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame.
ALPHA is a pair of active and inactive transparency values. The
default value for ALPHA is based on
`dotspacemacs-active-transparency' and
`dotspacemacs-inactive-transparency'."
  (interactive)
  (let ((alpha-setting (or alpha
                           (cons dotspacemacs-active-transparency
                                 dotspacemacs-inactive-transparency))))
    (set-frame-parameter frame 'alpha alpha-setting)))

(defun my/disable-transparency (&optional frame)
  "Disable transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'alpha '(100 . 100)))

(defun my/increase-transparency (&optional frame)
  "Increase transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (increased-alpha (- current-alpha 5)))
    (when (>= increased-alpha frame-alpha-lower-limit)
      (set-frame-parameter frame 'alpha
                           (cons increased-alpha increased-alpha)))))

(defun my/decrease-transparency (&optional frame)
  "Decrease transparency for FRAME.
If FRAME is nil, it defaults to the selected frame."
  (interactive)
  (let* ((current-alpha (or (car (frame-parameter frame 'alpha)) 100))
         (decreased-alpha (+ current-alpha 5)))
    (when (<= decreased-alpha 100)
      (set-frame-parameter frame 'alpha
                           (cons decreased-alpha decreased-alpha)))))

(my|define-transient-state scale-transparency
  :title "Frame Transparency Transient State"
  :doc "\n[_+_/_=_] increase transparency [_-_] decrease [_T_] toggle [_q_] quit"
  :bindings
  ("+" my/increase-transparency)
  ("=" my/increase-transparency)
  ("-" my/decrease-transparency)
  ("T" my/toggle-transparency)
  ("q" nil :exit t))
(my/set-leader-keys "TT"
  'my/scale-transparency-transient-state/my/toggle-transparency)

(provide 'core-evil-keybindings)
