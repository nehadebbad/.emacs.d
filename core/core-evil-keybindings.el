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



(provide 'core-evil-keybindings)
