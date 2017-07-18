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

(defvar dotspacemacs-show-transient-state-title t
  "If non nil show the titles of transient states.")

(defvar dotspacemacs-show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys.")

(provide 'core-variables)
