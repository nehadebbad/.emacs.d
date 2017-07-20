(require 'cl-lib)

(defun my//set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test my--indent-variable-alist)
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
           (throw 'break ' (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

;; need to delay this macro since it relies on evil key maps to be defined
(with-eval-after-load 'evil
  (defmacro evil-map (state key seq)
    "Map for a given STATE a KEY to a sequence SEQ of keys.
Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
    (let ((map (intern (format "evil-%S-state-map" state))))
      `(define-key ,map ,key
         (lambda ()
           (interactive)
           ,(if (string-equal key (substring seq 0 1))
                `(progn
                   (call-interactively ',(lookup-key evil-normal-state-map key))
                   (execute-kbd-macro ,(substring seq 1)))
              (execute-kbd-macro ,seq)))))))

(defun my//hydra-key-doc-function (key key-width doc doc-width)
  "Custom hint documentation format for keys."
  (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
          key doc))

(defmacro my|define-text-object (key name start end)
  "Define a text object and a surround pair.
START and END are strings (not regular expressions) that define
the boundaries of the text object."
  `(progn
     (my|define-text-object-regexp ,key ,name
                                          ,(regexp-quote start)
                                          ,(regexp-quote end))
     (with-eval-after-load 'evil-surround
       (push (cons (string-to-char ,key)
                   (if ,end
                       (cons ,start ,end)
                     ,start))
             evil-surround-pairs-alist))))

(defmacro my|define-text-object-regexp (key name start-regexp end-regexp)
  "Define a text object.
START-REGEXP and END-REGEXP are the boundaries of the text object."
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(defun spacemacs//run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

;; Utility functions or functions that change the default behaviour of existing functions
(defun my/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun my/evil-smart-doc-lookup ()
  "Version of `evil-lookup' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m h h`, to lookup the source of the definition,
        while falling back to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotspacemacs-leader-key " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun my/error-delegate ()
  "Decide which error API to delegate to.
Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
  (if (and (bound-and-true-p flycheck-mode)
           (let ((buf (ignore-errors (next-error-find-buffer))))
             (not (and buf (get-buffer-window buf)))))
      'flycheck
    'emacs))

(defun my/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (my/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-next-error))
     ((eq 'emacs sys) (call-interactively 'next-error)))))

(defun my/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (my/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-previous-error))
     ((eq 'emacs sys) (call-interactively 'previous-error)))))

(defun my/toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun my/split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun my/push-mark-and-goto-beginning-of-line ()
  "Push a mark at current location and go to the beginning of the line."
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun my/push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

(defun my/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun my/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun my/evil-goto-next-line-and-indent (&optional count)
  "Match the current lines indentation to the next line.
A COUNT argument matches the indentation to the next COUNT lines."
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

(defcustom my-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'my)

(defcustom my-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode)
  "Major modes which `my/check-large-file' will not be
automatically applied to."
  :group 'my
  :type '(list symbol))

(defun my/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode my-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

;; TODO implement centered buffer mode
(defun spacemacs/toggle-centered-buffer-mode ()
  "Toggle `spacemacs-centered-buffer-mode'."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (call-interactively 'spacemacs-centered-buffer-mode)))

(defun spacemacs/toggle-centered-buffer-mode-frame ()
  "Open current buffer in the new frame centered and without mode-line."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (switch-to-buffer-other-frame (current-buffer) t)
    (toggle-frame-fullscreen)
    (run-with-idle-timer
     ;; FIXME: We need this delay to make sure that the
     ;; `toggle-frame-fullscreen' fully "finished"
     ;; it will be better to use something more reliable
     ;; instead :)
     1
     nil
     (lambda ()
       (call-interactively 'spacemacs-centered-buffer-mode)
       (setq mode-line-format nil)))))

(defun spacemacs/centered-buffer-mode-full-width ()
  "Center buffer in the frame."
  ;; FIXME Needs new key-binding.
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (spacemacs/maximize-horizontally)
    (call-interactively 'spacemacs-centered-buffer-mode)))


(defun my/useful-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
        (cl-loop for useful-regexp in my-useful-buffers-regexp
                 thereis (string-match-p useful-regexp buf-name))
        (cl-loop for useless-regexp in my-useless-buffers-regexp
                 never (string-match-p useless-regexp buf-name)))))

(defun my/useless-buffer-p (buffer)
  "Determines if a buffer is useless."
  (not (spacemacs/useful-buffer-p buffer)))

(defun my/swap-windows (window1 window2)
  "Swap two windows.
WINDOW1 and WINDOW2 must be valid windows. They may contain child
windows."
  (let ((state1 (window-state-get window1))
        (state2 (window-state-get window2)))
    ;; to put state into dedicated windows, we must undedicate them first (not
    ;; needed with Emacs 25.1)
    (dolist (win (list window1 window2))
      (if (window-live-p win)
          (set-window-dedicated-p win nil)
        ;; win has sub-windows, undedicate all of them
        (walk-window-subtree (lambda (leaf-window)
                               (set-window-dedicated-p leaf-window nil))
                             win)))
    (window-state-put state1 window2)
    (window-state-put state2 window1)))

(defun my/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((window-tree (car (window-tree)))
           (current-split-vertical-p (car window-tree))
           (first-window (nth 2 window-tree))
           (second-window (nth 3 window-tree))
           (second-window-state (window-state-get second-window))
           (splitter (if current-split-vertical-p
                         #'split-window-horizontally
                       #'split-window-vertically)))
      (delete-other-windows first-window)
      ;; `window-state-put' also re-selects the window if needed, so we don't
      ;; need to call `select-window'
      (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

(defun my/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun my/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (my/rotate-windows-forward (* -1 count)))

(defun my/move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the spacemacs numbering. follow-focus-p
   controls whether focus moves to new window (with buffer), or stays on
   current"
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

(defun my/swap-buffers-to-window (windownum follow-focus-p)
  "Swaps visible buffers between active window and selected window.
   follow-focus-p controls whether focus moves to new window (with buffer), or
   stays on current"
  (interactive)
  (let* ((b1 (current-buffer))
         (w1 (selected-window))
         (w2 (winum-get-window-by-number windownum))
         (b2 (window-buffer w2)))
    (unless (eq w1 w2)
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (unrecord-window-buffer w1 b1)
      (unrecord-window-buffer w2 b2)))
  (when follow-focus-p (winum-select-window-by-number windownum)))

(dotimes (i 9)
  (let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
              ,(format "Move buffer to the window with number %i." n)
              (interactive "P")
              (if arg
                  (my/swap-buffers-to-window ,n t)
                (my/move-buffer-to-window ,n t))))
    (eval `(defun ,(intern (format "move-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (my/move-buffer-to-window ,n nil)))
    (eval `(defun ,(intern (format "swap-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (my/swap-buffers-to-window ,n nil)))
    ))

(defun my/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.
When NEW-FILENAME is not specified, asks user for a new name.
Also renames associated buffer (if any exists), invalidates
projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists!" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-name)
               (recentf-remove-if-non-kept filename))
             (when (and (require 'projectile nil 'noerror)
                        (projectile-project-p))
               (call-interactively #'projectile-invalidate-cache))
             (message "File '%s' successfully renamed to '%s'" short-name (file-name-nondirectory new-name))))))i)

(defun my/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.
If called without a prefix argument, the prompt is
initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (and (require 'projectile nil 'noerror)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name) 'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?") new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun my/delete-file (filename &optional ask-user)
  "Remove specified file or directory.
Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.
When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (when (and (require 'projectile nil 'noerror)
                 (projectile-project-p))
        (call-interactively #'projectile-invalidate-cache)))))

(defun my/delete-file-confirm (filename)
  "Remove specified file or directory after users approval.
FILENAME is deleted using `my/delete-file' function.."
  (interactive "f")
  (funcall-interactively #'my/delete-file filename t))

(defun my/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (require 'projectile nil 'noerror)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun my/sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name parsed-method
                                                     parsed-user
                                                     parsed-host
                                                     nil
                                                     parsed-hop
                                                     ))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name "sudo"
                                                       "root"
                                                       parsed-host
                                                       parsed-localname
                                                       new-hop)))
           new-fname))))))

;; check when opening large files - literal file open
(defun my/check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode my-large-file-modes-list))
           size (> size (* 1024 1024 dotspacemacs-large-file-size))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun my/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun my/ace-delete-window (&optional arg)
  "Ace delete window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (require 'ace-window)
  (aw-select
   " Ace - Delete Window"
   (lambda (window)
     (when (equal '(4) arg)
       (with-selected-window window
         (my/kill-this-buffer arg)))
     (aw-delete-window window))))

(defun my/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun my/ace-kill-this-buffer (&optional arg)
  "Ace kill visible buffer in a window.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (require 'ace-window)
  (let (golden-ratio-mode)
    (aw-select
     " Ace - Kill buffer in Window"
     (lambda (window)
       (with-selected-window window
         (my/kill-this-buffer arg))))))

(defun my/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun my/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(defun my/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun my/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun my/new-empty-buffer (&optional split)
  "Create a new buffer called untitled(<n>).
A SPLIT argument with the value: `left',
`below', `above' or `right', opens the new
buffer in a split window."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    (case split
      ('left  (split-window-horizontally))
      ('below (my/split-window-vertically-and-switch))
      ('above (split-window-vertically))
      ('right (my/split-window-horizontally-and-switch)))
    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t))
    ;; pass non-nil force-same-window to prevent `switch-to-buffer' from
    ;; displaying buffer in another window
    (switch-to-buffer newbuf nil 'force-same-window)))

(defun my/new-empty-buffer-left ()
  "Create a new buffer called untitled(<n>),
in a split window to the left."
  (interactive)
  (my/new-empty-buffer 'left))

(defun my/new-empty-buffer-below ()
  "Create a new buffer called untitled(<n>),
in a split window below."
  (interactive)
  (my/new-empty-buffer 'below))

(defun my/new-empty-buffer-above ()
  "Create a new buffer called untitled(<n>),
in a split window above."
  (interactive)
  (my/new-empty-buffer 'above))

(defun my/new-empty-buffer-right ()
  "Create a new buffer called untitled(<n>),
in a split window to the right."
  (interactive)
  (my/new-empty-buffer 'right))

(defun my/split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun my/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun my/layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun my/layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun my/insert-line-above-no-indent (count)
  "Insert a new line above with no indentation."
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
       (if (eq (line-number-at-pos) 1)
          (evil-move-beginning-of-line)
        (progn
          (evil-previous-line)
          (evil-move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

(defun my/insert-line-below-no-indent (count)
  "Insert a new line below with no indentation."
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

(defun my/rudekill-matching-buffers (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.
Returns the count of killed buffers."
  (let* ((buffers (remove-if-not
                   (lambda (buffer)
                     (let ((name (buffer-name buffer)))
                       (and name (not (string-equal name ""))
                            (or internal-too (/= (aref name 0) ?\s))
                            (string-match regexp name))))
                   (buffer-list))))
    (mapc 'kill-buffer buffers)
    (length buffers)))

(defun my/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.
Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message
   (format "%d buffer(s) killed."
           (my/rudekill-matching-buffers regexp internal-too))))

(defvar my-really-kill-emacs nil
  "prevent window manager close from closing instance.")

(defun my//persistent-server-running-p ()
  "Requires my-really-kill-emacs to be toggled and
dotspacemacs-persistent-server to be t"
  (and (fboundp 'server-running-p)
       (server-running-p)
       dotspacemacs-persistent-server))

(defadvice kill-emacs (around my-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (and (not my-really-kill-emacs)
           (my//persistent-server-running-p))
      (my/frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around my-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (and (not my-really-kill-emacs)
           (my//persistent-server-running-p))
      (my/frame-killer)
    ad-do-it))

(defun my/save-buffers-kill-emacs ()
  "Save all changed buffers and exit Spacemacs"
  (interactive)
  (setq my-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun my/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (setq my-really-kill-emacs t)
  (kill-emacs))

(defun my/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (setq my-really-kill-emacs t)
  (save-some-buffers nil t)
  (kill-emacs))

(defun my/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case nil
      (delete-frame nil 1)
    (error
     (make-frame-invisible nil 1))))

(defun my/toggle-frame-fullscreen ()
  "Respect the `dotspacemacs-fullscreen-use-non-native' variable when
toggling fullscreen."
  (interactive)
  (if dotspacemacs-fullscreen-use-non-native
      (my/toggle-frame-fullscreen-non-native)
    (toggle-frame-fullscreen)))

(defun my/toggle-fullscreen ()
  "Toggle full screen on X11 and Carbon"
  (interactive)
  (cond
   ((eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen))
                           'fullboth)))
   ((eq window-system 'mac)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullscreen)))))

(defun my/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	   (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	     (if (eq (frame-parameter nil 'maximized) 'maximized)
		 'maximized)
	   'fullboth)))))

;; taken from Prelude: https://github.com/bbatsov/prelude
(defmacro my|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun my/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun my/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun my//find-ert-test-buffer (ert-test)
  "Return the buffer where ERT-TEST is defined."
  (save-excursion
    (car (find-definition-noselect (ert-test-name ert-test) 'ert-deftest))))

(defun my/ert-run-tests-buffer ()
  "Run all the tests in the current buffer."
  (interactive)
  (save-buffer)
  (load-file (buffer-file-name))
  (let ((cbuf (current-buffer)))
    (ert '(satisfies (lambda (test)
                       (eq cbuf (my//find-ert-test-buffer test)))))))

(defun my//open-in-external-app (file-path)
  "Open `file-path' in external application."
  (cond
   ((spacemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
   ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open" file-path)))))

(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (spacemacs//open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (spacemacs//open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(defun spacemacs/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; http://stackoverflow.com/a/10216338/4869
(defun spacemacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun spacemacs/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; BEGIN align functions

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun spacemacs/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))

    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))

    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun spacemacs/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro spacemacs|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "spacemacs/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (spacemacs/align-repeat start end ,regexp ,justify-right after)))))

(spacemacs|create-align-repeat-x "comma" "," nil t)
(spacemacs|create-align-repeat-x "semicolon" ";" nil t)
(spacemacs|create-align-repeat-x "colon" ":" nil t)
(spacemacs|create-align-repeat-x "equal" "=")
(spacemacs|create-align-repeat-x "math-oper" "[+\\-*/]")
(spacemacs|create-align-repeat-x "ampersand" "&")
(spacemacs|create-align-repeat-x "bar" "|")
(spacemacs|create-align-repeat-x "left-paren" "(")
(spacemacs|create-align-repeat-x "right-paren" ")" t)
(spacemacs|create-align-repeat-x "left-curly-brace" "{")
(spacemacs|create-align-repeat-x "right-curly-brace" "}" t)
(spacemacs|create-align-repeat-x "left-square-brace" "\\[")
(spacemacs|create-align-repeat-x "right-square-brace" "\\]" t)
(spacemacs|create-align-repeat-x "backslash" "\\\\")

;; END align functions

(defun spacemacs/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun spacemacs/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun spacemacs/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

;; from https://www.emacswiki.org/emacs/CopyingWholeLines
(defun spacemacs/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                             ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun spacemacs/uniquify-lines ()
  "Remove duplicate adjacent lines in a region or the current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun spacemacs/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun spacemacs/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (spacemacs/sort-lines -1))

(defun spacemacs/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column,
using a visual block/rectangle selection.
A non-nil argument sorts in REVERSE order."
  (interactive "P")
  (if (and
       ;; is there an active selection
       (or (region-active-p) (evil-visual-state-p))
       ;; is it a block or rectangle selection
       (or (eq evil-visual-selection 'block) (eq rectangle-mark-mode t))
       ;; is the selection height 2 or more lines
       (>= (1+ (- (line-number-at-pos (region-end))
                  (line-number-at-pos (region-beginning)))) 2))
      (sort-columns reverse (region-beginning) (region-end))
    (error "Sorting by column requires a block/rect selection on 2 or more lines.")))

(defun spacemacs/sort-lines-by-column-reverse ()
"Sort lines by the selected column in reverse order,
using a visual block/rectangle selection."
  (interactive)
  (spacemacs/sort-lines-by-column -1))

;; BEGIN linum mouse helpers

(defvar spacemacs-linum-mdown-line nil
  "Define persistent variable for linum selection")

(defun spacemacs//line-at-click ()
  "Determine the visual line at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos))
      )))

(defun spacemacs/md-select-linum (event)
  "Set point as spacemacs-linum-mdown-line"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (spacemacs//line-at-click))
  (set-mark (point))
  (setq spacemacs-linum-mdown-line
        (line-number-at-pos)))

(defun spacemacs/mu-select-linum ()
  "Select code block between point and spacemacs-linum-mdown-line"
  (interactive)
  (when spacemacs-linum-mdown-line
    (let (mu-line)
      (setq mu-line (spacemacs//line-at-click))
      (goto-line (max spacemacs-linum-mdown-line mu-line))
      (set-mark (line-end-position))
      (goto-line (min spacemacs-linum-mdown-line mu-line))
      (setq spacemacs-linum-mdown-line nil))))

(defun spacemacs/select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1)
    (when (re-search-backward "\n[ \t]*\n" nil "move")
      (re-search-forward "\n[ \t]*\n"))
    (setq p1 (point))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1)))

;; END linum mouse helpers

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
      (lambda (buf str)

        (let ((case-fold-search nil))
          (if (or (string-match "exited abnormally" str)
                  (string-match "FAILED" (buffer-string)))

              ;; there were errors
              (message "There were errors. SPC-e-n to visit.")
            (unless (or (string-match "Grep finished" (buffer-string))
                        (string-match "Ag finished" (buffer-string))
                        (string-match "nosetests" (buffer-name)))

              ;; no errors
              (message "compilation ok."))))))

;; from http://www.emacswiki.org/emacs/WordCount
(defun spacemacs/count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words
        alist_words_compare
        (formated "")
        (overview (call-interactively 'count-words)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (interactive-p)
      (if (> (length formated) 2)
          (message (format "%s\nWord count: %s"
                           overview
                           (substring formated 0 -2)))
        (message "No words.")))
    words))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun spacemacs/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) spacemacs-yank-indent-threshold)
      (indent-region beg end nil)))

(spacemacs|advise-commands
 "indent" (yank yank-pop evil-paste-before evil-paste-after) around
 "If current mode is not one of spacemacs-indent-sensitive-modes
 indent yanked text (with universal arg don't indent)."
 (evil-start-undo-step)
 ad-do-it
 (if (and (not (equal '(4) (ad-get-arg 0)))
          (not (member major-mode spacemacs-indent-sensitive-modes))
          (or (derived-mode-p 'prog-mode)
              (member major-mode spacemacs-indent-sensitive-modes)))
     (let ((transient-mark-mode nil)
           (save-undo buffer-undo-list))
       (spacemacs/yank-advised-indent-function (region-beginning)
                                               (region-end))))
 (evil-end-undo-step))

;; find file functions in split
(defun spacemacs//display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.
BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains
a split-side entry, its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist)
    window))

(defun spacemacs/find-file-vsplit (file)
  "find file in vertical split"
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . right)))))

(defun spacemacs/find-file-split (file)
  "find file in horizontal split"
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . below)))))

(defun spacemacs/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (and (not exists)
               (not (eq major-mode dotspacemacs-scratch-mode))
               (fboundp dotspacemacs-scratch-mode))
      (funcall dotspacemacs-scratch-mode))))

(defvar spacemacs--killed-buffer-list nil
  "List of recently killed buffers.")

(defun spacemacs//add-buffer-to-killed-list ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name spacemacs--killed-buffer-list)))

(defun spacemacs/reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when spacemacs--killed-buffer-list
    (find-file (pop spacemacs--killed-buffer-list))))

(defun spacemacs/switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))))

(defun spacemacs/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))


;; Line number

(defun spacemacs/no-linum (&rest ignore)
  "Disable linum if current buffer."
  (when (or 'linum-mode global-linum-mode)
    (linum-mode 0)))

(defun spacemacs/enable-line-numbers-p ()
  "Return non-nil if line numbers should be enabled for current buffer.
Decision is based on `dotspacemacs-line-numbers'."
  (and dotspacemacs-line-numbers
       (spacemacs//linum-current-buffer-is-not-special)
       (spacemacs//linum-curent-buffer-is-not-too-big)
       (or (spacemacs//linum-backward-compabitility)
           (spacemacs//linum-enabled-for-current-major-mode))))

(defun spacemacs//linum-on (origfunc &rest args)
  "Advice function to improve `linum-on' function."
  (when (spacemacs/enable-line-numbers-p)
    (apply origfunc args)))

(defun spacemacs//linum-update-window-scale-fix (win)
  "Fix linum for scaled text in the window WIN."
  (set-window-margins win
                      (ceiling (* (if (boundp 'text-scale-mode-step)
                                      (expt text-scale-mode-step
                                            text-scale-mode-amount)
                                    1)
                                  (or (car (window-margins win)) 1)))))

(defun spacemacs//linum-backward-compabitility ()
  "Return non-nil if `dotspacemacs-line-numbers' has an old format and if
`linum' should be enabled."
  (and dotspacemacs-line-numbers
       (not (listp dotspacemacs-line-numbers))
       (or (eq dotspacemacs-line-numbers t)
           (eq dotspacemacs-line-numbers 'relative))))

(defun spacemacs//linum-current-buffer-is-not-special ()
  "Return non-nil if current buffer is not a special buffer."
  (not (string-match-p "\\*.*\\*" (buffer-name))))

(defun spacemacs//linum-curent-buffer-is-not-too-big ()
  "Return non-nil if buffer size is not too big."
  (not (and (listp dotspacemacs-line-numbers)
            (spacemacs/mplist-get dotspacemacs-line-numbers :size-limit-kb)
            (> (buffer-size)
               (* 1000 (car (spacemacs/mplist-get dotspacemacs-line-numbers
                                                  :size-limit-kb)))))))

;; mode in :enabled, not in :disabled ==> t
;; mode not in :enabled, in :disabled ==> nil
;; mode in :enabled, parent in :disabled ==> t
;; parent in :enabled, mode in :disabled ==> nil
;; not in :enabled, not in :disabled, :enabled is empty ==> t
;; not in :enabled, not in :disabled, :enabled is not empty ==> nil
;; both :enabled and :disabled are empty ==> t
(defun spacemacs//linum-enabled-for-current-major-mode ()
  "Return non-nil if line number is enabled for current major-mode."
  (let* ((enabled-for-modes (spacemacs/mplist-get dotspacemacs-line-numbers
                                                  :enabled-for-modes))
         (disabled-for-modes (spacemacs/mplist-get dotspacemacs-line-numbers
                                                   :disabled-for-modes))
         (enabled-for-parent (apply #'derived-mode-p enabled-for-modes))
         (disabled-for-parent (apply #'derived-mode-p disabled-for-modes)))
    (or
     ;; current mode or a parent is in :enabled-for-modes, and there isn't a
     ;; more specific parent (or the mode itself) in :disabled-for-modes
     (and enabled-for-parent
          ;; handles the case where current major-mode has a parent both in
          ;; :enabled-for-modes and in :disabled-for-modes. Return non-nil if
          ;; enabled-for-parent is the more specific parent (IOW doesn't derive
          ;; from disabled-for-parent)
          (not (spacemacs/derived-mode-p enabled-for-parent disabled-for-parent)))
     ;; current mode (or parent) not explicitly disabled, and :enabled-for-modes
     ;; not explicitly specified by user (meaning if it isn't explicitly
     ;; disabled then it's enabled)
     (and (null enabled-for-modes) (not disabled-for-parent)))))

(provide 'core-funcs)
