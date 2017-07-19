(defvar my-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local my-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro my|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `my-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `my-jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my//init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "my-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`my-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq my-jump-handlers
               (append ,handlers-list
                       my-default-jump-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (my/set-leader-keys-for-major-mode ',mode
           "gg" 'my/jump-to-definition
           "gG" 'my/jump-to-definition-other-window)))))

(defun my/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler my-jump-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

(defun my/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `my/jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (my/jump-to-definition)))

;; Set the `:jump' property manually instead of just using `evil-define-motion'
;; in an `eval-after-load' macro invocation because doing that prevents
;; `describe-function' from correctly finding the source.
(with-eval-after-load 'evil
  (evil-set-command-property 'my/jump-to-definition :jump t))

(provide 'core-jump)
