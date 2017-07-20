;;; Code:
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defvar my-packages
  '(ace-window
    use-package
    solarized-theme
    rainbow-delimiters
    all-the-icons
    spaceline
    persp-mode
    counsel
    company
    pos-tip
    company-quickhelp
    hydra
    projectile
    counsel-projectile
    neotree
    magit
    multiple-cursors
    smartparens
    flycheck
    elpy
    ein
    py-autopep8
    prodigy
    evil
    diminish
    bind-map
    which-key
    helm
    paredit
    async
    evil-escape
    evil-visualstar
    exec-path-from-shell
    help-fns+)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  "Check if all packages in `my-packages' are installed."
  (every #'package-installed-p my-packages))

(defun my-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package my-packages)
    (add-to-list 'my-packages package))
  (unless (package-installed-p package)
    (message "Installing package : %s" package)
    (package-install package)
    (message "Package %s installed" package)))

(defun my-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'my-require-package packages))

(defun my-install-packages ()
  "Install all packages listed in `my-packages'."
  (unless (my-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (my-require-packages my-packages)))
