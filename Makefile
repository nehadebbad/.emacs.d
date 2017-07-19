# Ensure emacs always runs from this makefile's PWD
EMACS_FLAGS=--eval '(setq user-emacs-directory default-directory)' -l my-packages.el
EMACS=emacs --batch $(EMACS_FLAGS)

install: my-packages.el
	@$(EMACS) -f my-install-packages
