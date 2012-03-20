(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ido mode (fuzzy matching for everything)
(require 'ido)
(ido-mode t)

;; latex editing
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; better python mode
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;; pymacs + ropemacs
(add-to-list 'load-path "~/.emacs.d/Pymacs/")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; ropemacs
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)


(add-to-list 'load-path "~/.emacs.d/evil/")
(require 'evil)  
(evil-mode 1)
