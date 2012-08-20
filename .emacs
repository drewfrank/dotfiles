;; This .emacs file is based off of init.el from emacs-kicker.
;; It uses el-get to manage installed packages.

(require 'cl)               ; common lisp goodies, loop
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
    "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
    (lambda (s)
      (end-of-buffer)
      (eval-print-last-sexp))))
;; Now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes -- ideally, keep this in separate files in a recipe
;; directory (just to keep .emacs clean)
(setq
  el-get-sources '(
    (:name magit
	    :after (lambda ()
               (global-set-key (kbd "C-x C-z") 'magit-status)
             )
    )
    (:name evil
	    :after (lambda ()
               (evil-mode 1)
             )
    )
  )
)

;; Now set our own packages.
(setq
  my:el-get-packages
  '(
    el-get                    ; el-get is self-hosting
    auto-complete             ; complete as you type with overlays
    color-theme               ; nice looking emacs
    escreen                   ; screen for emacs, C-\ C-h
    evil                      ; vim is in my blood
    magit                     ; git integration
    zencoding-mode            ; http://www.emacswiki.org/emacs/ZenCoding
  ))
;; Not sure what this does, but it seems important.
(setq my:el-get-packages
      (append
        my:el-get-packages
        (loop for src in el-get-sources collect (el-get-source-name src))))

;; Install new packages and init already installed packages.
(el-get 'sync my:el-get-packages)

;; Visual settings
(setq inhibit-splash-screen t) ; no splash screen, thanks
(line-number-mode 1)			     ; have line numbers and
(column-number-mode 1)			   ; column numbers in the mode line

(global-hl-line-mode)			     ; highlight current line
(global-linum-mode 1)			     ; add line numbers on the left

;; org-mode settings
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)
(setq org-log-done 'time)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/research/org-wiki/2.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
