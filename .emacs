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
	    :after (progn (global-set-key (kbd "C-x C-z") 'magit-status))
    )
    (:name evil
	    :after (progn (evil-mode 1))
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
    color-theme-solarized
    color-theme-tomorrow
    color-theme-zenburn
    escreen                   ; screen for emacs, C-\ C-h
    evil                      ; vim is in my blood
    evil-surround             ; cs"'
    evil-numbers              ; evil-numbers/{inc,dec}-at-pt
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

;; Basic behavior
(savehist-mode 1)
(setq make-backup-files nil)        ; get rid of clutter~
(setq vc-follow-symlinks t)         ; just edit the file.

;; Visual settings
(setq inhibit-splash-screen t) ; no splash screen, thanks
(menu-bar-mode 0)                    ; no menu bar
(tool-bar-mode 0)                    ; no tool bar
(scroll-bar-mode 0)                  ; no scroll bar
(line-number-mode 1)			     ; have line numbers and
(column-number-mode 1)		  	     ; column numbers in the mode line
(global-hl-line-mode)			     ; highlight current line
(global-visual-line-mode)            ; without this, hl-line-mode and org-indent-mode 
                                     ; conflict with each other. this should probably
                                     ; be enabled solely for org-mode via a hook.
(global-linum-mode 1)			     ; add line numbers on the left

(defadvice comment-region (before slick-comment activate compile)
  "When called interactively with no active region, comment a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice uncomment-region (before slick-uncomment activate compile)
  "When called interactively with no active region, uncomment a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position) (line-beginning-position 2)))))

;; evil-mode customizations
(define-key evil-normal-state-map (kbd "C-6") 'evil-buffer)
;;; make esc quit everything.
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)
;; define a leader key for use in custom mappings.
(let ((leader ","))
(define-key evil-normal-state-map (concat leader "cc") 'comment-region)
(define-key evil-insert-state-map (concat leader "cc") 'comment-region)
(define-key evil-visual-state-map (concat leader "cc") 'comment-region)
(define-key evil-normal-state-map (concat leader "cu") 'uncomment-region)
(define-key evil-insert-state-map (concat leader "cu") 'uncomment-region)
(define-key evil-visual-state-map (concat leader "cu") 'uncomment-region)
)

;; org-mode settings
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-startup-indented t)
(setq org-log-done 'time)
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)
;; Remap org-mode meta keys for convenience
;; These didn't work when I tried to combine them using mapc...
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'insert org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'insert org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'insert org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'insert org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'insert org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'insert org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'insert org-mode-map (kbd "M-J") 'org-shiftmetadown)
(evil-define-key 'insert org-mode-map (kbd "M-H") 'org-shiftmetaleft)
;; ;; Parent can't be marked as done unless all children are done
;; (setq org-enforce-todo-dependencies t)
;; ;; TODO make this happen automatically.
;; (defun org-summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(setq org-publish-project-alist
      '(
	("all-org"
	 :base-directory "~/org"
	 :base-extension "org"
	 :publishing-directory "~/org/html"
	 :recursive nil
	 :publishing-function org-publish-org-to-html
	 :headline-levels 4
	 :style-include-default nil
	 :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"zeller.css\" />"
	)
	("all-static"
	 :base-directory "~/org"
	 :base-extension "css\\|js\\|png\\|jpg"
	 :publishing-directory "~/org/html"
	 :recursive nil
	 :publishing-function org-publish-attachment
	)
	("all" :components ("all-org" "all-static"))
       ))
