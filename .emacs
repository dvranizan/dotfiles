(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; package management
;; tools
(straight-use-package 'helm)
(straight-use-package 'magit)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'deadgrep)
(straight-use-package 'doom-modeline)
(straight-use-package 'rust-mode)
(straight-use-package 'smex)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company)
(straight-use-package 'shackle)
(straight-use-package 'powershell)
(straight-use-package 'dumb-jump)

;; themes
(straight-use-package 'solarized-theme)
(straight-use-package 'dracula-theme)

;;;; set theme and hotkeys
;; Swap c/h
(add-hook 'c-mode-common-hook
	  (lambda()
	    (local-set-key (kbd "C-c o") 'ff-find-other-file)))
;; deadgrep
(global-set-key (kbd "C-c f") #'deadgrep)
;; themes
(load-theme 'dracula t)
					; cycle themes
(fset 'my-toggle-themes
      (let ((toggle-var (make-symbol "toggle")))
	(set toggle-var nil)
	`(lambda () (interactive)
	   (cond (,toggle-var
		  (load-theme 'solarized-dark t)
		  (setq ,toggle-var nil))
		 (t
		  (load-theme 'solarized-light t)
		  (setq, toggle-var t)))
	   (message "Theme toggled."))))
(global-set-key (kbd "C-c v") 'my-toggle-themes)
;; Helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;; config tools
;; clangd
(setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
;; TRAMP
(setq explicit-shell-file-name "/bin/bash")
                                        ; speed it up
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq remote-file-name-inhibit-cache nil)
;;; cannot set tramp-remote-path until tramp has been loaded, due to bug
(custom-set-variables
 '(tramp-remote-path (quote (tramp-own-remote-path)) nil (tramp)))
;; Line Modes
(global-hl-line-mode +1)
(global-display-line-numbers-mode)
;; Rainbows
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; doom modeline(
(doom-modeline-init)
;; GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Helm
(helm-mode 1)
                                        ; helm default highlight color is invis
(set-face-attribute 'helm-selection nil 
                    :background "purple"
                    :foreground "black")
                                        ; shackle helm window
(setq helm-display-function 'pop-to-buffer) ; make helm play nice
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)))
;; dumb-jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; rust
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'lsp)
;; c/c++
(add-hook 'c++-mode-hook #'lsp)
;;;; config formatting
;; 4 spaces and
(setq-default c-basic-offset 4)
;; Tabs https://www.emacswiki.org/emacs/TabsAreEvil
(setq-default indent-tabs-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6f3c58c167f50ea43e1acde39813a0cb2b6c4287011e7b67ba77c676ff958f81" default))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
