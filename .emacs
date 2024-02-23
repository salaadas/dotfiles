(package-initialize)

(load "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")

;;; @Temporary: testing the split threshold
(setq split-height-threshold nil)

;;; Change initial message of the *scratch* buffer
;; (setq initial-scratch-message ";; Two plus two is ten... IN BASE FOUR! I'M FINE!")
;; (setq initial-scratch-message ";; There are 69,105 bits in this file.")
(setq initial-scratch-message ";; The strong man is not the good wrestler; but the strong man is he who controls himself when he is angry.")

;;; Appearance
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "Iosevka-15")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; @Note: this is temporary
(setq minibuffer-frame-alist '((minibuffer . only)))

;; show ansi escape sequence in compilation mode Emacs
(rc/require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;; set up hot key for compilling

(setq compilation-directory-locked nil)
(setq salaadas-makescript "./build.sh")
(setq salaadas-build-directory "build")

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (string= default-directory "~") (error "Whoops! Could not find the build directory!!")
      nil)

  (if (file-directory-p salaadas-build-directory) (cd salaadas-build-directory)
    (cd "../")
    (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a build directory."
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a build directory."
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive)
    (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile salaadas-makescript))
  (other-window 1))
(define-key global-map "\M-4" 'make-without-asking)
(global-set-key [f9] 'make-without-asking)

;; my own theme
(load-theme 'actraiser t)

;;; use this to disable `Async-native-compile-log` at startup with Emacs 29+
(setq native-comp-jit-compilation-deny-list '(".*-loaddefs.el.gz"))

;;; use this to close the message buffer on startup
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;;; show the clock in the taskbar
(setq display-time-format "---------------- %a, %d %b %y: %I:%M%p")
(setq display-time-default-load-average nil)
(setq display-time-mail-string "")
(display-time)

(eval-after-load 'zenburn
  (set-face-attribute 'line-number nil :inherit 'default))

;;; color for comments
(rc/require 'fixmee)
(setq fixmee-mode '(simpc-mode c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-incomplete-face)
(make-face 'font-lock-todo-face)
(mapc (lambda (mode)
	    (font-lock-add-keywords
	     mode
	     '(("\\<\\(Todo\\)" 1 'font-lock-todo-face t)
           ("\\<\\(Note\\)" 1 'font-lock-note-face t)
           ("\\<\\(Important\\)" 1 'font-lock-important-face t)
           ("\\<\\(Incomplete\\)" 1 'font-lock-incomplete-face t)
           ("\\<\\(Fixme\\)" 1 'font-lock-fixme-face t))))
	  fixmee-mode)
(modify-face 'font-lock-todo-face "GreenYellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "AntiqueWhite1" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "gold" nil nil t nil t nil nil)
(modify-face 'font-lock-incomplete-face "aquamarine" nil nil t nil t nil nil)
(modify-face 'font-lock-fixme-face "IndianRed" nil nil t nil t nil nil)

;;; my custom keys

;;; for some reason, the definition of 'previous' and 'next' in emacs is wacky
(define-key global-map "\M-]" 'previous-buffer)
(define-key global-map "\M-[" 'next-buffer)

(define-key global-map "\M-j" 'imenu)

(define-key global-map "\M-0" 'delete-window)
(define-key global-map "\M-1" 'delete-other-windows)
(define-key global-map "\M-2" 'split-window-below)
(define-key global-map "\M-3" 'split-window-right)
(define-key global-map "\M-o" 'other-window)
(define-key global-map "\M-k" 'kill-this-buffer)

;;; ido
(rc/require 'smex 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; hl-line mode
(global-hl-line-mode 1)
;; if you want to get funky
;; (set-face-background 'hl-line "midnight blue")
;; (set-face-background 'hl-line "RoyalBlue4")

;;; javascript-mode
;; (setq js-indent-level 2)

;; (setq js-indent-level 2
;;       js2-basic-offset 2
;;       web-mode-markup-indent-offset 2
;;       web-mode-css-indent-offset 2
;;       web-mode-code-indent-offset 2
;;       web-mode-indent-style 2)

;;; c-mode
(add-hook 'java-mode-hook (lambda ()
   (setq c-default-style "bsd")))

(setq-default c-basic-offset 4
              c-default-style '((awk-mode . "awk")
                                (other . "bsd")))
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))
(add-hook 'c++-mode-hook (lambda ()
                           (c-set-offset 'case-label '+)))
(c-set-offset 'case-label '+)

;;; Paredit
(rc/require 'paredit)

(defun rc/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook  'rc/turn-on-paredit)
(add-hook 'lisp-mode-hook        'rc/turn-on-paredit)

;;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;;; Whitespace mode
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;; (add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
;; (add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
;; (add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'nasm-mode-hook 'rc/set-up-whitespace-handling)

;;; display-line-numbers-mode
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode 0))

;;; magit
;;; magit requres this lib, but it is not installed automatically on
;;; Windows.
(rc/require 'cl-lib)
(rc/require 'magit)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;;; multiple cursors
(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; ;;; helm
;; (rc/require 'helm 'helm-git-grep 'helm-ls-git)

;; (setq helm-ff-transformer-show-only-basename nil)

;; (global-set-key (kbd "C-c h t") 'helm-cmd-t)
;; (global-set-key (kbd "C-c h g g") 'helm-git-grep)
;; (global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
;; (global-set-key (kbd "C-c h f") 'helm-find)
;; (global-set-key (kbd "C-c h a") 'helm-org-agenda-files-headings)
;; (global-set-key (kbd "C-c h r") 'helm-recentf)

;;; truncate lines or global word wrap
;; (setq-default truncate-lines t)
;; (setq truncate-partial-width-windows nil)
(global-word-wrap-whitespace-mode t)

;;; Allow maximum 1 line for the mini-buffer
(setq max-mini-window-height 1)

(add-hook 'markdown-mode-hook 'rc/enable-word-wrap)

;;; tramp
;;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
(setq tramp-auto-save-directory "/tmp")

;;; powershell
(rc/require 'powershell)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))

;;; eldoc mode
(defun rc/turn-on-eldoc-mode ()
  (interactive)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-eldoc-mode)

;;; Company
(rc/require 'company)
(require 'company)

;; (setq company-dabbrev-char-regexp "\\sw\\|[a-z-_'/]")
(setq company-dabbrev-other-buffers 'all)
(setq company-dabbrev-code-other-buffers (quote all)) ;; irrelevant
(setq company-dabbrev-ignore-buffers "nil")

(add-to-list 'company-backends '(company-dabbrev-code company-dabbrev company-capf))
;; (setq company-dabbrev-downcase nil)

(global-company-mode)

;;; Nasm Mode
(rc/require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))

;;; LaTeX mode
(add-hook 'tex-mode-hook
          (lambda ()
            (interactive)
            (add-to-list 'tex-verbatim-environments "code")))

(setq font-latex-fontify-sectioning 'color)

;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; Packages that don't require configuration
(rc/require
 'glsl-mode
 'cmake-mode
 'jinja2-mode
 'markdown-mode
 'php-mode
 'typescript-mode
 )

(add-to-list 'load-path "~/.emacs.local/")

(require 'simpc-mode)
;; (add-to-list 'auto-mode-alist '("\\.[hh|cc]\\'" . simpc-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun astyle-buffer (&optional justify)
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=bsd"
     nil
     t)
    (goto-line saved-line-number)))

(add-hook 'simpc-mode-hook
          (lambda ()
            (interactive)
            (setq-local fill-paragraph-function 'astyle-buffer)))

(require 'compile)

;; pascalik.pas(24,44) Error: Can't evaluate constant expression

compilation-error-regexp-alist-alist

(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers-type 'relative)
 '(js-indent-level 2)
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 ;; '(package-selected-packages
 ;;   '(ansi-color-theme fixmee highlight-numbers rainbow-mode php-mode markdown-mode cmake-mode glsl-mode yaml-mode move-text nasm-mode editorconfig company powershell yasnippet multiple-cursors magit paredit ido-completing-read+ smex dash-functional dash))
 '(safe-local-variable-values
   '((eval progn
           (auto-revert-mode 1)
           (rc/autopull-changes)
           (add-hook 'after-save-hook 'rc/autocommit-changes nil 'make-it-local))))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp)))
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
