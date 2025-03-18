(package-initialize)

;;
;; Setup the melpa package stuff:
;;
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun rc/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (rc/require theme-package)
    (load-theme theme t)))




;; (load "~/.emacs.rc/misc-rc.el")

;;
;; Setup miscalleneous things:
;;
(global-set-key (kbd "C-c p") 'find-file-at-point)
(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-width 4
              indent-tabs-mode nil
              compilation-scroll-output t
              visible-bell nil)

;;
;; Duplicat line with Ctrl+,
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'rc/duplicate-line)

;;; A little hack which fixes a problem with meta key in fluxbox under VNC.
(setq x-alt-keysym 'meta)






;; @Temporary: testing the split threshold
(setq split-height-threshold nil)

;; Change initial message of the *scratch* buffer
(setq initial-scratch-message ";; This space intentionally left blank.")

(setq pop-up-windows nil)
(setq Man-width-max nil)

;;; Appearance
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-15")
   ((eq system-type 'gnu/linux) "Iosevka-16")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))
(add-to-list 'default-frame-alist '(width . 110))
(add-to-list 'default-frame-alist '(height . 40))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; @Note: This is temporary, why???
(setq minibuffer-frame-alist '((minibuffer . only)))

;; Show ansi escape sequence in compilation mode Emacs
(rc/require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)



;;
;; My own theme
;;
(deftheme actraiser "Dark-themed colorscheme for Emacs")
(let* ((a-fg         "#e4e4ef")
       (a-fg+1       "#f4f4ff")
       (a-fg+2       "#f6f6f6")
       (a-white      "#ffffff")
       (a-black      "#000000")
       (a-bg-1       "#030805")
       (a-bg         "#081310") ; #051510
       (a-bg+1       "#223225")
       (a-bg+2       "#3f483f")
       (a-bg+3       "#485548")
       (a-bg+4       "#527570")
       (a-red-2      "#501020")
       (a-red-1      "#c73c3f")
       (a-red        "#f43841")
       (a-red+1      "#ff4f58")
       (a-orange     "#ff6622")
       (a-pink       "#DCA3A3")
       (a-green      "#40b36c")
       (a-yellow-1   "#dbbe97")
       (a-yellow     "#eedd33") ; #ffdd33
       (a-gold       "#ffdd33")
       (a-brown      "#cc8c3c")
       (a-quartz     "#95aca5")
       (a-niagara-2  "#303540")
       (a-niagara-1  "#565f73")
       (a-niagara    "#96a9c9")
       (a-wisteria-1 "#88719d")
       (a-wisteria   "#9e95c7")
       (a-aqua       "aquamarine")

       (a-comment              "#40b36c")  ; #40b3b1
       (a-text                 a-yellow-1)
       (a-doc-string           "#40b36c")
       (a-selection            "#153232")  ; #303838
       (a-type-and-punctuation a-quartz)
       (a-keyword              a-aqua)
       (a-variable             a-yellow-1)
       (a-function             a-yellow-1)
       (a-string               "#27e3e3") ; a-green
       (a-constant             a-quartz)
       (a-preprocessor         a-quartz)
       (a-reference            a-quartz)
       (a-linenum-fg           a-bg+4)
       (a-linenum-current-fg   a-aqua)
       (a-warning              a-orange)
       (a-error                a-red+1)
       (a-success              "#95cc15")
       (a-custom-state         a-green)
       (a-cursor               a-aqua)
       )
  (custom-theme-set-variables
   'actraiser
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'actraiser

   ;; fundamental elements colorscheme
   `(border ((t ,(list :background a-bg
                       :foreground a-bg+2))))
   `(cursor ((t (:background ,a-cursor))))
   `(default ((t ,(list :background a-bg
                        :foreground a-text))))
   `(region ((t (:background ,a-selection :foreground nil))))
   `(fringe ((t ,(list :background nil
                       :foreground a-bg+2))))
   `(vertical-border ((t ,(list :foreground a-bg+2))))
   `(link ((t (:foreground ,a-niagara :underline t))))
   `(link-visited ((t (:foreground ,a-wisteria :underline t))))
   `(match ((t (:background ,a-bg+4))))
   `(shadow ((t (:foreground ,a-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,a-niagara)))) ;; this is what you see when you C-f or M-x or ...
   `(secondary-selection ((t ,(list :background a-selection
                                    :foreground nil))))

   ;; `(trailing-whitespace ((t ,(list :foreground a-black
   ;;                                  :background a-red))))
   `(tooltip ((t ,(list :background a-bg+4
                        :foreground a-white))))
   `(query-replace ((t (:inherit (isearch)))))

   ;; line number & line highlighting
   `(linum ((t ,(list :foreground a-quartz
                      :background a-bg))))
   `(line-number ((t (:inherit default :background ,a-bg :foreground ,a-bg+4))))
   `(line-number-current-line ((t (:inherit default :background ,a-bg :foreground ,a-aqua))))
   `(highlight ((t (:background ,a-red-2 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background a-red-2
                                            :foreground nil))))

   ;; holidays
   `(holiday-face ((t (:foreground ,a-pink))))

   ;; terminal (ansi-term)
   `(term-color-black ((t (:foreground ,a-bg+3 :background ,a-bg+4))))
   `(term-color-red ((t (:foreground ,a-red-1 :background ,a-red-1))))
   `(term-color-green ((t (:foreground ,a-green :background ,a-green))))
   `(term-color-blue ((t (:foreground ,a-niagara :background ,a-niagara))))
   `(term-color-yellow ((t (:foreground ,a-yellow :background ,a-yellow))))
   `(term-color-magenta ((t (:foreground ,a-wisteria :background ,a-wisteria))))
   `(term-color-cyan ((t (:foreground ,a-quartz :background ,a-quartz))))
   `(term-color-white ((t (:foreground ,a-fg :background ,a-white))))

   ;; compilation
   `(compilation-info ((t ,(list :foreground a-success
                                 :background a-bg 
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground a-warning
                                    :background a-bg
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t ,(list :foreground a-error
                                  :background a-bg))))
   `(compilation-error ((t (:foreground ,a-error))))
   `(compilation-mode-line-fail ((t ,(list :foreground a-error
                                           :background a-bg
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground a-success
                                           :background a-bg
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; show paren
   `(show-paren-match-face ((t (:background ,a-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,a-red-1))))

   ;; speedbar
   `(speedbar-directory-face ((t ,(list :foreground a-niagara
                                        :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,a-fg))))
   `(speedbar-highlight-face ((t (:background ,a-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,a-red))))
   `(speedbar-tag-face ((t (:foreground ,a-yellow))))

   ;; completion
   `(completions-annotations ((t (:inherit 'shadow))))

   ;; ido
   `(ido-first-match ((t (:foreground ,a-aqua :bold nil))))
   `(ido-only-match ((t (:foreground ,a-green :weight bold))))
   `(ido-subdir ((t (:foreground ,a-niagara :weight bold))))

   ;; font-lock
   `(font-lock-add-keywords '(c-mode c++-mode)
                            '(("\\(\\w+\\)\\s-*\("
                               (1 font-lock-function-name-face)))
                            t)
   `(font-lock-keyword-face ((t (:foreground ,a-keyword :bold t))))
   `(font-lock-type-face ((t (:foreground ,a-type-and-punctuation))))
   `(font-lock-constant-face ((t (:foreground ,a-constant))))
   `(font-lock-variable-name-face ((t (:foreground ,a-variable))))
   `(font-lock-builtin-face ((t (:foreground ,a-type-and-punctuation))))
   `(font-lock-string-face ((t (:foreground ,a-string))))
   `(font-lock-comment-face ((t (:foreground ,a-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,a-comment))))
   `(font-lock-doc-face ((t (:foreground ,a-doc-string))))
   `(font-lock-doc-string-face ((t (:foreground ,a-doc-string))))
   `(font-lock-function-name-face ((t (:foreground ,a-function))))
   `(font-lock-preprocessor-face ((t (:foreground ,a-preprocessor))))
   `(font-lock-reference-face ((t (:foreground ,a-reference))))
   `(font-lock-warning-face ((t (:foreground ,a-warning))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,a-constant))))

   ;; custom
   `(custom-state ((t (:foreground ,a-custom-state))))

   ;; ;; tree-sitter
   ;; `(tree-sitter-hl-face:method.call ((t (:inherit font-lock-function-name-face))))
   ;; `(tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
   ;; `(tree-sitter-hl-face:operator ((t (:inherit default))))
   ;; `(tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-constant-face))))
   ;; `(tree-sitter-hl-face:number ((t (:inherit font-lock-constant-face))))
   ;; `(tree-sitter-hl-face:variable ((t (:inherit font-lock-variable-name-face))))
   ;; `(tree-sitter-hl-face:keyword ((t (:inherit font-lock-keyword-face))))

   ;; whitespace
   `(whitespace-space ((t ,(list :background a-bg
                                 :foreground a-bg+1))))
   `(whitespace-tab ((t ,(list :background a-bg
                               :foreground a-bg+1))))
   `(whitespace-hspace ((t ,(list :background a-bg
                                  :foreground a-bg+2))))
   `(whitespace-line ((t ,(list :background a-bg
                                :foreground a-red+1))))
   `(whitespace-newline ((t ,(list :background a-bg
                                   :foreground a-bg+2))))
   `(whitespace-trailing ((t ,(list :background a-red+1
                                    :foreground a-red+1))))
   `(whitespace-empty ((t ,(list :background a-niagara-2
                                 :foreground a-niagara-2))))
   `(whitespace-indentation ((t ,(list :background a-bg
                                       :foreground a-bg+1))))
   `(whitespace-space-after-tab ((t ,(list :background a-red+1
                                           :foreground a-red+1))))
   `(whitespace-space-before-tab ((t ,(list :background a-red+1
                                            :foreground a-red+1))))

   ;; mode-line stuff
   `(mode-line ((t ,(list :background a-text
                          :foreground a-bg
                          :box        nil))))
   `(mode-line-inactive ((t ,(list :background a-bg+1
                                   :foreground a-quartz))))
   `(mode-line-buffer-id ((t ,(list :bold t))))

   ;; isearch
   `(isearch ((t ,(list :foreground a-black
                        :background a-pink))))
   `(isearch-fail ((t ,(list :foreground a-black
                             :background a-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                       :foreground a-fg+1
                                       :background a-niagara-1))))

   ;; dired
   `(dired-directory ((t (:foreground ,a-niagara :weight bold))))
   `(dired-symlink ((t (:foreground ,a-pink))))
   `(dired-ignored ((t ,(list :foreground a-quartz
                              :inherit 'unspecified))))

   ;; eshell
   `(eshell-ls-backup ((t (:foreground ,a-quartz))))
   `(eshell-ls-directory ((t (:foreground ,a-niagara))))
   `(eshell-ls-executable ((t (:foreground ,a-green))))
   `(eshell-ls-symlink ((t (:foreground ,a-yellow))))
   )
  )

(font-lock-add-keywords '(c-mode c++-mode)
                        '(("\\(\\w+\\)\\s-*\("
                           (1 font-lock-function-name-face)))
                        t)
(provide-theme 'actraiser)
(load-theme 'actraiser t)



;; Use this to disable `Async-native-compile-log` at startup with Emacs 29+.
(setq native-comp-jit-compilation-deny-list '(".*-loaddefs.el.gz"))

;; Use this to close the message buffer on startup.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Show the clock in the taskbar.
(setq display-time-format "---------- %a, %d %b %y: %I:%M%p")
(setq display-time-default-load-average nil)
(setq display-time-mail-string "")
(display-time)

;; Color for comment tags like @Incomplete @Todo @Note.
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
(modify-face 'font-lock-todo-face         "GreenYellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face         "AntiqueWhite1" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face    "gold" nil nil t nil t nil nil)
(modify-face 'font-lock-incomplete-face   "aquamarine" nil nil t nil t nil nil)
(modify-face 'font-lock-fixme-face        "IndianRed" nil nil t nil t nil nil)

;;
;; My custom keys.
;; For some reason, the definition of 'previous' and 'next' in emacs is wacky.
;;
(define-key global-map "\M-]" 'next-buffer)
(define-key global-map "\M-[" 'previous-buffer)

(define-key global-map "\M-j" 'imenu)

(define-key global-map "\M-0" 'delete-window)
(define-key global-map "\M-1" 'delete-other-windows)
(define-key global-map "\M-2" 'split-window-below)
(define-key global-map "\M-3" 'split-window-right)
(define-key global-map "\M-o" 'other-window)
(define-key global-map "\M-k" 'kill-this-buffer)

;; Disable the emacs exit prompt -- "Really exit Emacs?" -- which is annoying.
(setq confirm-kill-emacs nil)

;; ido mode, which is some interactiveness when you hit M-x.
(rc/require 'smex 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;
;; Highlight the current line.
;;
(global-hl-line-mode 1)
;; If you want to get funky.
;; (set-face-background 'hl-line "midnight blue")
;; (set-face-background 'hl-line "RoyalBlue4")
(set-face-background 'hl-line "#450828")

;; For javascript-mode.
(setq js-indent-level 4)

;; Java mode, C mode, ...
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

(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
;; (add-hook 'java-mode-hook 'rc/set-up-whitespace-handling)

;; display-line-numbers-mode
(when (version <= "26.0.50" emacs-version)
  (global-display-line-numbers-mode 0))

;; multiple cursors.
(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; dired mode.
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

;; Enable word wrap for everything.
(global-word-wrap-whitespace-mode t)

;;; Allow maximum 1 line for the mini-buffer
(setq max-mini-window-height 1)

;;; tramp
;;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
(setq tramp-auto-save-directory "/tmp")

;; glsl mode
(add-to-list 'auto-mode-alist '("\\.gl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glh\\'" . glsl-mode))

;; powershell
(rc/require 'powershell)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))

;;; eldoc mode
(defun rc/turn-on-eldoc-mode ()
  (interactive)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'rc/turn-on-eldoc-mode)

;;
;; Company / autocomplete
;;
(rc/require 'company)
(require 'company)

;; (setq company-dabbrev-char-regexp "\\sw\\|[a-z-_'/]")
(setq company-dabbrev-other-buffers 'all)
(setq company-dabbrev-code-other-buffers (quote all)) ;; irrelevant
(setq company-dabbrev-ignore-buffers "nil")

(add-to-list 'company-backends '(company-dabbrev-code company-dabbrev company-capf))
;; (setq company-dabbrev-downcase nil)
(global-company-mode)

;;; LaTeX mode
(add-hook 'tex-mode-hook
          (lambda ()
            (interactive)
            (add-to-list 'tex-verbatim-environments "code")))

(setq font-latex-fontify-sectioning 'color)

;;
;; Move Text
;;
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;
;; Packages that don't require configuration
;;
(rc/require
 'glsl-mode
 'cmake-mode
)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (defun astyle-buffer (&optional justify)
;;   (interactive)
;;   (let ((saved-line-number (line-number-at-pos)))
;;     (shell-command-on-region
;;      (point-min)
;;      (point-max)
;;      "astyle --style=bsd"
;;      nil
;;      t)
;;     (goto-line saved-line-number)))

(require 'compile)
compilation-error-regexp-alist-alist

(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(display-line-numbers-type 'relative)
;;  '(imenu-auto-rescan t)
;;  '(imenu-auto-rescan-maxout 500000)
;;  '(js-indent-level 2)
;;  '(package-selected-packages
;;    '(rainbow-mode smex powershell php-mode paredit nasm-mode multiple-cursors move-text ido-completing-read+ glsl-mode fixmee company cmake-mode))
;;  '(safe-local-variable-values
;;    '((eval progn
;;            (auto-revert-mode 1)
;;            (rc/autopull-changes)
;;            (add-hook 'after-save-hook 'rc/autocommit-changes nil 'make-it-local))))
;;  '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp)))
;;  '(whitespace-style
;;    '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
