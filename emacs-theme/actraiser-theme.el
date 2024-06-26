(deftheme actraiser
  "Dark-themed colorscheme for Emacs 29")

(unless (>= emacs-major-version 27)
  (error "The actraiser theme requires Emacs 27 or later!"))

(let* ((a-fg         "#e4e4ef")
       (a-fg+1       "#f4f4ff")
       (a-fg+2       "#f6f6f6")
       (a-white      "#ffffff")
       (a-black      "#000000")
       (a-bg-1       "#030805")
       (a-bg         "#051510") ; #081810
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
       (a-selection            "#303838")
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

   ;; tree-sitter
   `(tree-sitter-hl-face:method.call ((t (:inherit font-lock-function-name-face))))
   `(tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
   `(tree-sitter-hl-face:operator ((t (:inherit default))))
   `(tree-sitter-hl-face:constant.builtin ((t (:inherit font-lock-constant-face))))
   `(tree-sitter-hl-face:number ((t (:inherit font-lock-constant-face))))
   `(tree-sitter-hl-face:variable ((t (:inherit font-lock-variable-name-face))))
   `(tree-sitter-hl-face:keyword ((t (:inherit font-lock-keyword-face))))

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
