;; -*- mode: emacs-lisp -*-

(defun level-up-block ()
  (interactive)
  (let ((cur-point (point))
        (new-point (hs-find-block-beginning)))
    (when (or (equal cur-point new-point)
              (not new-point))
      (progn
        (goto-char (decf cur-point))
        (hs-find-block-beginning)))))

(defun buffer-modified-title-sign ()
  (if (buffer-modified-p (current-buffer))
      "[~]  "
    ""))

(defun current-branch-name ()
  (require 'magit)
  (if-let (branch (magit-git-string "status"))
    (concat "  .:.  " (car (last (split-string branch " "))))))

(defun xterm-title-update ()
  ;; https://www.emacswiki.org/emacs/FrameTitle
  "Sets buffer file name or buffer name as terminal title"
  (interactive)
  (require 'projectile)

  (if buffer-file-name
    (send-string-to-terminal (concat "\033]2; " (buffer-modified-title-sign) (buffer-file-name) (current-branch-name) "\007"))
    (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007")))

  (if (not (equal (projectile-project-name) "-"))
    (send-string-to-terminal (concat "\033]1; (" (projectile-project-name) ") \007"))
    (send-string-to-terminal (concat "\033]1; " (buffer-name) "\007")))
)

(defun toggle-ag-hidden-search ()
  (interactive)
  (if (and (boundp 'helm-ag-command-option)
           (symbol-value 'helm-ag-command-option))
    (progn
      (setq helm-ag-command-option nil)
      (message "Search all files OFF" ))
    (progn
      (setq helm-ag-command-option "-u")
      (message "Search all files ON" ))))

(defun toggle-comment ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun select-current-line ()
  "Select current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun zoom-window-no-color-change ()
  (interactive)
  (require 'zoom-window)
  (let ((enabled (zoom-window--enable-p))
        (curframe (window-frame nil)))
    (if (and (one-window-p) (not enabled))
        (message "There is only one window!!")
      (if enabled
          (with-demoted-errors "Warning: %S"
            (zoom-window--do-unzoom))
        (zoom-window--save-mode-line-color)
        (zoom-window--save-buffers)
        (zoom-window--save-window-configuration)
        (delete-other-windows))
      ;;(force-mode-line-update)
      (zoom-window--toggle-enabled))))

(defun paste-with-replace ()
  "Deletes selected region and pastes"
  (interactive)
  (when (and transient-mark-mode mark-active)
    (delete-active-region))
  (yank))

(defun sp-delete-sexp (&optional arg)
  ;;(require 'smartparens)
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (b (point-max))
         (e (point)))
    (cond
     ;; kill to the end or beginning of list
     ((and raw
           (= n 4))
      (let ((next (sp-get-thing (< arg 0)))
            (enc (sp-get-enclosing-sexp)))
        (if (sp-compare-sexps next enc)
              (let ((del (sp-get-whitespace)))
                (sp-get del (delete-region :beg :end)))
          (if (> arg 0)
              (delete-region
               (sp-get next :beg-prf) (sp-get enc :end-in))
            (delete-region
             (sp-get next :end) (sp-get enc :beg-in)))
          (let ((del (sp-get-whitespace)))
              (sp-get del (delete-region :beg :end))))))
     ;; kill the enclosing list
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (delete-region
                     :beg-prf :end))))
     ;; kill inside of sexp
     ((= n 0)
      (let ((e (sp-get-enclosing-sexp)))
        (when e
          (sp-get e (delete-region
                     :beg-in :end-in)))))
     ;; regular kill
     (t
      (save-excursion
        (while (and (> n 0) ok)
          (setq ok (sp-forward-sexp (sp--signum arg)))
          (sp-get ok
            (when (< :beg-prf b) (setq b :beg-prf))
            (when (> :end e) (setq e :end)))
          (setq n (1- n))))
      (when ok
        (let ((bm (set-marker (make-marker) b)))
          (if (eq last-command 'kill-region)
              (progn
                (when (member sp-successive-kill-preserve-whitespace '(1 2))
                  (kill-append sp-last-kill-whitespace nil))
                (delete-region
                 (if (> b (point)) (point) b) e))
            (delete-region b e))
          ;; kill useless junk whitespace, but only if we're actually
          ;; killing the region
          (sp--cleanup-after-kill)
          ;; kill useless newlines
          (when (string-match-p "\n" (buffer-substring-no-properties bm (point)))
            (setq sp-last-kill-whitespace
                  (concat sp-last-kill-whitespace
                          (buffer-substring-no-properties bm (point))))
            (delete-region bm (point)))
          (when (= 0 sp-successive-kill-preserve-whitespace)
            (kill-append sp-last-kill-whitespace nil))))))))

(defun paste-sexp-with-replace ()
  "Deletes selected sexp and pastes previously copied one"
  (interactive)
  (sp-delete-sexp)
  (yank))

(defun copy-region-or-sexp ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-ring-save (region-beginning) (region-end))
    (sp-copy-sexp)))

(defun kill-region-or-sexp ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (sp-kill-sexp)))

(defun copy-whole-buffer ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard!")
  )

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun fmnoise/setup-clipboard ()
  (defun copy-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun paste-from-osx ()
    (shell-command-to-string "pbpaste"))

  (setq interprogram-cut-function 'copy-to-osx)
  (setq interprogram-paste-function 'paste-from-osx))

(defun fmnoise/setup-mc ()
  (require 'multiple-cursors)

  (defun mc-mark-line (direction)
   (mc/mark-lines 1 direction))

  (defun mc-cursors-on ()
    (interactive)
    (mc/maybe-multiple-cursors-mode))

  (defun mc-cursors-off ()
    (interactive)
    (mc/keyboard-quit))

  (defun mc-next-line ()
    (interactive)
    (mc-mark-line 'forwards))

  (defun mc-prev-line ()
    (interactive)
    (mc-mark-line 'backwards))

  (global-set-key (kbd "M-+ _@v") 'mc-next-line)
  (global-set-key (kbd "M-+ _@^") 'mc-prev-line)
  (global-set-key (kbd "M-+ _|") 'mc-cursors-on)
  (global-set-key (kbd "M-|") 'mc-cursors-off))

;; #   cmd
;; _   shift
;; *   ctrl (when used with additional keys)
;; +   alt (when used with additional keys)
;; !!  enter
;; @>  arrow right
;; @<  arrow left
;; @v  arrow down
;; @^  arrow up

(defun fmnoise/setup-custom-commands ()
  ;; just an example
  (global-set-key (kbd "M-] a") 'neotree-toggle)
  )

(defun fmnoise/setup-editor ()
  (global-set-key (kbd "M-c")     'copy-region-or-sexp)
  ;; (global-set-key (kbd "TAB")     'self-insert-command)

  (global-set-key (kbd "M-# |")   'neotree-toggle)
  (global-set-key (kbd "M-# ~")   'helm-buffers-list)
  (global-set-key (kbd "M-# z")   (lambda () (interactive) (deactivate-mark) (undo)))
  (global-set-key (kbd "M-# n")   'spacemacs/new-empty-buffer)
  (global-set-key (kbd "M-# g")   'rgrep)
  (global-set-key (kbd "M-# d")   'spacemacs/duplicate-line-or-region)
  (global-set-key (kbd "M-# _d")  'sp-clone-sexp)
  (global-set-key (kbd "M-# _v")  'paste-sexp-with-replace)
  (global-set-key (kbd "M-# '")   'helm-resume)
  (global-set-key (kbd "M-# x")   'kill-region-or-sexp)
  (global-set-key (kbd "M-# /")   'toggle-comment)
  (global-set-key (kbd "M-# _@>") 'select-current-line)
  (global-set-key (kbd "M-# _@<") 'select-current-line)
  (global-set-key (kbd "M-# p")   'helm-projectile-find-file)
  (global-set-key (kbd "M-# +@v") 'next-buffer)
  (global-set-key (kbd "M-# +@^") 'previous-buffer)
  (global-set-key (kbd "M-# v")   'paste-with-replace)
  (global-set-key (kbd "M-# f")   'helm-do-ag-this-file)
  (global-set-key (kbd "M-# F")   'helm-projectile-ag)
  (global-set-key (kbd "M-# q")   'kill-buffer-and-window)
  (global-set-key (kbd "M-# }")   'indent-rigidly-right)
  (global-set-key (kbd "M-# {")   'indent-rigidly-left)
  (global-set-key (kbd "M-# a")   'copy-whole-buffer)
  (global-set-key (kbd "M-# _a")  'mark-whole-buffer)
  (global-set-key (kbd "M-# b")   'magit-blame)
  (global-set-key (kbd "M-# s")   'save-buffer) ;; TODO clean selection
  (global-set-key (kbd "M-+ __")  'next-multiframe-window)
  (global-set-key (kbd "M-# @>")  'end-of-visual-line)
  (global-set-key (kbd "M-# @<")  'beginning-of-line-text)

  (global-set-key (kbd "M-/")     'helm-semantic-or-imenu)
  (global-set-key (kbd "M-?")     'helm-imenu-in-all-buffers)
  (global-set-key (kbd "M-z")     'zoom-window-no-color-change)

  ;; blocks navigation
  (global-set-key (kbd "M-+ @<")  'level-up-block)
  (global-set-key (kbd "M-+ @v")  'sp-beginning-of-next-sexp) ;; hs-show-block
  (global-set-key (kbd "M-+ @^")  'sp-beginning-of-previous-sexp) ;; hs-hide-block
  (global-set-key (kbd "M-+ @>")  'sp-down-sexp) ;; 'hs-toggle-hiding

  ;; hide/show
  (global-set-key (kbd "M-* +@<") 'hs-hide-block)
  (global-set-key (kbd "M-* +@>") 'hs-show-block)
  (global-set-key (kbd "M-F h")   'hs-hide-all)
  (global-set-key (kbd "M-F s")   'hs-show-all)

  ;; TODO - move away from arrow keys
  ;; (global-set-key (kbd "M-w")     'previous-line)
  ;; (global-set-key (kbd "M-a")     'left-char)
  ;; (global-set-key (kbd "M-s")     'next-line)
  ;; (global-set-key (kbd "M-d")     'right-char)

  ;; ??? UNUSED/RARE
  (global-set-key (kbd "M-r")     'replace-string)
  (global-set-key (kbd "M-R")     'replace-regexp)
  (global-set-key (kbd "M-l")     'linum-mode)
  (global-set-key (kbd "M-;")     'indent-guide-mode)
  (global-set-key (kbd "C-a")     'toggle-ag-hidden-search)

  (global-set-key (kbd "M-`")     'keyboard-quit)
  (global-set-key (kbd "M-a")     'mc/mark-all-like-this)
  (global-set-key (kbd "M-w")     'mc/mark-next-like-this)
  (global-set-key (kbd "M-q")     'delete-window)
  (global-set-key (kbd "M-Q")     'kill-this-buffer)
  (global-set-key (kbd "M-h")     'highlight-regexp)
  (global-set-key (kbd "M-H")     'unhighlight-regexp)

  (global-set-key (kbd "C-g")     'goto-line)
  (global-set-key (kbd "C-t")     'helm-themes)

  ;; scrolling
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line) ;; TODO off all mc / selection
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "M-+ *@^") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "M-+ *@v") 'scroll-up-line) ;; TODO off all mc / selection

  (global-set-key (kbd "M-# @^")  'backward-paragraph)
  (global-set-key (kbd "M-# @v") 'forward-paragraph)

  ;; UNUSED
  (global-set-key (kbd "M-# m")   (lambda () (interactive) (switch-to-buffer (messages-buffer))))
  (global-set-key (kbd "<f12>") 'spacemacs/find-dotfile)
  (global-set-key (kbd "<f5>") 'spacemacs/copy-file)
  (global-set-key (kbd "<f6>") 'spacemacs/rename-file)
  (global-unset-key (kbd "<f10>"))

  ;; todo - setup window size manipulation: option + ctrl + arrow
  ;; todo - setup bookmarks

  ;; todo - good hotkeys
  ;; M-n new?
  ;; M-d delete?
  ;; M-j jump?
  ;; M-k kill?

  ;; todo - fn+ctrl+shift+alt+arrow(Home End PgUp PgDown) for navigation
  ;; todo - good hotkeys caps(=alt)+shift + a z / s x / w e

  (with-eval-after-load "neotree"
    (define-key neotree-mode-map [(left)] 'neotree-select-up-node)
    (define-key neotree-mode-map [(right)] 'neotree-enter)
    (define-key neotree-mode-map (kbd "TAB") 'neotree-stretch-toggle)))

(defun fmnoise/setup-term ()
  (global-set-key (kbd "M-t") 'term)
  (with-eval-after-load "term"
    (define-key term-raw-map (kbd "M-x") 'helm-M-x)
    (define-key term-raw-map (kbd "M-z") 'zoom-window-zoom)
    (define-key term-raw-map (kbd "M-q") 'delete-window)
    (define-key term-raw-map (kbd "M-`") 'term-line-mode)
    (define-key term-mode-map (kbd "M-`") 'term-char-mode)))

(defun cider-eval-toplevel-sexp ()
  ;; requires smartparens to work
  (interactive)
  (let ((curpoint (point)))
    (when (< (point) (line-end-position))
      (goto-char (incf (point))))
    (setq deepness 0)
    (while (and
            (> (point) (line-beginning-position))
            (< (incf deepness) 50))
      (goto-char (decf (point)))
      (ignore-errors (sp-beginning-of-sexp)))
    (cider-eval-sexp-at-point)
    (goto-char curpoint)))

(defun fmnoise/setup-clojure ()
  (global-set-key (kbd "M-# #_!!") 'cider-eval-defun-to-comment)
  (global-set-key (kbd "M-# !!") 'cider-eval-toplevel-sexp)
  (global-set-key (kbd "M-# _!!") 'cider-eval-region) ;; TODO - good combination

  (global-set-key (kbd "M-i") 'cider-inspect-last-result)
  (global-set-key (kbd "M-d") 'cider-doc)

  ;; UNUSED
  (global-set-key (kbd "M-# r") 'cider-restart)
  (global-set-key (kbd "M-# l") 'cider-jack-in)
  (global-set-key (kbd "M-# k") 'cider-quit)

  (global-set-key (kbd "M-# )") (lambda () (interactive) (parinfer-indent)))
  (global-set-key (kbd "M-P") (lambda () (interactive) (parinfer--switch-to-paren-mode)))
  (global-set-key (kbd "M-I") (lambda () (interactive) (parinfer--switch-to-indent-mode)))
  (global-set-key (kbd "M-# *!!") 'spacemacs/eval-current-form-sp) ;; this is about elisp
  ;; TODO
  ;; - er/mark-outside-pairs
  ;; - er/expand-region
  ;; - paredit-raise-sexp
  ;; - paredit-wrap-round
  ;; - paredit-wrap-square
  ;; - paredit-wrap-curly

  (setq cider-repl-display-in-current-window t)
  (setq cider-eval-result-duration 30)
  (setq cider-cljs-lein-repl
        "(do
           (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  ;; (add-hook 'cider-mode-hook #'parinfer-mode)

  (require 'cider-inspector)
  (define-key cider-inspector-mode-map (kbd "M-+ @<") 'cider-inspector-prev-page)
  (define-key cider-inspector-mode-map (kbd "M-+ @>") 'cider-inspector-next-page)
  (define-key cider-inspector-mode-map (kbd "M-+ @^") 'cider-inspector-pop)
  (define-key cider-inspector-mode-map (kbd "M-+ @v") 'cider-inspector-operate-on-point)

  (require 'clojure-mode)
  ;; (define-key clojure-mode-map (kbd "TAB") 'self-insert-command)
  (define-key clojure-mode-map (kbd "M-# *!!") 'cider-eval-buffer)

  ;; (setq clojure-indent-style :align-arguments)
  ;; (put-clojure-indent 'lete 1)
  ;; TODO setup clojure indentation
  )

(defun toggle-magit-status ()
  (interactive)
  (if-let ((repo (magit-toplevel)))
      (if-let ((buf (get-buffer (concat "*magit: " (car (last (split-string-and-unquote repo "/")))))))
          (if (get-buffer-window buf)
              (progn (delete-windows-on buf) (kill-buffer buf))
            (magit-status))
        (magit-status))
    (message "Not a git repo!"))
  )

(defun fmnoise/setup-git ()
  (global-set-key (kbd "M-'") 'toggle-magit-status)
  (global-set-key (kbd "M-G") 'github-browse-file)
  (global-set-key (kbd "M-Z") 'magit-diff-buffer-file)

  (setq github-browse-file-show-line-at-point 1)
  )

(defun fmnoise/setup-company ()
  (with-eval-after-load 'company
    (define-key company-active-map [left] #'company-abort)))

(defun fmnoise/setup-extensions ()
  (add-to-list 'auto-mode-alist '("\\.html\\'" . sgml-mode))

  (require 'hideshow)
  (require 'sgml-mode)

  (add-to-list 'hs-special-modes-alist
               '(sgml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))

  (add-hook 'sgml-mode-hook 'hs-minor-mode)
  )

(defun fmnoise/setup ()
  ;; vars
  (setq indent-tabs-mode nil)
  (setq dotspacemacs-mode-line-unicode-symbols nil)
  (setq powerline-default-separator nil)
  (setq company-auto-complete t)
  (setq recentf-save-file (format "/tmp/recentf.%s" (emacs-pid))) ;; https://github.com/syl20bnr/spacemacs/issues/5186
  ;; don't add newline
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
  ;; don't make autoident on newline
  (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

  ;; (setq split-height-threshold nil)
  ;; (setq split-width-threshold 0)

  ;; modes
  (global-company-mode 1)
  (global-eldoc-mode 1)
  (delete-selection-mode 1)
  (savehist-mode 0)
  ;; hooks
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook
            (lambda () (if (not indent-tabs-mode)
                           (untabify (point-min) (point-max)))
              nil))
  (add-hook 'post-command-hook 'xterm-title-update)

  ;; setup calls
  (fmnoise/setup-company)
  (fmnoise/setup-term)
  (fmnoise/setup-git)
  (fmnoise/setup-clipboard)
  (fmnoise/setup-mc)
  (fmnoise/setup-editor)
  ;;(fmnoise/setup-custom-commands)
  (fmnoise/setup-extensions)
  (fmnoise/setup-clojure))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     sql
     javascript
     markdown
     clojure
     helm
     emacs-lisp
     react
     )
   dotspacemacs-additional-packages '(company
                                      rainbow-mode
                                      parinfer
                                      zoom-window
                                      magit
                                      color-theme-sanityinc-tomorrow
                                      material-theme
                                      clojure-cheatsheet
                                      dockerfile-mode
                                      badger-theme ;; needs tuning
                                      github-browse-file)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(evil-visualstar
                                    evil-visual-mark-mode
                                    evil-unimpaired
                                    evil-tutor
                                    evil-surround
                                    evil-search-highlight-persist
                                    evil-numbers
                                    evil-nerd-commenter
                                    evil-mc
                                    evil-matchit
                                    evil-magit
                                    evil-lisp-state
                                    evil-indent-plus
                                    evil-iedit-state
                                    evil-exchange
                                    evil-escape
                                    evil-ediff
                                    evil-args
                                    anaconda-mode
                                    web-mode
                                    evil-anzu)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists nil
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'clojure-mode
   dotspacemacs-themes '(zenburn
                         leuven-yellow
                         material-light
                         planet
                         )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 25
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ())

(defun dotspacemacs/user-config () (fmnoise/setup))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(evil-want-Y-yank-to-eol nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (org-plus-contrib projectile cider dockerfile-mode ujelly-theme darkburn-theme badger-theme base16-theme discover web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode plain-theme anti-zenburn-theme flatui-theme perspective sql-indent js-comint yaml-mode github-modern-theme github-theme paxedit autumn-light-theme flycheck-clojure flycheck skewer-mode simple-httpd json-snatcher json-reformat js2-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode anaconda-mode pythonic clojure-cheatsheet rainbow-mode packed magit magit-popup git-commit with-editor evil goto-chg github-browse-file zoom-window persistent-scratch helm-clojuredocs planet-theme plan9-theme color-theme-sanityinc-tomorrow company-quickhelp ac-cider helm-cider magithub mmm-mode markdown-toc markdown-mode gh-md simpleclip helm-fuzzy-find company define-word zenburn-theme ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smeargle reveal-in-osx-finder restart-emacs rainbow-delimiters popwin persp-mode pcre2el pbcopy parinfer paradox osx-trash osx-dictionary orgit org-bullets open-junk-file neotree move-text material-theme magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint launchctl json-mode js2-refactor js-doc info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-ag google-translate golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu elisp-slime-nav dumb-jump column-enforce-mode coffee-mode clj-refactor clean-aindent-mode cider-eval-sexp-fu auto-highlight-symbol auto-compile apropospriate-theme aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
