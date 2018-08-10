;; -*- mode: emacs-lisp -*-

(defun select-region (begin end)
  (goto-char begin)
  (set-mark-command nil)
  (goto-char end))

(defun select-sexp-at-point (&optional arg dont-kill)
  (interactive "P")
  (require 'smartparens)
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (b (point-max))
         (e (point)))
    (cond
     ((and raw
           (= n 4))
      (let ((next (sp-get-thing (< arg 0)))
            (enc (sp-get-enclosing-sexp)))
        (if (sp-compare-sexps next enc)
            (when (not dont-kill)
              (let ((del (sp-get-whitespace)))
                (sp-get del (delete-region :beg :end))))
          (if (> arg 0)
              (sp--kill-or-copy-region
               (sp-get next :beg-prf) (sp-get enc :end-in) dont-kill)
            (sp--kill-or-copy-region
             (sp-get next :end) (sp-get enc :beg-in) dont-kill))
          (when (not dont-kill)
            (let ((del (sp-get-whitespace)))
              (sp-get del (delete-region :beg :end)))))))
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (sp--kill-or-copy-region
                     :beg-prf :end dont-kill))))
     ((= n 0)
      (let ((e (sp-get-enclosing-sexp)))
        (when e
          (sp-get e (sp--kill-or-copy-region
                     :beg-in :end-in dont-kill)))))
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
                (select-region (if (> b (point)) (point) b) e))
            (select-region b e))
          (when (not dont-kill)
            (sp--cleanup-after-kill)
            (when (string-match-p "\n" (buffer-substring-no-properties bm (point)))
              (setq sp-last-kill-whitespace
                    (concat sp-last-kill-whitespace
                            (buffer-substring-no-properties bm (point))))
              (select-region bm (point)))
            (when (= 0 sp-successive-kill-preserve-whitespace)
              (kill-append sp-last-kill-whitespace nil)))))))))

(defun clojure-ignore ()
  (interactive)
  (when (not (string-equal (string (following-char)) "(")) ;; TODO {} []
    (paredit-backward-up))
  (insert "#_"))

(defun search-symbol-at-point ()
  (interactive)
  (select-sexp-at-point)
  (helm-projectile-ag))

(defun re-frame-jump-to-reg () ;; https://github.com/oliyh/re-jump.el
  (interactive)

  (require 'cider-util)
  (require 'cider-resolve)
  (require 'cider-client)
  (require 'cider-common)
  (require 'cider-interaction)
  (require 'clojure-mode)
  (let* ((kw (cider-symbol-at-point 'look-back))
         (ns-qualifier (and
                        (string-match "^:+\\(.+\\)/.+$" kw)
                        (match-string 1 kw)))
         (kw-ns (if ns-qualifier
                    (cider-resolve-alias (cider-current-ns) ns-qualifier)
                  (cider-current-ns)))
         (kw-to-find (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw))))

    (when (and ns-qualifier (string= kw-ns (cider-current-ns)))
      (error "Could not resolve alias \"%s\" in %s" ns-qualifier (cider-current-ns)))

    (progn (cider-find-ns "-" kw-ns)
           (search-forward-regexp (concat "[a-zA-Z-]*[ \\\n]+" kw-to-find) nil 'noerror)
           ;;(search-forward-regexp kw-to-find nil 'noerror)
           )))

(defun jump-to-current-version ()
  (interactive)
  (when (fboundp 'projectile-project-root)
    (when-let ((name (buffer-name)))
      (let ((cpoint (point))
            (filename (concat (projectile-project-root)
                              (first (split-string name ".~")))))
        (if (file-exists-p filename)
            (progn
              (find-file filename)
              (goto-char cpoint))
          (message "File doesn't exist: %s" filename))))))

(defun sp-clone-sexp-noindent ()
  (interactive)
  (require 'smartparens)
  (-when-let (ok (let ((sexp (sp-get-thing)))
                   (if (not (equal (sp-get sexp :op) ""))
                       sexp
                     (sp-get-enclosing-sexp))))
    (sp-get ok
      (undo-boundary)
      (if (< :beg-prf (point))
          (save-excursion
            (goto-char :beg-prf)
            (insert-buffer-substring-no-properties
             (current-buffer) :beg-prf :end-suf)
            (newline-and-indent))
        (goto-char :end-suf)
        (save-excursion
          (insert-buffer-substring-no-properties
           (current-buffer) :beg-prf :end-suf))
        (newline-and-indent)))))

(defun new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))
          (insert text))))
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position))))
        (if (> 0 n)
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun delete-space-forward ()
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

(defun delete-space-backward ()
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t\n") (point))))

(defun buffer-modified-title-sign ()
  (if (buffer-modified-p (current-buffer)) "[...]  " ""))

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
    (send-string-to-terminal (concat "\033]1; " (buffer-name) "\007"))))

(defun toggle-comment ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

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

(defun copy-surrounding-sexp ()
  (interactive)
  (let ((cpoint (point)))
    (paredit-backward-up)
    (sp-copy-sexp)
    (goto-char cpoint)))

(defun kill-surrounding-sexp ()
  (interactive)
  (paredit-backward-up)
  (sp-kill-sexp))

(defun copy-whole-buffer ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard!"))

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

  (global-set-key (kbd "M-a")     'mc/mark-all-like-this)
  (global-set-key (kbd "M-w")     'mc/mark-next-like-this)
  (global-set-key (kbd "M-S-<down>") 'mc-next-line)
  (global-set-key (kbd "M-S-<up>") 'mc-prev-line)
  (global-set-key (kbd "M-|")  'mc-cursors-on)
  (global-set-key (kbd "M-\\") 'mc-cursors-off))

;; ---------KEYS IN ESC SEQ------------
;; #   cmd
;; _   shift
;; __  tab
;; *   ctrl
;; +   alt
;; !!  enter
;; %%  delete
;; @>  arrow right
;; @<  arrow left
;; @v  arrow down
;; @^  arrow up
;; ---------KEYS IN ESC SEQ------------

(defun fmnoise/setup-custom-commands ()
  ;; just an example
  (global-set-key (kbd "M-] a") 'neotree-toggle))

(defun fmnoise/setup-mappings ()
  (define-key input-decode-map "\e[1;10A" [S-M-up])
  (define-key input-decode-map "\e[1;10B" [S-M-down])
  (define-key input-decode-map "\e[1;10C" [S-M-right])
  (define-key input-decode-map "\e[1;10D" [S-M-left])
  (define-key input-decode-map "\e[1;9A"  [M-up])
  (define-key input-decode-map "\e[1;9B"  [M-down])
  (define-key input-decode-map "\e[1;9C"  [M-right])
  (define-key input-decode-map "\e[1;9D"  [M-left])
  (define-key input-decode-map "\e[1;8A"  [C-M-up])
  (define-key input-decode-map "\e[1;8B"  [C-M-down])
  (define-key input-decode-map "\e[1;8C"  [C-M-right])
  (define-key input-decode-map "\e[1;8D"  [C-M-left])

  ;; (define-key input-decode-map "\e[1;5A" [C-up])
  ;; (define-key input-decode-map "\e[1;5B" [C-down])
  ;; (define-key input-decode-map "\e[1;5C" [C-right])
  ;; (define-key input-decode-map "\e[1;5D" [C-left])
  ;; (define-key input-decode-map "\e[1;6A" [C-S-up])
  ;; (define-key input-decode-map "\e[1;6B" [C-S-down])
  ;; (define-key input-decode-map "\e[1;6C" [C-S-right])
  ;; (define-key input-decode-map "\e[1;6D" [C-S-left])
  )

(defun fmnoise/setup-editor ()
  ;; crappy menu-bar
  (global-unset-key (kbd "<f10>"))

  ;; basic
  (global-set-key (kbd "M-c")     'copy-region-or-sexp)
  (global-set-key (kbd "M-C")     'copy-surrounding-sexp)
  (global-set-key (kbd "M-# D")   'sp-clone-sexp-noindent)
  (global-set-key (kbd "M-# V")   'paste-sexp-with-replace)
  (global-set-key (kbd "M-# v")   'paste-with-replace)
  (global-set-key (kbd "M-# x")   'kill-region-or-sexp)
  (global-set-key (kbd "M-# X")   'kill-surrounding-sexp)
  (global-set-key (kbd "M-# %%")  (lambda () (interactive) (if (and transient-mark-mode mark-active) (delete-active-region) (paredit-kill))))
  (global-set-key (kbd "M-# a")   'copy-whole-buffer)
  (global-set-key (kbd "M-# A")   'mark-whole-buffer)
  (global-set-key (kbd "M-# z")   (lambda () (interactive) (deactivate-mark) (undo-tree-undo)))
  (global-set-key (kbd "M-# Z")   (lambda () (interactive) (deactivate-mark) (undo-tree-redo)))
  (global-set-key (kbd "M-# s")   'save-buffer) ;; TODO clean selection
  (global-set-key (kbd "M-`")     'keyboard-quit)
  (global-set-key (kbd "M-# /")   'toggle-comment)
  (global-set-key (kbd "M-# }")   'indent-rigidly-right)
  (global-set-key (kbd "M-# {")   'indent-rigidly-left)
  (global-set-key (kbd "M-# d")   'duplicate-line-or-region)
  (global-set-key (kbd "M-# l")   'delete-space-forward)
  (global-set-key (kbd "M-# k")   'delete-space-backward)

  ;; helm
  (global-set-key (kbd "M-# ~")   'helm-buffers-list)
  (global-set-key (kbd "M-# '")   'helm-resume)
  (global-set-key (kbd "C-t")     'helm-themes)

  ;; search
  (require 'ivy)
  (global-set-key (kbd "M-# \"")  'ivy-resume)
  (global-set-key (kbd "M-# S")   'swiper)
  (global-set-key (kbd "M-# +S")  'counsel-projectile-ag)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'kill-this-buffer)
  (global-set-key (kbd "M-# g")   'rgrep)
  (global-set-key (kbd "M-# .")   'search-symbol-at-point)
  (global-set-key (kbd "M-# f")   'helm-do-ag-this-file)
  (global-set-key (kbd "M-# F")   'helm-projectile-ag)
  (global-set-key (kbd "M-# p")   'helm-projectile-find-file)
  (global-set-key (kbd "M-/")     'helm-semantic-or-imenu)
  (global-set-key (kbd "M-?")     'helm-imenu-in-all-buffers)

  ;; tools
  (with-eval-after-load "neotree"
    (define-key neotree-mode-map [(left)] 'neotree-select-up-node)
    (define-key neotree-mode-map [(right)] 'neotree-enter)
    (define-key neotree-mode-map (kbd "TAB") 'neotree-stretch-toggle))
  (global-set-key (kbd "M-# |")   'neotree-toggle)

  ;; windows/buffers management
  (global-set-key (kbd "M-+ __")  'next-multiframe-window)
  (global-set-key (kbd "M-# q")   'kill-buffer-and-window)
  (global-set-key (kbd "M-# w")   'delete-window)
  (global-set-key (kbd "M-# +@v") 'next-buffer)
  (global-set-key (kbd "M-# +@^") 'previous-buffer)
  (global-set-key (kbd "M-z")     'zoom-window-no-color-change)
  (global-set-key (kbd "M-# n")   'new-empty-buffer)
  (global-set-key (kbd "M-# N")   (lambda () (interactive) (new-empty-buffer) (clojure-mode)))

  ;; navigation
  (global-set-key (kbd "M-# @^")  'backward-paragraph)
  (global-set-key (kbd "M-# @v") 'forward-paragraph)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line) ;; TODO off all mc / selection
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "C-M-<up>") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "C-M-<down>") 'scroll-up-line) ;; TODO off all mc / selection
  (global-set-key (kbd "M-# @>") 'end-of-visual-line)
  (global-set-key (kbd "M-# @<") 'beginning-of-line-text)
  (global-set-key (kbd "M-# _@<")  'backward-word)
  (global-set-key (kbd "M-# _@>")  'forward-word)
  (global-set-key (kbd "M-# *@<")  'backward-sentence) ;; UNUSED
  (global-set-key (kbd "M-# *@>")  'forward-sentence) ;; UNUSED
  (global-set-key (kbd "C-g")     'goto-line)
  (global-set-key (kbd "M-# j")   'bookmark-set)
  (global-set-key (kbd "M-# J")   'bookmark-bmenu-list)

  ;; hide/show
  (global-set-key (kbd "C-\\") 'hs-toggle-hiding)
  (global-set-key (kbd "M-F h")   'hs-hide-all)
  (global-set-key (kbd "M-F s")   'hs-show-all)

  ;; UNUSED
  (global-set-key (kbd "M-l")     'linum-mode)
  (global-set-key (kbd "M-;")     'indent-guide-mode)
  (global-set-key (kbd "M-# m")   (lambda () (interactive) (switch-to-buffer (messages-buffer))))

  ;; TODO
  ;; - setup window size manipulation: option + ctrl + arrow
  ;; - setup bookmarks
  ;; fn+ctrl+shift+alt+arrow(Home End PgUp PgDown) for navigation
  ;; good hotkeys caps(=alt)+shift + a z / s x / w e
  )

 ;;UNUSED
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

(defun add-reframe-regs-to-imenu ()
  (add-to-list
   'imenu-generic-expression
   '("re-frame" "(*reg-\\(event-db\\|sub\\|fx\\|event-fx\\|cofx\\)[ \n]+\\([^\t \n]+\\)" 2)
   t))

(defun clojure-imenu-index ()
  (list (cons "a" (copy-marker 1))
        (cons "b" (copy-marker 2))))

(defun set-clojure-imenu-expression ()
  (setq
   imenu-generic-expression
   '((nil "^\\s-*(\\(?:s\\|t/\\)?def[a-z]-?+[[:space:]\n]+\\(?:\\(?:\\^{[^}]+}[[:space:]\n]+\\)\\|\\(?:\\^:[^[:space:]\n]+\\s-+\\)\\)?\\([^[:space:]\n\)]+\\)" 1)
     ;;(nil "^\\s-*(\\(?:s\\|t/\\)?def\\(?!\\(method\\s\\|multi\\s\\|once\\s\\|macro\\s\\|record\\s\\|type\\s\\|interface\\s\\|protocol\\s\\)\\)[a-z]+[[:space:]\n]+\\(?:\\(?:\\^{[^}]+}[[:space:]\n]+\\)\\|\\(?:\\^:[^[:space:]\n]+\\s-+\\)\\)?\\([^[:space:]\n\)]+\\)" 1)
     ("var""^\\s-*(\\(?:s\\|t/\\)?def[[:space:]\n]+\\(?:\\(?:\\^{[^}]+}[[:space:]\n]+\\)\\|\\(?:\\^:[^[:space:]\n]+\\s-+\\)\\)?\\([^[:space:]\n\)]+\\)" 1)
     ("var""^\\s-*(\\(?:s\\|t/\\)?defonce[[:space:]\n]+\\(?:\\(?:\\^{[^}]+}[[:space:]\n]+\\)\\|\\(?:\\^:[^[:space:]\n]+\\s-+\\)\\)?\\([^[:space:]\n\)]+\\)" 1)
     ("macro" "^\\s-*(defmacro\\s-+\\([^[:space:]\n]+\\)" 1)
     ("record" "^\\s-*(\\(?:s/\\)?defrecord\\s-+\\([^[:space:]\n]+\\)" 1)
     ("type" "^\\s-*(deftype\\+?\\s-+\\([^[:space:]\n]+\\)" 1)
     ("protocol" "^\\s-*(\\(?:def\\(?:-abstract-type\\|interface\\+?\\|protocol\\)\\)\\s-+\\([^[:space:]\n]+\\)" 1)
     ("multi" "^\\s-*(defmulti\\s-+\\([^[:space:]\n]+\\)" 1)
     ("method" "^\\s-*(defmethod\\s-+\\([^[:space:]\n]+\\s-+[^[:space:]\n]+\\)" 1)
     ))
  (setq imenu-create-index-function 'imenu-default-create-index-function))

(defun fmnoise/setup-lisp ()
  (setq cljr-warn-on-eval nil)
  (setq imenu-auto-rescan t)

  (define-key emacs-lisp-mode-map (kbd "M-# !!") 'spacemacs/eval-current-form-sp)

  (require 'expand-region)
  (global-set-key (kbd "M-e") 'er/expand-region)
  (global-set-key (kbd "M-o") 'er/mark-outside-pairs)

  (global-set-key (kbd "M-p r") 'paredit-raise-sexp)
  (global-set-key (kbd "M-p j") 'paredit-join-sexps)
  (global-set-key (kbd "M-p s") 'paredit-splice-sexps)
  (global-set-key (kbd "M-p [") 'paredit-wrap-square)
  (global-set-key (kbd "M-p (") 'paredit-wrap-round)
  (global-set-key (kbd "M-p {") 'paredit-wrap-curly)
  (global-set-key (kbd "M-p o") 'er/mark-outside-pairs)
  (global-set-key (kbd "M-p i") 'er/mark-inside-pairs)
  (global-set-key (kbd "M-p e") 'er/expand-region)

  (setq cider-repl-display-in-current-window t)
  (setq cider-eval-result-duration 30)
  (setq cider-cljs-lein-repl
        "(do
         (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

  (add-hook 'clojure-mode-hook #'paredit-mode)
  ;; (add-hook 'clojure-mode-hook #'set-clojure-imenu-expression)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojurescript-mode-hook #'add-reframe-regs-to-imenu)

  (require 'paredit)
  (define-key paredit-mode-map (kbd "M-<up>")    'paredit-backward-up)
  (define-key paredit-mode-map (kbd "M-<down>")  'paredit-forward-down)
  (define-key paredit-mode-map (kbd "M-<left>")  'paredit-backward)
  (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward)

  (define-key paredit-mode-map (kbd "M-k") 'paredit-kill)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-9") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-0") 'paredit-close-round)
  (define-key paredit-mode-map (kbd "M-# +[") 'paredit-wrap-square) ;; can't use simple M-[ due to crashing escape seq handling

  (require 'cider-inspector)
  (define-key cider-inspector-mode-map (kbd "M-<left>") 'cider-inspector-prev-page)
  (define-key cider-inspector-mode-map (kbd "M-<right>")'cider-inspector-next-page)
  (define-key cider-inspector-mode-map (kbd "M-<up>") 'cider-inspector-pop)
  (define-key cider-inspector-mode-map (kbd "M-<down>") 'cider-inspector-operate-on-point)

  (require 'clojure-mode)
  (define-key clojure-mode-map (kbd "M-# r") 'hydra-cljr-help-menu/body)
  (define-key clojure-mode-map (kbd "M-# */") 'clojure-ignore)

  (define-key clojure-mode-map (kbd "M-RET e p") 'cider-eval-sexp-at-point)
  (define-key clojure-mode-map (kbd "M-RET e t") 'cider-eval-toplevel-sexp)
  (define-key clojure-mode-map (kbd "M-RET s X") 'cider-restart)
  (define-key clojure-mode-map (kbd "M-RET s j") 'cider-create-sibling-cljs-repl)

  (define-key clojure-mode-map (kbd "M-# *!!")  'cider-eval-buffer)
  (define-key clojure-mode-map (kbd "M-# #_!!") 'cider-eval-defun-to-comment)
  (define-key clojure-mode-map (kbd "M-# !!")   'cider-eval-toplevel-sexp)
  (define-key clojure-mode-map (kbd "M-# _!!")  'cider-eval-sexp-at-point) ;; TODO - good combination

  (define-key clojure-mode-map (kbd "M-i") 'cider-inspect-last-result)
  (define-key clojure-mode-map (kbd "M->") 're-frame-jump-to-reg)

  ;; TODO
  ;; - setup cljr, hydra-cljr keys
  ;; - setup clojure indentation
  )

(defun toggle-magit-status ()
  (interactive)
  (if-let ((repo (magit-toplevel)))
      (if-let ((buf (get-buffer (concat "magit: " (car (last (split-string-and-unquote repo "/")))))))
          (if (get-buffer-window buf)
              (progn (delete-windows-on buf) (kill-buffer buf))
            (magit-status))
        (magit-status))
    (message "Not a git repo!")))

(defun fmnoise/setup-git ()
  (require 'magit)
  (setq github-browse-file-show-line-at-point 1)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (global-set-key (kbd "M-# b") 'magit-blame)
  (global-set-key (kbd "M-'")   'toggle-magit-status)
  (global-set-key (kbd "M-G")   'github-browse-file)
  (global-set-key (kbd "M-Z")   'magit-diff-buffer-file) ;;???
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
  (fmnoise/setup-mappings)
  (fmnoise/setup-company)
  (fmnoise/setup-term)
  (fmnoise/setup-git)
  (fmnoise/setup-clipboard)
  (fmnoise/setup-mc)
  (fmnoise/setup-editor)
  (fmnoise/setup-extensions)
  (fmnoise/setup-lisp)
  )

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default configuration-layer--elpa-archives
                '(("melpa-stable" . "https://stable.melpa.org/packages/")))
  (setq-default package-archives configuration-layer--elpa-archives)

  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     yaml
     ;; sql
     javascript
     markdown
     clojure
     helm
     emacs-lisp
     react
     )
   dotspacemacs-additional-packages '(company
                                      ag
                                      rainbow-mode
                                      zoom-window
                                      magit
                                      swiper
                                      counsel
                                      counsel-projectile
                                      color-theme-sanityinc-tomorrow
                                      planet-theme
                                      command-log-mode
                                      material-theme
                                      clojure-cheatsheet
                                      dockerfile-mode
                                      bm
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
                                    coffee-mode
                                    slim-mode
                                    aggressive-indent
                                    emmet-mode
                                    haml-mode
                                    pug-mode
                                    sass-mode
                                    scss-mode
                                    less-css-mode
                                    fancy-battery-mode
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
    (ag bm counsel-projectile counsel swiper-helm swiper ivy command-log-mode org-plus-contrib projectile cider dockerfile-mode ujelly-theme darkburn-theme badger-theme base16-theme discover web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode plain-theme anti-zenburn-theme flatui-theme perspective sql-indent js-comint yaml-mode github-modern-theme github-theme paxedit autumn-light-theme flycheck-clojure flycheck skewer-mode simple-httpd json-snatcher json-reformat js2-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode anaconda-mode pythonic clojure-cheatsheet rainbow-mode packed magit magit-popup git-commit with-editor evil goto-chg github-browse-file zoom-window persistent-scratch helm-clojuredocs planet-theme plan9-theme color-theme-sanityinc-tomorrow company-quickhelp ac-cider helm-cider magithub mmm-mode markdown-toc markdown-mode gh-md simpleclip helm-fuzzy-find company define-word zenburn-theme ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smeargle reveal-in-osx-finder restart-emacs rainbow-delimiters popwin persp-mode pcre2el pbcopy parinfer paradox osx-trash osx-dictionary orgit org-bullets open-junk-file neotree move-text material-theme magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint launchctl json-mode js2-refactor js-doc info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-ag google-translate golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu elisp-slime-nav dumb-jump column-enforce-mode coffee-mode clj-refactor clean-aindent-mode cider-eval-sexp-fu auto-highlight-symbol auto-compile apropospriate-theme aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
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
