;;; Package --- My minimalistic init

;;; Commentary:
;;; This minimalistic init loads my real configuration from OneDrive stored in
;;; an org file, and saves customizations in custom-vars.el in the
;;; user-emacs-directory folder.

;;* Code:
(message "running my init.el")

;; Set the custom-file variable if it is nil
;; If you want to override the location of the custom-vars.el location
;; then put in code in early-init.el
(if (not custom-file)
    (setq custom-file
          (locate-user-emacs-file
           (expand-file-name "custom-vars.el" user-emacs-directory))))

;;* Initialize package manager
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ox-odt" . "https://kjambunathan.github.io/elpa/") t)  ;; for ox-odt
;(setq package-gnupghome-dir (expand-file-name "elpa/gnupg" user-emacs-directory))
(setq package-gnupghome-dir nil)
(package-initialize)

;; fetch the list of packages available 
(or (file-exists-p package-user-dir)
    (package-refresh-contents)) 

;; install the missing packages
;; list the packages you want
;; (defvar package-list "temporary variable for managing package list")
;; (setq package-list '(use-package))
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))
;; (unless (package-installed-p 'vc-use-package)
;;   (package-vc-install "https://github.com/slotThe/vc-use-package"))
;; (require 'vc-use-package)

;;* sample code
;; code snippet for how to add keys to a mode
;; this is done in a hook
;; (add-hook 'texinfo-mode-hook
;;           (lambda ()
;;             (define-key texinfo-mode-map "\C-cp"
;;                         'backward-paragraph)
;;             (define-key texinfo-mode-map "\C-cn"
;;                         'forward-paragraph)))
;;             (define-key texinfo-mode-map "\C-c\C-xx" nil)

;;* plugins initialization
;; Initialize load-path to include all subdirectories under plugins
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))

;;* Update the path to include plugins
(let* ((plugins-folder (concat (expand-file-name user-emacs-directory) "plugins")))
  ;; (message plugins-folder)             
  (setenv "PATH" (concat (getenv "PATH") ":" plugins-folder))
  ;; (message (getenv "PATH"))
  (setq exec-path (append exec-path (list plugins-folder))))

;;* use-package
;; for profiling use-package
;; to see the report use Alt-X use-package-report
(setq use-package-compute-statistics t)
(eval-when-compile (require 'use-package))
(setq use-package-always-defer t)

;;* Settings
(message "Loading settings")
;;** Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 200 1000 1000))
(setq ad-redefinition-action 'accept)

;;** fonts
;; Test char and monospace:
;; 0123456789abcdefghijklmnopqrstuwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
(defun frl-select-font (&optional frame)
  "Checks to see if my preferred fonts exists and then changes the
frame and default fonts. Multiple options are provided"
  (with-selected-frame (or frame (selected-frame))
    (cond 
     ((find-font (font-spec :name "Hack Nerd Font"))
      (set-frame-font "Hack Nerd Font-11" t t t)
      (set-face-font 'default "Hack Nerd Font-11"))
     ((find-font (font-spec :name "FiraCode Nerd Font Mono"))
      (set-frame-font "FiraCode Nerd Font Mono-11" t t t)
      (set-face-font 'default "FiraCode Nerd Font Mono-11"))
     ((find-font (font-spec :name "Iosevka Term Extended"))
      (set-frame-font "Iosevka Term Extended-11" t t t)
      (set-face-font 'default "Iosevka Term Extended-11"))
     ((find-font (font-spec :name "SauceCodePro Nerd Font"))
      (set-frame-font "SauceCodePro Nerd Font-11" t t t)
      (set-face-font 'default "SauceCodePro Nerd Font-11"))
     ((find-font (font-spec :name "Consolas"))
      (set-frame-font "Consolas-11" t t t)
      (set-face-font 'default "Consolas-11"))
     ((find-font (font-spec :name "courier"))
      (set-frame-font "courier-11" t t t)
      (set-face-font 'default "courier-11")))
    (cond
     ((find-font (font-spec :name "Aptos Light"))
      (set-face-font 'variable-pitch "Aptos Light-18"))
     ((find-font (font-spec :name "OpenSans Light"))
      (set-face-font 'variable-pitch "OpenSans Light-15"))
     )))
(frl-select-font)
(add-hook 'after-make-frame-functions 'frl-select-font)

;; (set-face-attribute 'default nil :family "Consolas")
;; (set-face-attribute 'fixed-pitch nil :family "Consolas")
;; (set-face-attribute 'variable-pitch nil :family "Calibri")

;;** Visuals
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(load-theme 'modus-operandi t)
(setq modus-themes-headings   
      (quote ((1 . (background overline 1.04))
              (2 . (overline background 1.03))
              (3 . (overline 1.02))
              (t . (rainbow 1.01)))))
(load-theme 'modus-vivendi t)

(put 'narrow-to-region 'disabled nil)

;; emojis
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

;; transparency mode
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; use window divider
(window-divider-mode)
(set-face-foreground 'window-divider "red3")

;;** Enable minor modes
(column-number-mode)
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode -1)        ; Revert buffers when the underlying file has changed
(prefer-coding-system 'utf-8-unix) 
(recentf-mode 1)
(save-place-mode 1)                ; Remember and restore the last cursor location
(savehist-mode 1)                  ; save history for ordering commands
(set-language-environment "UTF-8")
(show-paren-mode 1)
(global-visual-line-mode t)
(tab-bar-mode t)

;;*** Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;** Miscellaneous settings
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))
(setq bookmark-save-flag 1)
(setq comp-deferred-compilation t)      ; Perform jit compilation
(setq cua-auto-tabify-rectangles nil)   ; Don't tabify after rectangle commands
(setq frame-title-format "Emacs - %b")  ; for autohotkey
(setq history-length 25)
(setq inhibit-startup-message t)        ; no splash screen
(setq initial-scratch-message nil)      ; scratch message
(setq initial-major-mode 'fundamental-mode) ; set the scratch buffer mode
(setq package-native-compile t)
(setq select-active-regions nil)        ; prevents active mark from changing
                                        ; the primary X selection
(setq text-scale-mode-step 1.05)
(setq visible-bell t)
;; (setq warning-minimum-level :error)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete) ; make tab key do indent first then
                                           ; completion.
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq create-lockfiles nil)           ; turn off lock files. Causes issues with
                                      ; OneDrive and it is only me.
(setq native-comp-async-report-warnings-errors 'silent) ; eat the native compile warnings
(setq native-comp-async-jobs-number 1)
(setq kill-whole-line t)

;;** Save File settings
;; (auto-save-visited-mode 1)
;; (setq auto-save-interval 1000)          ; save every 1000 characters typed
(setq-default auto-save-default t)
(defvar BACKUPDIR (expand-file-name "~/.saves/"))
(setq backup-directory-alist
 `((".*" . ,BACKUPDIR)))                   ; don't litter my fs tree
(setq auto-save-timeout 30)
(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat BACKUPDIR "\\2") t)
        ("\\`/\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat BACKUPDIR "\\2") t)
        ("\\`[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat BACKUPDIR "\\2") t)
        ))

;;** Backup settings
(setq
 backup-by-copying t               ; don't clobber symlinks-
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                ; use versioned backups

;;** When using a server, Ctrl-x k will end the server edit
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

;;* Function Definitions
;;** Delete File and Buffer
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;; https://gist.github.com/hyOzd/23b87e96d43bca0f0b52
(defun frl/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;;** Client Save and then Kill Emacs 
(defun client-save-kill-emacs(&optional display)
  " This is a function that can bu used to save buffers and 
  shutdown the emacs daemon. It should be called using 
  emacsclient -e '(client-save-kill-emacs)'.  This function will
  check to see if there are any modified buffers, active clients
  or frame.  If so, an x window will be opened and the user will
  be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

    ;; Check if there are modified buffers, active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
                                     (> (length (frame-list)) 1)
                                     ))  

    ;; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
        (message "Initializing x windows system.")
        (x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

    ;; Save the current frame.  
    (setq new-frame (selected-frame))

    ;; When displaying the number of clients and frames: 
    ;; subtract 1 from clients (this client).
    ;; subtract 2 from frames (the frame just created and the default frame.)
    (when (or (not active-clients-or-frames)
              (yes-or-no-p
               (format "There are currently %d clients and %d frames. Exit anyway?"
                       (- (length server-clients) 1) (- (length (frame-list)) 2)))) 
      
      ;; If the user quits during the save dialog then don't exit emacs.
      ;; Still close the terminal though.
      (let((inhibit-quit t))
        ;; Save buffers
        (with-local-quit
          (save-some-buffers)) 
	
        (if quit-flag
            (setq quit-flag nil)  
          ;; Kill all remaining clients
          (progn
            (dolist (client server-clients)
              (server-delete-client client))
            ;; Exit emacs
            (kill-emacs))) 
        ))
    ;; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )

(defun modified-buffers-exist() 
  "This function will check to see if there are any buffers
  that have been modified.  It will return true if there are
  and nil otherwise. Buffers that have buffer-offer-save set to
  nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (buffer-modified-p buffer)
                 (not (buffer-base-buffer buffer))
                 (or
                  (buffer-file-name buffer)
                  (progn
                    (set-buffer buffer)
                    (and buffer-offer-save (> (buffer-size) 0))))
                 )
        (setq modified-found t)
        )
      )
    modified-found
    )
  )

;;** Browser functions

;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "/mnt/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")

(defun frl-browse-lucky (start end)
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "http://www.google.com/search?btnI&q="
                        (url-hexify-string q)))))

(defvar web-search-history)
(defun frl-browse-url-web-search (start &optional end)
  "Uses browse-url to search for the text in the region or prompts for a string."
  (interactive "r")
  (if (use-region-p)
      (let ((q (buffer-substring-no-properties start end)))
        (browse-url (concat "https://www.google.com/search?q="
                            (url-hexify-string q))))
    (let ((q (read-from-minibuffer "Search Web:" nil nil nil 'web-search-history)))
      (browse-url (concat "https://www.google.com/search?q="
                          (url-hexify-string q))))))

;;** Start git-bash within emacs
(defcustom git-path (or (getenv "GIT_INSTALL_ROOT") ; for standard install
                        (getenv "gitdir")           ; for portable git
                        nil)
  "Provides the path to the git installation. The environment variables GIT_INSTALL_ROOT or gitdir are used as the default value. If the value is nil, then neither of those were used.

This variable is really only applicable to the Windows environment."
  :initialize 'custom-initialize-delay
  :type 'string
  :group 'frl)

(defun frl-git-bash ()
  "Launches git-bash as a shell in Emacs.

When in the shell, not all bash escape codes work and so you may
need to fix up the prompt. The following is an example with a
simplified prompt.

if [ -n \"$INSIDE_EMACS\" ]; then
    export PS1='\\=\\[\\033[32m\\\]\\u@\\h \\=\\[\\033[33m\\]\\w\\=\\[\\033[36m\\]\`__git_ps1\\=\`\\=\\[\\033[0m\\]\\n$ '
fi

Source: https://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
"
  (interactive)
  (if git-path
      (let ((explicit-shell-file-name
             (concat git-path "\\bin\\bash.exe"))
            (explicit-sh-args '("--" "--cd-to-home" "--login" "-i" "-l")))
        (call-interactively 'shell))
    (message "Please customize `git-bash-path' to provide the location of the git installation."))
  )

;;* key bindings
;; Reference:
;;     https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; (global-set-key (kbd "") 'interactive-command)
(message "Loading keybindings")
(defvar frl-map
  (let ((map (make-sparse-keymap)))
    (define-key map "bs" 'bookmark-set)
    (define-key map "bj" 'bookmark-jump)
    (define-key map "d"  'dired-jump)
    (define-key map "k"  'describe-bindings)
    (define-key map "r " 'point-to-register)
    (define-key map "rj" 'jump-to-register)
    (define-key map "tm" 'modus-themes-toggle)
    (define-key map "W"  'frl-browse-lucky)
    (define-key map "w"  'frl-browse-url-web-search)
    (define-key map "rr" 'cua-rectangle-mark-mode)
    (define-key map "tl" 'toggle-truncate-lines)
    (define-key map "tt" 'toggle-transparency)
    map)
  "My key map.")
(define-key (current-global-map) (kbd "C-c") frl-map)

(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd  "C--") 'text-scale-adjust)
(global-set-key (kbd  "C-=") 'text-scale-adjust)
(global-set-key (kbd  "C-x ;") 'comment-dwim)
(global-set-key (kbd  "M-o") 'other-window)
(global-set-key (kbd  "C-o") 'find-file)
(global-set-key (kbd  "C-c w") 'kill-current-buffer)
(global-set-key (kbd  "M-1") 'delete-other-windows)
(global-set-key (kbd  "M-2") 'split-window-vertically)
(global-set-key (kbd  "M-3") 'split-window-horizontally)
(global-set-key (kbd  "M-0") 'delete-window)
(global-set-key (kbd  "<RET>") 'newline-and-indent)


;;* abbrev
(message "Loading abbrev")
(define-prefix-command 'abbrev-map nil "abbrevs")
(define-key frl-map (kbd "Ae") 'edit-abbrevs)
(define-key frl-map (kbd "Aa") 'add-mode-abbrev)           ; add; c-u word count
(define-key frl-map (kbd "At") 'inverse-add-mode-abbrev)   ; add typo
(define-key frl-map (kbd "Al") 'list-abbrevs)              ; list
(define-key frl-map (kbd "Aw") 'write-abbrev-file)         ; save

;; enable abbrev mode in some individual modes
(dolist (hook '(text-mode-hook          ; the list of modes to enable abbrev
                org-mode-hook))
  (add-hook hook #'abbrev-mode))

(setq save-abbrevs 'silent)        ; save abbrevs when files are saved

;; If the abbrev file does not exist, then create a blank file
(if (not (file-exists-p (expand-file-name "abbrev_defs" user-emacs-directory)))
    (with-temp-buffer
      (write-file (expand-file-name "abbrev_defs" user-emacs-directory)))
  (message "abbrev_def exists"))

;; tell emacs where to read abbrev definitions from...
(setq abbrev-file-name                  
      (expand-file-name "abbrev_defs" user-emacs-directory))  
(read-abbrev-file)                      ; read abbrevs in

;;* ahk
(message "Loading ahk")
(use-package ahk-mode
  :ensure t
  :hook '((ahk-mode . outline-minor-mode)
          (ahk-mode . eglot-ensure))
  :mode "\\.ahk[2]*\\'"
  :custom
  (ahk-indentation 2)
  :config
  (setq outline-regexp ";;+"))

(use-package python-mode
  :ensure nil  ; makes sure that Emacs in-build python-mode is used
  :hook
  (python-mode . eglot-ensure)  ; connect to language server when py-file is opened
  (python-ts-mode . eglot-ensure)
  :custom
  (python-shell-interpreter "python")
  )

;;* all-the-icons
(message "Loading all-the-icons")
(use-package all-the-icons
  :ensure t
  :demand)  ;; forcing the load of this; we defer everything by default

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;;* company
(use-package company
  :ensure t
  :commands global-company-mode
  )
(add-hook 'after-init-hook 'global-company-mode)

;;* consult
(defun frl-org-find-filename () (interactive) (consult-fd org-directory))
(defun frl-org-find-in-files () (interactive) (consult-ripgrep org-directory))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("M-b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-fd)
         ; ("M-s D" . consult-locate)
         ; ("M-s g" . consult-grep)
         ("M-s F" . frl-org-find-filename)
         ("M-s G" . frl-org-find-in-files)
         ;; ("M-s G" . consult-git-grep)
         ("M-s g" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;;** Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;;** The :init configuration is always executed (Not lazy)
  :init

  ;;** Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;;** Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;;** Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;;** Configure other variables and modes in the :config section,
  ;;** after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;;* dired
(message "Loading dired")

(defvar frl-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun frl-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'frl-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))

;;* ediff

(defun frl-dired-ediff-files ()
  "Run ediff against marked files in dired."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(use-package dired
  :bind (:map dired-mode-map ("/" . 'frl-dired-limit-regexp)
              ("~" . 'frl-dired-ediff-files)))

(setq dired-listing-switches "-agho --group-directories-first")

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map ("H" . dired-hide-dotfiles-mode )))

;;* flycheck
(use-package flycheck
  :ensure t
  :hook ((text-mode org-mode prog-mode) . flycheck-mode)
  )
;;* flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(defun suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'message #'silence))))

(advice-add 'ispell-init-process :around #'suppress-messages)
(advice-add 'ispell-kill-ispell :around #'suppress-messages)

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . 'flyspell-correct-wrapper)))

;;* google-this
(use-package google-this
  :ensure t)
(google-this-mode 1)

;;* helpful
(message "Loading helpful")
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command) 
  ([remap describe-key] . helpful-key))

;;* hideshow (Not used)
;; (use-package hideshow
;;   :ensure t
;;   :init (add-hook #'prog-mode-hook #'hs-minor-mode)
;;   :bind (:map hs-minor-mode-map ("C-<tab>" . hs-toggle-hiding)
;;               ("C-M-<tab>" . hs-show-all))
;;   :config
;;   (define-key hs-minor-mode-map (kbd "M-SPC @")
;;     (lookup-key hs-minor-mode-map (kbd "C-c @")))
;;   (define-key hs-minor-mode-map (kbd "C-c @") nil))
;;* htmlize
(message "Loading htmlize")
(use-package htmlize
  :ensure t
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region))

;;* linum
(eval-after-load "linum"
  (custom-theme-set-faces
   'user
   '(line-number-current-line ((t (:inherit (hl-line fixed-pitch)))))
   '(line-number ((t (:inherit fixed-pitch))))))


;;* magit
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :commands magit)

;;* marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;* markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :bind (:map markdown-mode-map 
              ("C-c p" . markdown-preview))
  :custom
  (markdown-command "pandoc -d html " "Use pandoc for markdown generation.")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
;;* modeline
(message "Loading simple-modeline")
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :custom ((doom-modeline-height 15)))
(use-package simple-modeline
  :ensure t
  :hook (after-init . simple-modeline-mode)
  :custom
  (simple-modeline-segments
   '((simple-modeline-segment-modified simple-modeline-segment-buffer-name simple-modeline-segment-position)
     (simple-modeline-segment-input-method
      simple-modeline-segment-eol simple-modeline-segment-encoding
      simple-modeline-segment-vc simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode))))

;;* modeline nerd-icons
;; Install the nerd icons by using, then install the font on Windows 
;;     M-x nerd-icons-install-fonts
(use-package nerd-icons
  :ensure t)

;;* orderless
;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;;* outline-minor-mode
(setq outline-minor-mode-cycle t)
(add-hook 'prog-mode-hook
	  (lambda() "Initialize outline-mode"
	    (outline-minor-mode)))
(with-eval-after-load "outline"
  (require 'foldout))
;;* outline mode - change to use comments
;;** variables
(defvar my-outline-regexp-alist
  '((emacs-lisp-mode . "\\s-*;;\\*+")
    (js2-mode        . "\\s-*//\\*+")
    (web-mode        . "\\s-*//\\*+")
    (java-mode       . "\\s-*//\\*+")
    (c-mode          . "\\s-*//\\*+")
    ;; (python-mode     . "\\s-*##\\*+")
    ;; (python-ts-mode  . "\\s-*##\\*+")
    (sh-mode         . "\\s-*##\\*+")))

;;** hooks
(add-hook 'emacs-lisp-mode-hook #'my-setup-outline-mode)
(add-hook 'js2-mode-hook #'my-setup-outline-mode)
;; (add-hook 'python-mode-hook #'my-setup-outline-mode)
;; (add-hook 'python-ts-mode-hook #'my-setup-outline-mode)
(add-hook 'sh-mode-hook #'my-setup-outline-mode)
(add-hook 'java-mode-hook #'my-setup-outline-mode)
(add-hook 'c-mode-hook #'my-setup-outline-mode)
(add-hook 'web-mode-hook #'my-setup-outline-mode)

;;** my-toggle-outline
(defun my-toggle-outline ()
  "Toggle outline visibility using `outline-toggle-children'."
  (interactive)
  (if (outline-on-heading-p t)
      (outline-toggle-children)
    (indent-for-tab-command)))

;;** my-python-outline-level
;; (defun my-python-outline-level ()
;;   (- (match-end 0) (match-beginning 0)))



;;** my-setup-outline-mode
(defun my-setup-outline-mode ()
  (let ((regexp (cdr (assoc major-mode my-outline-regexp-alist))))
    (when regexp
      (setq-local outline-regexp regexp)
      (outline-minor-mode 1)
      (define-key outline-minor-mode-map (kbd "TAB") 'my-toggle-outline))))
      ;; (when (eq major-mode 'python-ts-mode)
      ;;   (setq-local outline-heading-end-regexp "\n")
      ;;   (setq-local outline-level #'my-python-outline-level)))))

;;* different way to do python outline

(defun python-mode-outline-hook ()
  (setq outline-level 'python-outline-level)

  (setq outline-regexp
	(rx (or
	     ;; Commented outline heading
	     (group
	      (* space)	 ; 0 or more spaces
	      (one-or-more (syntax comment-start))
	      (one-or-more space)
	      ;; Heading level
	      (group (repeat 1 8 "\*"))  ; Outline stars
	      (one-or-more space))

	     ;; Python keyword heading
	     (group
	      ;; Heading level
	      (group (* space))	; 0 or more spaces
	      bow
	      ;; Keywords
	      (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
	      eow)))))

(defun python-outline-level ()
  (or
   ;; Commented outline heading
   (and (string-match (rx
		       (* space)
		       (one-or-more (syntax comment-start))
		       (one-or-more space)
		       (group (one-or-more "\*"))
		       (one-or-more space))
		      (match-string 0))
	(- (match-end 0) (match-beginning 0)))

   ;; Python keyword heading, set by number of indentions
   ;; Add 8 (the highest standard outline level) to every Python keyword heading
   (+ 8 (- (match-end 0) (match-beginning 0)))))

(add-hook 'python-mode-hook 'python-mode-outline-hook)
(add-hook 'python-ts-mode-hook ' python-mode-outline-hook)

;;* org-agenda
(defun frl-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun frl-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;; (defun frl-org-agenda-hook ()
;;   "Sets up my special org-agenda views"
;;   (interactive)
;;   (setq org-agenda-custom-commands
;;         '(("d" "Daily agenda and all TODOs"
;;            ((tags "PRIORITY=\"A\""
;;                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                    (org-agenda-overriding-header "High-priority unfinished tasks:")))
;;             (agenda "" ((org-agenda-ndays 1)))
;;             (alltodo ""
;;                      ((org-agenda-skip-function '(or (frl-org-skip-subtree-if-habit)
;;                                                      (frl-org-skip-subtree-if-priority ?A)
;;                                                      (org-agenda-skip-if nil '(scheduled deadline))))
;;                       (org-agenda-overriding-header "ALL normal priority tasks:"))))
;;            ((org-agenda-compact-blocks t)))))
;;   )
;; (add-hook 'org-mode-hook 'frl-org-agenda-hook)

;;* org-appear
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-autosubmarkers t)
  )

;;* org ox-odt
(use-package ox-odt
  :ensure t
  :commands org-odt--translate-list-tables )

(with-eval-after-load 'ox-html
  (unless (featurep 'ox-odt)
    (require 'ox-odt))
  (add-to-list
   'org-export-filter-parse-tree-functions
   (defun org-html--translate-list-tables (tree backend info)
     (if (eq backend 'html)
         (org-odt--translate-list-tables tree backend info)
       tree))))

;;* org color link
(defun frl-setup-color-link-hook ()
  "Setup the color link support in org-mode."
  (org-link-set-parameters "color"
                           :follow (lambda (path) (message "Changes the text color."))
                           :export (lambda (path desc format _)
                                     (cond
                                      ((eq format 'html)
                                       (format "<span style=\"color:%s;\">%s</span>"
                                               path desc))
                                      ((eq format 'latex)
                                       (format "{color{%s}%s}" path desc))))
                           :face (lambda (path)
                                   (if (color-defined-p path)
                                       (list :background
                                             (if (color-dark-p (color-name-to-rgb path))
                                                 "white" "black")
                                             :foreground path)))))

(add-hook 'org-mode-hook 'frl-setup-color-link-hook)

;;* org electric pair
;; (message "Loading org electric pair")
;; (defvar org-electric-pairs '((?/ . ?/) (?= . ?=)
;;                              (?\_ . ?\_) (?~ . ?~) (?\* . ?\*))
;;   "Electric pairs for org-mode.")

;; ;; The following was derived from this reddit article and response
;; ;; https://www.reddit.com/r/emacs/comments/getsn7/cant_match_in_electricpairinhibitpredicate/?utm_source=share&utm_medium=web2x&context=3
;; ;; The key was putting the items in the syntax table as paired delimiters.
;; (defun my--org-electric-pair-inhibit-predicate (char)
;;   "Return `t' if CHAR is \"<\", \"*\" at the beginning of the line, or "
;;   (or (char-equal char ?<)
;;       (and (char-equal char ?*) (not(use-region-p))) ; Only insert matching * if region is active
;;       ;; Still need to apply the user option.
;;       (funcall (default-toplevel-value 'electric-pair-inhibit-predicate)
;;                char)))

;; (defun org-add-electric-pairs ()
;;   (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
;;   (setq-local electric-pair-text-pairs electric-pair-pairs)
;;   (setq-local electric-pair-inhibit-predicate #'my--org-electric-pair-inhibit-predicate)
;;   (modify-syntax-entry ?* "$")
;;   (modify-syntax-entry ?~ "$")
;;   (modify-syntax-entry ?= "$")
;;   (modify-syntax-entry ?_ "$")
;;   (modify-syntax-entry ?/ "$")
;;   )

;; (add-hook 'org-mode-hook 'org-add-electric-pairs)

;;* org html export css support
;; The following allows me to select the CSS theme to use for the exported html.
;; The org-theme-css-dir has the path where org files are stored.  
(defvar org-theme-css-dir (expand-file-name "org-css/" user-emacs-directory)
  "Location of css files for org export.")
(defvar org-theme-skip-style nil "skip styling due to handling somewhere else")

(defun org-theme ()
  "Provides a prompt to collect the CSS theme to use during export."
  (interactive)
  (let* ((cssdir org-theme-css-dir)
         (css-choices (directory-files cssdir nil ".css$"))
         (css (completing-read "theme: " css-choices nil t)))
    (concat cssdir css)))

(defun set-org-html-style (&optional backend)
  "A hook function to select the CSS html style to incorporate into the export."
  (interactive)
  (when (and (or (null backend) (eq backend 'html)) (eq org-theme-skip-style nil))
    (let ((f (or (and (boundp 'org-theme-css) org-theme-css) (org-theme))))
      (if (file-exists-p f)
          (progn
            (set (make-local-variable 'org-theme-css) f)            
            (set
             (make-local-variable 'org-html-head)
             (with-temp-buffer
               (insert "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n") 
               (insert-file-contents f)
               (goto-char (point-max))
               (insert "\n/*]]>*/-->\n</style>\n")
               (buffer-string)))
            (set (make-local-variable 'org-html-head-include-default-style)
                 nil)
            (message "Set custom style from %s" f))
        (message "Custom header file %s doesnt exist")))))

;; always prompt for the css style to use
(add-hook 'org-export-before-parsing-hook 'set-org-html-style)

;;* org integrity link
(if (string-equal-ignore-case (user-login-name) "ba919") 
    (add-hook 'org-mode-hook 'frl-org-integrity-link-hook))

(defun frl-org-integrity-link-hook ()
  "hook to initialize the integrity link"
  (let ((inhibit-message t)
        (message-log-max nil))
    (org-add-link-type "integrity" 'frl-org-integrity-open))
  )

(defun frl-org-integrity-open (link)
  "Open the integrity item identified by the link." 
  (w32-shell-execute
   "open"
   (concat "integrity:" link)))

(message "Loading org-mode")
(defun frl-copy-cell () "Copy the content of a cell"
       (interactive) ;copy the content of a cell
       (when (org-at-table-p)
         (kill-new
          (string-trim
           (substring-no-properties(org-table-get-field))))))


;;   frl/get-open-org-file      :arrow_right: select a buffer from org buffers
(defun frl/get-open-org-file ()
  (buffer-file-name
   (get-buffer
    (org-icompleting-read "Buffer: "
                          (mapcar 'buffer-name
                                  (org-buffer-list 'files))))))

;;* org-tidy
(use-package org-tidy
  :ensure t
  :custom
  (org-tidy-protect-overlay nil)
  (org-tidy-properties-style 'fringe)
  (org-tidy-top-property-style 'keep)
  :hook
  (org-mode . org-tidy-mode)
  :bind (:map org-mode-map
         ("C-c o p" . #'org-tidy-toggle)
         )
  )

;;* org-mode
(use-package org
  :ensure t
  :commands org-mode
  :bind (("C-c l" . #'org-store-link)
	 ("C-c a" . #'org-agenda)
	 ("C-c c" . #'org-capture)
         ("C-c C" . #'org-capture-goto-last-stored)
         :map org-mode-map
         ("C-S-<right>" . nil)
         ("C-S-<left>" . nil)
         ("S-<right>" . nil)
         ("S-<left>" . nil)
         ("C-c o e" . org-emphasize)
         ("C-c o n" . org-toggle-narrow-to-subtree)
         ("C-c o l" . org-toggle-link-display)
         ("C-c o i" . org-time-stamp-inactive)
         ("C-c o h" . org-toggle-heading)
         ("C-c o t w" . org-table-toggle-column-width)
         ("C-c o t s" . org-table-shrink)
         ("C-c o t e" . org-table-edit-field)
         ("C-c o c" . frl-copy-cell)
         )
  :mode ("\\.txt\\'" . 'org-mode)
  :custom
  (org-agenda-custom-commands
   '(("d" "Daily agenda and all TODOs"
      ((tags "PRIORITY=\"A\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (tags "project=\"active\"|project=\"unplanned\"|project=\"paused\"")
       (agenda "" ((org-agenda-ndays 1)))
       (alltodo ""
                 ((org-agenda-skip-function '(or (frl-org-skip-subtree-if-habit)
                                                (frl-org-skip-subtree-if-priority ?A)
                                                (org-agenda-skip-if nil '(scheduled deadline))))
                 (org-agenda-overriding-header "ALL normal priority tasks:")))))
      ; ((org-agenda-compact-blocks t))

     ("p" . "Projects" )
     ("pa" "Active" tags "+project=\"active\"")
     ("pu" "Unplanned" tags "+project=\"unplanned\"")
     ("pp" "Paused" tags "+project=\"paused\"")))
  (org-attach-preferred-new-method 'dir)
  (org-attach-use-inheritance t)
  (org-refile-targets '((frl/get-open-org-file . (:maxlevel . 1))))
  (org-outline-path-complete-in-steps nil)
  (org-agenda-files '("~/OneDrive/notes"))
  (org-agenda-file-regexp "\\`[^.].*\\.txt\\'")
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCEL(c)")))
  (org-support-shift-select t)          ; shift works in special headings
  (org-hide-emphasis-markers t)
  (org-use-fast-todo-selection 'expert)
  (org-log-into-drawer t)               ; use :logbook: drawers 
  (org-log-done 'time)
  (org-log-done-with-time nil)
  (org-html-postamble nil)
  (org-emphasis-alist
   '(("*"
      (bold :foreground "RosyBrown"))
     ("/" italic)
     ("_" underline)
     ("="
      (:background "maroon" :foreground "white"))
     ("~"
      (:background "deep sky blue" :foreground "MidnightBlue"))
     ("+"
      (:strike-through t))))
  (org-export-with-broken-links t)
  (org-src-preserve-indentation t)
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (user-full-name "FR Lyvers")
  (org-directory "~/OneDrive/notes")
  (org-return-follows-link t)
  (org-ellipsis "…")
  (org-html-validation-link nil)
  :config
  ;; set up a project property to use to find projects for agenda views
  (add-to-list 'org-global-properties-fixed '("project_ALL" . "no unplanned active paused complete canceled"))
  (setq org-global-properties '(("project" . "no")))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch :foreground "light cyan")
  
  (which-key-add-key-based-replacements
  "C-c o" "org"
  "C-c t" "toggle"
  "C-c o t" "org-table")
  
  ;; open standard file types in Windows executables if on windows
  (if (eq system-type 'windows-nt)
      (add-to-list 'org-file-apps 
                   '("\\.\\(?:PDF\\|DOCX\\|XLS[BMX]?\\|PPTX?\\|pdf\\|docx\\|xls[bmx]\\|pptx?\\)\\'" . default))
    )

  ;; going to use explorer.exe to open files if on WSL
  (if (string-match "WSL2" operating-system-release)
        (add-to-list
         'org-file-apps
         '("\\.\\(?:PDF\\|DOCX\\|XLS[BMX]?\\|PPTX?\\|pdf\\|docx\\|xls[bmx]?\\|pptx?\\|x?html?\\)\\'" . "startwin.sh %s")
         ))
;; For this to work you need a startwin.sh in the path containing the following:
;; #!/bin/env bash

;; filepath="$1";
;; [ "" == "$1" ] && filepath=".";
;; if [ ! -e "${filepath}" ]; then
;;     echo "  Did not find the file, \"$filepath\". Unable to continue.";
;;     return;
;; fi;
;; fullfilepath="$(realpath "${filepath}")"
;; winfilepath="$(wslpath -m "$fullfilepath")"
;; echo "  Opening \"${fullfilepath}\" using explorer.exe.";
;; echo "  Opening \"${winfilepath}\""
;; explorer.exe "file://${winfilepath}"
 
  ;; todo.txt is found relative to org-directory
  (setq org-capture-templates
        '(
          ;; Templates for tasks
          ("t" "Todo"
           entry (file+headline "todo.txt" "Inbox")
           "* TODO %?
:PROPERTIES:
:created: %u
:link: %a
:END:
 %i" :prepend t)
          ("n" "Meeting Notes" entry
           (file+function buffer-name (lambda () (goto-char (point))))
           "* %u %^{Subject}%^{attendees|Randy Lyvers;}p
** Notes
- %?
** Actions
")
          ("a" "Meeting Agenda" entry
           (file+function buffer-name (lambda () (goto-char (point))))
           "* %^u %^{Subject}%^{attendees|Randy Lyvers;}p
 *P*: %^{Purpose} \\\\
 *O*: %^{Outcome} \\\\
 *S*: Agenda
      1. %?
 *T*: %^{Duration} \\\\
** Notes
- 
** Actions
")
          ))
  (add-to-list 'org-capture-templates
               '("w"                    ; keybinding
                 "Wait"                 ; short name in template
                 plain (file+headline
                        "todo.txt"      ; filename
                        "Inbox")        ; heading
                 "%?"
                 :jump-to-captured t
                 :hook (lambda ()
                         (yas-expand-snippet
                          (yas-lookup-snippet
                           "wait"       ; yasnippet name
                           'org-mode t)))))

  (visual-line-mode t)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '(             ; (python . t)
                                 (emacs-lisp . t)
                                 (shell . t))))

;;* org-inlinetask
;; (use-package org-inlinetask
;;   :bind
;;   (
;;    :map org-mode-map
;;         ("C-c o k" . #'org-inlinetask-insert-task)
;;         )
;;   :custom (org-inlinetask-min-level 8)
;;   (org-inlinetask-export t)
;;   )

;;* org capture screen shot
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
   (let* ((filename (concat (make-temp-name (concat (buffer-file-name) "_"
                                                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png")))
     (shell-command "snippingtool /clip")
     (shell-command (concat "powershell.exe -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
     )
   (insert (concat "[[file:" filename "]]")))
   ((string-match "WSL2" operating-system-release)
    (let* ((filename (concat (make-temp-name (concat (buffer-file-name) "_"
                                                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
           (wslfilename (string-trim (shell-command-to-string (concat "wslpath -m \"$(realpath '" filename "')\""))))
           (wslshortfile (format "./%s" (file-name-nondirectory wslfilename))))
           ; (message (format "1|filename=%s|fullpath=%s|shortname=%s" filename wslfilename wslshortfile))
           ; (shell-command "snippingtool.exe /clip")
           (shell-command (concat "powershell.exe -command 'Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save(\""wslfilename "\",[System.Drawing.Imaging.ImageFormat]::Png); Write-Output \"clipboard content saved as file\"} else {Write-Output \"clipboard does not contain image data\"}'"))       
           (insert (concat "[[file:"wslshortfile"]]"))
      )
    )
   )
  (org-display-inline-images))

(global-set-key "\C-cs" 'my-org-screenshot)

;;* org onenote link
(if (string-equal-ignore-case (user-login-name) "ba919")
    (add-hook 'org-mode-hook #'(lambda () (let ((inhibit-message t)
        (message-log-max nil)) (org-add-link-type "onenote" 'org-onenote-open)))))

(defun org-onenote-open (link)
  "Open the OneNote item identified by the unique OneNote URL." 
  (w32-shell-execute
   "open"
   (concat "onenote:" link)))

;;* org-refile
(defvar frl-refiled-location-link nil)

(defun frl-move-todo ()
  "move the todo to my inbox putting a cross reference to the current location, and add a link to the  current location as an action"
  (interactive)
  (let (before-move-loc)
    (save-excursion
      (org-back-to-heading)
      (setq before-move-loc (point-marker)))
    (org-refile)
    (when (and before-move-loc
               frl-refiled-location-link)
      (let ((buf (marker-buffer before-move-loc)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (goto-char before-move-loc)
              (insert (concat "- [ ] " frl-refiled-location-link "\n"))))))
      (setq frl-refiled-location-link nil))
  ))

(defun frl-set-last-refile-link ()
  "capture the link where the refile occurred"
  (setq frl-refiled-location-link (org-store-link nil)))
(add-hook 'org-after-refile-insert-hook #'frl-set-last-refile-link)

(defun frl-org-refile-keybind ()
  (define-key org-mode-map "\C-com" '("move todo" . frl-move-todo)))
(add-hook 'org-mode-hook #'frl-org-refile-keybind)

;;* org-roam
;;
(message "Loading org-roam")
(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  ;; emacs if folder exists
  (if (file-directory-p "~/OneDrive - Cummins/")
      (setq org-roam-directory "~/OneDrive - Cummins/__notes")
    (setq org-roam-directory "~/OneDrive/notes"))
; :hook ((after-init-hook . org-roam-db-autosync-enable))
  
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "daily/")
  (org-roam-extract-new-file-path "%<%Y%m%d>-${slug}.txt")
  (org-roam-file-extensions '("org" "txt"))
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d>-${slug}.txt"
                 "#+title: ${title}\n\n* ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  :bind (
         ("C-c n c" . org-roam-capture)
         ("C-c n d c" . org-roam-dailies-capture-today)
         ("C-c n d d" . org-roam-dailies-goto-date)
         ("C-c n d n" . org-roam-dailies-goto-next-note)
         ("C-c n d p" . org-roam-dailies-goto-previous-note)
         ("C-c n d t" . org-roam-dailies-goto-today)
         ("C-c n f" . org-roam-node-find)
         ("M-n c" . org-roam-capture)
         ("M-n d c" . org-roam-dailies-capture-today)
         ("M-n d d" . org-roam-dailies-goto-date)
         ("M-n d n" . org-roam-dailies-goto-next-note)
         ("M-n d p" . org-roam-dailies-goto-previous-note)
         ("M-n d t" . org-roam-dailies-goto-today)
         ("M-n f" . org-roam-node-find)
         :map org-mode-map 
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . org-roam-refile)
         ("C-c n t" . org-id-get-create)
         ("M-n b" . org-roam-buffer-toggle)
         ("M-n i" . org-roam-node-insert)
         ("M-n r" . org-roam-refile)
         ("M-n t" . org-id-get-create))
  :config
  ;; (org-roam-setup)
  (run-with-idle-timer 9 nil 'org-roam-db-autosync-mode)
  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

;;* ox-frl-clip
(require 'htmlize)
(defun ox-frl-clip (r1 r2 &optional prefix)
  "Export the selected region to HTML and copy it to the clipboard.
R1 and R2 define the selected region.
Providing a prefix argument (c-u) will update the org-roam ids."
  (interactive "r\nP")
  ;; (copy-region-as-kill r1 r2)
  (if (equal major-mode 'org-mode)
      (save-window-excursion
        (let* ((org-html-with-latex 'dvipng)
               (outfile (expand-file-name "~/.frl-clip.html"))
               (org-theme-skip-style t)
               (org-theme-css (concat org-theme-css-dir "github-cleantable.css"))
	       (buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
               (html (with-current-buffer buf (buffer-string)))
               (f (or (and (boundp 'org-theme-css) org-theme-css) (org-theme))))
          ;; (message (format "filename=%s" f))
          (if prefix 
              (progn (message "ox-frl-clip: Updating org-roam ids.")
                     (org-roam-update-org-id-locations)))  ;; used to fix up links for html
          (if (file-exists-p f)
              (with-current-buffer buf
                (goto-char (point-min))
                (insert (with-temp-buffer
                          (insert "<style type=\"text/css\">\n<!--/*--><!--*/\n<![CDATA[/*>\n") 
                          (insert-file-contents f)
                          (goto-char (point-max))
                          (insert "\n/*]]>*/-->\n</style>\n")
                          (buffer-string))
                        )
                (clone-buffer "*lyvers-test-buffer*")
                (goto-char (point-max))))
          (cond
           ((eq system-type 'windows-nt)
            (with-current-buffer buf
              (write-region nil nil outfile)
              (call-process "powershell.exe" nil nil nil
                            "type" outfile "|" "set-clipboard" "-ashtml")
              (message "HTML is on the clipboard.")))
           ((string-match "WSL2" operating-system-release)
            (if (file-directory-p "~/winhome/")
                (let* ((outfile (expand-file-name "~/winhome/.frl-clip.html"))
                       (wsloutfile (string-trim (shell-command-to-string "wslpath -m $(realpath ~/winhome/.frl-clip.html)")))
                       )
                  (write-region nil nil outfile)
                  (call-process "powershell.exe" nil nil nil
                                "type" wsloutfile "|" "set-clipboard" "-ashtml")
                  (message (format "ox-frl-clip: Clipboard is loaded from the file, %s." wsloutfile))
                  )
                )
            ))
          (kill-buffer buf)))
    ;; Use htmlize when not in org-mode.
    (progn
      (deactivate-mark)
      (let* ((outfile (expand-file-name "~/.frl-clip.html"))
             (html (htmlize-region-for-paste r1 r2)))
      (cond
       ((eq system-type 'windows-nt)
        (with-temp-buffer
          (insert html)
          (write-region nil nil outfile)
          (call-process "powershell.exe" nil nil nil
                        "type" outfile "|" "set-clipboard" "-ashtml")
          (message "HTML is on the clipboard.")))
       ((eq system-type 'darwin) (message "Unsupported - see ox-clip"))
       ((eq system-type 'gnu/linux) (message "Unsupported - see ox-clip"))
       )))))

(global-set-key (kbd "C-c x") 'ox-frl-clip)

;; (use-package ox-clip
;;   :ensure t
;;   :custom
;;    (ox-clip-w32-cmd "powershell -Command \"$input | set-clipboard -ashtml\"")
;;   :bind ("C-C x" . ox-clip-formatted-copy))
;;* ox-pandoc
(use-package ox-pandoc
  :ensure t
  :commands ox-pandoc
  :config
  (setq org-pandoc-options '((standalone . t)))
  (setq org-pandoc-options-for-docx '((standalone . t)))
  )
(with-eval-after-load 'ox
  (require 'ox-pandoc))

;;* placeholder
;; To use this functionality put <++> into the file and then move
;; forward and backward to change the elements
(use-package placeholder
  :vc (:url "https://github.com/oantolin/placeholder.git"
        :rev :newest)
  :bind (("C-S-n" . placeholder-forward)
         ("C-S-p" . placeholder-backward)
         ("C-S-x" . placeholder-insert)))

;;* plantuml
(use-package plantuml-mode
  :ensure t
  :after org-mode
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-executable-path "/usr/bin/plantuml")
  (plantuml-default-exec-mode 'executable)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  )

(use-package flycheck-plantuml
  :ensure 
  :after (flycheck plantuml-mode)
  :config
  (flycheck-plantuml-setup))

;;* python

;;** eglot
(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :hook (python-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c <tab>" . #'company-complete)
              ("C-c e f n" . #'flymake-goto-next-error)
              ("C-c e f p" . #'flymake-goto-prev-error)
              ("C-c e r" . #'eglot-rename)
              )
  )

;;** blacken
(use-package blacken
  :ensure t
  :hook ((python-mode
          python-ts-mode) . blacken-mode))

;;** python
;; Built-in Python utilities
;; python venv for LSP
;; python -m venv ~/venvs/<venv>
;; source ~/venvs/<venv>/bin/activate
;; pip install jedi-language-server
;; pip install black

(defun python-google-docstring ()
  "Generate google-style docstring for python."
  (interactive)
  (if (region-active-p)
      (progn
        (call-process-region
         (region-beginning) (region-end)
         (expand-file-name
          "~/.emacs.d/plugins/python-format-g-docs/format-g-docs.exe")
         nil t t)
        (message "Docs are generated")
        (deactivate-mark))
    (message "No region active; can't generate docs!"))
  )

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook ((python-mode
;;           python-ts-mode) . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; add the ability to generate google-style document comments
  (bind-key "C-c pr" #'python-google-docstring python-mode-map)
  ;; Use IPython when available or fall back to regular Python 
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "py")
    (setq python-shell-interpreter "py"))
   ((executable-find "python")
    (setq python-shell-interpreter "python"))
   (t
    (setq python-shell-interpreter "python3"))))

;;** pyvenv
;; Required to easily switch virtual envs 
;; via the menu bar or with `pyvenv-workon` 
;; Setting the `WORKON_HOME` environment variable points 
;; at where the envs are located. 
(use-package pyvenv
  :ensure t
  :commands (pyvenv-mode)
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/venvs"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  (pyvenv-mode 1)
  ;; Restart the python process when switching environments
  ;; This was causing a lot of restarts of the python environment. Moving around a buffer
  ;; would cause it to restart. Removed fro now. 
  ;; (add-hook 'pyvenv-post-activate-hooks (lambda ()
  ;;                                         (pyvenv-restart-python)))
  :hook (python-ts-mode . pyvenv-mode))

;;** code-cells
(defun frl-code-cells ()
  "Load code-cell-mode and start REPL for Python."
  (interactive)
  (run-python)
  (code-cells-mode 1))

(use-package code-cells
  :ensure t
  :defer t
  :commands code-cells-mode
  :bind (:map code-cells-mode-map
              ("M-p" . 'code-cells-backward-cell)
              ("M-n" . 'code-cells-forward-cell)
              ("C-c C-c" . 'code-cells-eval)
              ([remap python-shell-send-region] . 'code-cells-eval))
  :config
  (add-to-list 'code-cells-eval-region-commands '(python-ts-mode . python-shell-send-region))
  )

;;** dap
(use-package dap-mode
  :ensure t
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands (dap-debug dap-debug-edit-template)
  :after lsp-mode
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  :bind (:map lsp-mode-map
              ("C-c Pd" . dap-hydra))
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-auto-configure-mode)
  (setq dap-python-debugger 'debugpy))

;;** pytest
(use-package python-pytest
  :ensure t
  :after python
  :bind ("C-c Pt" . 'python-pytest-dispatch))

;;* rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;* ripgrep
(use-package rg
  :ensure t
  :commands rg-menu
  :bind (("C-c g" . 'rg-menu)))

;;* separedit
(use-package separedit
  :ensure t
  :commands separedit
  :bind (:map prog-mode-map ("C-c '" . 'separedit))
  :config
  (setq separedit-default-mode 'markdown-mode)
  )

;;* savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;;* tab jump out
(use-package tab-jump-out
  :ensure t
  :after yasnippet-mode
  :config
  (setq yas-fallback-behavior '(apply tab-jump-out 1)))

;;* treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-t"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;* tree-sitter
(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))
;;* vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  )


;;* which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;;* yas-snippet
(message "Loading yasnippet.")
(defun frl-yas-org-collect-tags ()
  "retrieves the tags for the current heading for inclusion in TODOs with yasnippet"
       (let (
             (tags (org-get-tags))
             (value ":"))
         (while tags
           (setq value (concat value (format "%s" (pop tags)) ":")))
         (if (string= value ":") "" value)
         ))

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          ahk-mode
          snippet-mode) . yas-minor-mode-on)
)

(use-package yasnippet-snippets
  :ensure t)

;;* vterm
(use-package vterm
  :ensure t
  :commands vterm
  :bind (:map vterm-mode-map (("C-o" . nil)  ; allow c-o to switch buffers
			      ("M-b" . nil)  ; allow M-b to list buffers
			      ("C-S-x" . nil) ; allow to pass thru
			      ("C-S-v" . vterm-yank) ; Ctrl-Shift-V
			      ))
  :custom ((vterm-shell "bash")))

;;* yaml
(use-package yaml-mode
  :ensure t
  :commands yaml-mode
  :mode "\\.yaml\\'"
  :bind (:map yaml-mode-map ("\C-m" . 'newline-and-indent))
  )

(put 'upcase-region 'disabled nil)

;;* Unused content
;; msys64 -- not using
;; (setq explicit-shell-file-name "c:/Users/versl/msys64/usr/bin/bash.exe")
;; (setq shell-file-name "bash")
;; (setq explicit-bash.exe-args '("--login" "-i"))
;; (setenv "SHELL" shell-file-name)
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; ;; Ignore modification-time-only changes in files, i.e. ones that
;; ;; don't really change the contents.  This happens often with
;; ;; switching between different VC buffers.

;; (defun update-buffer-modtime-if-byte-identical ()
;;   (let* ((size      (buffer-size))
;;          (byte-size (position-bytes size))
;;          (filename  buffer-file-name))
;;     (when (and byte-size (<= size 1000000))
;;       (let* ((attributes (file-attributes filename))
;;              (file-size  (nth 7 attributes)))
;;         (when (and file-size
;;                    (= file-size byte-size)
;;                    (string= (buffer-substring-no-properties 1 (1+ size))
;;                             (with-temp-buffer
;;                               (insert-file-contents filename)
;;                               (buffer-string))))
;;           (set-visited-file-modtime (nth 5 attributes))
;;           t)))))

;; (defun verify-visited-file-modtime--ignore-byte-identical (original &optional buffer)
;;   (or (funcall original buffer)
;;       (with-current-buffer buffer
;;         (update-buffer-modtime-if-byte-identical))))
;; (advice-add 'verify-visited-file-modtime :around #'verify-visited-file-modtime--ignore-byte-identical)

;; (defun ask-user-about-supersession-threat--ignore-byte-identical (original &rest arguments)
;;   (unless (update-buffer-modtime-if-byte-identical)
;;     (apply original arguments)))
;; (advice-add 'ask-user-about-supersession-threat :around #'ask-user-about-supersession-threat--ignore-byte-identical)

;;* custom-file
;; Load the custom-file which can have variables and other code added
(load custom-file)

;;* the closing message
(message (format "finished running my init.el. Time taken was %s." (emacs-init-time))) 

(provide 'init)
;;* init.el ends here

; LocalWords:  Calibri Consolas Iosevka FiraCode ABCDEFGHIJKLMNOPQRSTUVWXYZ
