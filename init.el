;;; Package -- My minimalistic init

;;; Commentary:
;;;    This minimalistic init loads my real configuration from OneDrive stored
;;;    in an org file, and saves customizations in custom-vars.el in the
;;;    user-emacs-directory folder.

;;; Code:

(message "running my init.el")


;;; Initialize package manager
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

; fetch the list of packages available 
(or (file-exists-p package-user-dir)
    (package-refresh-contents)) 
;; (unless package-archive-contents
  ;; (package-refresh-contents))

; install the missing packages
; list the packages you want
(setq package-list '(use-package))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Packages installed
;;    (package-install 'company)
;;    (package-install 'orderless)
;;    (package-install 'org)
;;    (package-install 'org-appear)
;;    (package-install 'org-modern)
;;    (package-install 'org-roam)
;;    (package-install 'use-package)
;;    (package-install 'vertico)

;;; sample code
;; code snippet for how to add keys to a mode
;; this is done in a hook
;; (add-hook 'texinfo-mode-hook
;;           (lambda ()
;;             (define-key texinfo-mode-map "\C-cp"
;;                         'backward-paragraph)
;;             (define-key texinfo-mode-map "\C-cn"
;;                         'forward-paragraph)))
;;             (define-key texinfo-mode-map "\C-c\C-xx" nil)

;;; use-package
;; for profiling use-package
;; to see the report use Alt-X use-package-report
(setq use-package-compute-statistics t)
(eval-when-compile (require 'use-package))

;;; Settings
(message "Loading settings")
;;;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 200 1000 1000))

;;;; Visuals
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(load-theme 'modus-vivendi t)

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
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; use window divider
(window-divider-mode)
(set-face-foreground 'window-divider "red3")

;;;; Enable minor modes
(column-number-mode)
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)        ; Revert buffers when the underlying file has changed
(prefer-coding-system 'utf-8-unix) 
(recentf-mode 1)
(save-place-mode 1)                ; Remember and restore the last cursor location
(savehist-mode 1)                  ; save history for ordering commands
(set-language-environment "UTF-8")
(show-paren-mode 1)
(global-visual-line-mode t)

;;;;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;; Miscellaneous settings
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))
(setq bookmark-save-flag 1)
(setq comp-deferred-compilation t)     ; Perform jit compilation
(setq cua-auto-tabify-rectangles nil)  ; Don't tabify after rectangle commands
(setq frame-title-format "Emacs - %b") ; for autohotkey 
(setq history-length 25)
(setq inhibit-startup-message t)   ; no splash screen
(setq initial-scratch-message nil) ; scratch message
(setq package-native-compile t)
(setq select-active-regions nil)   ; prevents active mark from changing the primary X selection
(setq text-scale-mode-step 1.05)
(setq visible-bell t)
;; (setq warning-minimum-level :error)
(setq-default fill-column 95)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete) ; make tab key do indent first then completion.
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq create-lockfiles nil)        ; turn off lock files. Causes issues with OneDrive and it is
                                   ; only me.

;;;; Backup settings
(setq
 backup-by-copying t               ; don't clobber symlinks-
 backup-directory-alist
 '(("." . "~/.saves"))             ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                ; use versioned backups

;;;; When using a server, Ctrl-x k will end the server edit
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

;;; Function Definitions
;;;; Delete File and Buffer
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

;;;; Client Save and then Kill Emacs 
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

;;;; Browser functions
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

;;;; Start git-bash within emacs
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

;;; key bindings
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
    (define-key map "r SPC" 'point-to-register)
    (define-key map "rj" 'jump-to-register)
    (define-key map "tm" 'modus-themes-toggle)
    (define-key map "W"  'frl-browse-lucky)
    (define-key map "w"  'frl-browse-url-web-search)
    (define-key map "rr" 'cua-rectangle-mark-mode)
    map)
  "My key map.")
(define-key (current-global-map) (kbd "M-<SPC>") frl-map)
(define-key (current-global-map) (kbd "C-c") frl-map)

(global-set-key (kbd "C-S-c") 'kill-ring-save)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd  "M-b") 'switch-to-buffer)
(global-set-key (kbd  "C--") 'text-scale-adjust)
(global-set-key (kbd  "C-=") 'text-scale-adjust)
(global-set-key (kbd  "C-x ;") 'comment-dwim)
(global-set-key (kbd  "M-o") 'other-window)
(global-set-key (kbd  "C-o") 'find-file)
(global-set-key (kbd  "C-c w") 'kill-this-buffer)
(global-set-key (kbd  "M-1") 'delete-other-windows)
(global-set-key (kbd  "M-2") 'split-window-vertically)
(global-set-key (kbd  "M-3") 'split-window-horizontally)
(global-set-key (kbd  "M-0") 'delete-window)
(global-set-key (kbd  "<RET>") 'newline-and-indent)

;;; abbrev
(message "Loading abbrev")
(global-set-key (kbd "M-SPC ae") 'edit-abbrevs)            ; edit
(global-set-key (kbd "M-SPC aa") 'add-mode-abbrev)         ; add; c-u word count
(global-set-key (kbd "M-SPC at") 'inverse-add-mode-abbrev) ; add typo
(global-set-key (kbd "M-SPC al") 'list-abbrevs)            ; list
(global-set-key (kbd "M-SPC aw") 'write-abbrev-file)       ; save
(global-set-key (kbd "C-c ae") 'edit-abbrevs)              ; edit
(global-set-key (kbd "C-c aa") 'add-mode-abbrev)           ; add; c-u word count
(global-set-key (kbd "C-c at") 'inverse-add-mode-abbrev)   ; add typo
(global-set-key (kbd "C-c al") 'list-abbrevs)              ; list
(global-set-key (kbd "C-c aw") 'write-abbrev-file)         ; save

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

;;; ahk
(message "Loading ahk")
(use-package ahk-mode
  :ensure t
  :mode "\\.ahk\\'"
  :custom
  ahk-indentation 4)

;;; all-the-icons
(message "Loading all-the-icons")
(use-package all-the-icons
  :ensure t
  :demand)  ;; forcing the load of this; we defer everything by default

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;;; company
(use-package company
  :ensure t
  :commands global-company-mode
  )
(add-hook 'after-init-hook 'global-company-mode)

;;; dired
(message "Loading dired")
(setq dired-listing-switches "-agho --group-directories-first")

(use-package dired-hide-dotfiles
   :ensure t
   :hook (dired-mode . dired-hide-dotfiles-mode)
   :bind (:map dired-mode-map ("H" . dired-hide-dotfiles-mode )))

;;; flycheck
(use-package flycheck
  :ensure t
  :hook ((text-mode org-mode prog-mode) . flycheck-mode)
  )
;;; flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . 'flyspell-correct-wrapper)))

;;; helpful
(message "Loading helpful")
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command) 
  ([remap describe-key] . helpful-key))

;;; hideshow (Not used)
;; (use-package hideshow
;;   :ensure t
;;   :init (add-hook #'prog-mode-hook #'hs-minor-mode)
;;   :bind (:map hs-minor-mode-map ("C-<tab>" . hs-toggle-hiding)
;;               ("C-M-<tab>" . hs-show-all))
;;   :config
;;   (define-key hs-minor-mode-map (kbd "M-SPC @")
;;     (lookup-key hs-minor-mode-map (kbd "C-c @")))
;;   (define-key hs-minor-mode-map (kbd "C-c @") nil))
;;; htmlize
(message "Loading htmlize")
(use-package htmlize
  :ensure t
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region))

;;; linum
(eval-after-load "linum"
  (custom-theme-set-faces
   'user
   '(line-number-current-line ((t (:inherit (hl-line fixed-pitch)))))
   '(line-number ((t (:inherit fixed-pitch))))))


;;; magit
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :commands magit)

;;; markdown
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
;;; modeline
(message "Loading doom-modeline")
(use-package doom-modeline
  :ensure t
  :demand   
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;; orderless
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

;;; outline-minor-mode
(setq outline-minor-mode-cycle t)
(add-hook 'prog-mode-hook
	  (lambda() "Initialize outline-mode"
	    (outline-minor-mode)))
(with-eval-after-load "outline"
  (require 'foldout))

;;; org-agenda
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

;;; org-appear
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-autosubmarkers t)
  )

;;; org color link
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

;;; org electric pair
(message "Loading org electric pair")
(defvar org-electric-pairs '((?/ . ?/) (?= . ?=)
                             (?\_ . ?\_) (?~ . ?~) (?\* . ?\*))
  "Electric pairs for org-mode.")

;; The following was derived from this reddit article and response
;; https://www.reddit.com/r/emacs/comments/getsn7/cant_match_in_electricpairinhibitpredicate/?utm_source=share&utm_medium=web2x&context=3
;; The key was putting the items in the syntax table as paired delimiters.
(defun my--org-electric-pair-inhibit-predicate (char)
  "Return `t' if CHAR is \"<\", \"*\" at the beginning of the line, or "
  (or (char-equal char ?<)
      (and (char-equal char ?*) (not(use-region-p))) ; Only insert matching * if region is active
      ;; Still need to apply the user option.
      (funcall (default-toplevel-value 'electric-pair-inhibit-predicate)
               char)))

(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs)
  (setq-local electric-pair-inhibit-predicate #'my--org-electric-pair-inhibit-predicate)
  (modify-syntax-entry ?* "$")
  (modify-syntax-entry ?~ "$")
  (modify-syntax-entry ?= "$")
  (modify-syntax-entry ?_ "$")
  (modify-syntax-entry ?/ "$")
  )

(add-hook 'org-mode-hook 'org-add-electric-pairs)

;;; org html export css support
;; The following allows me to select the CSS theme to use for the exported html.
;; The org-theme-css-dir has the path where org files are stored.  
(defvar org-theme-css-dir (expand-file-name "org-css/" user-emacs-directory)
  "Location of css files for org export.")

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
  (when (or (null backend) (eq backend 'html))
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

;;; org integrity link
(add-hook 'org-mode-hook 'frl-org-integrity-link-hook)
(defun frl-org-integrity-link-hook ()
    "hook to initilize the integrity hook"
    (org-add-link-type "integrity" 'frl-org-integrity-open)
  )
(defun frl-org-integrity-open (link)
  "Open the integrity item identified by the unique OneNote URL." 
  (w32-shell-execute
   "open"
   (concat "integrity:" link)))
;;; org-mode
(message "Loading org-mode")
(use-package org
  :ensure t
  :commands org-mode
  :bind (("C-c l" . #'org-store-link)
	 ("C-c a" . #'org-agenda)
	 ("C-c c" . #'org-capture)
         ("C-c C" . #'org-capture-goto-last-stored)
         :map org-mode-map
         ("M-SPC e" . org-emphasize)
         ("M-SPC t s" . org-table-shrink)
         ("M-SPC t e" . org-table-edit-field)
         ("M-SPC TAB" . org-table-toggle-column-width)
         ("C-c e" . org-emphasize)
         ("C-c t s" . org-table-shrink)
         ("C-c t e" . org-table-edit-field)
         ("C-c TAB" . org-table-toggle-column-width)
         )
  :mode ("\\.txt\\'" . 'org-mode)
  :custom
  (org-agenda-custom-commands
   '(("d" "Daily agenda and all TODOs"
      ((tags "PRIORITY=\"A\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda "" ((org-agenda-ndays 1)))
       (alltodo ""
                ((org-agenda-skip-function '(or (frl-org-skip-subtree-if-habit)
                                                (frl-org-skip-subtree-if-priority ?A)
                                                (org-agenda-skip-if nil '(scheduled deadline))))
                 (org-agenda-overriding-header "ALL normal priority tasks:"))))
      ((org-agenda-compact-blocks t)))))
  (org-agenda-files '("~/OneDrive/notes"))
  (org-agenda-file-regexp "\\`[^.].*\\.txt\\'")
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w@)" "|" "DONE(d)" "CANCEL(c)")))
  (org-support-shift-select t)          ; shift works in special headings
  (org-hide-emphasis-markers t)
  (org-emphasis-alist
   '(("*"
      (bold :foreground "Orange"))
     ("/" italic)
     ("_" underline)
     ("="
      (:background "maroon" :foreground "white"))
     ("~"
      (:background "deep sky blue" :foreground "MidnightBlue"))
     ("+"
      (:strike-through t))))
  (org-export-with-broken-links 'mark)
  (org-src-preserve-indentation t)
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (user-full-name "FR Lyvers")
  (org-directory "~/OneDrive/notes")
  (org-return-follows-link t)
  (org-ellipsis " ▾")
  (org-html-validation-link nil)
  :config
  ;; open standard file types in Windows executables if on windows
  (if (eq system-type 'windows-nt)
    (add-to-list 'org-file-apps 
                 '("\\.\\(?:PDF\\|DOCX\\|XLSX?\\|PPTX?\\|pdf\\|docx\\|xlsx?\\|pptx?\\)\\'" . default))
  )
  ;; todo.txt is found relative to org-directory
  (setq org-capture-templates
        '(
          ;; Templates for tasks
          ("t" "Tasks")
          ("tt" "Todo"
           entry (file+headline "todo.txt" "Inbox")
           "* TODO %?
:PROPERTIES:
:created: %u
:link: %a
:END:
 %i" :prepend t)
          ("tw" "Wait"
           entry (file+headline "todo.txt" "Inbox")
           "* WAIT @%^{waiting on} %?
:PROPERTIES:
:created: %u
:link: %a
:END:
 %i" :prepend t)
          ("m" "Meeting")
          ("mn" "Meeting Notes" entry
           (file+function buffer-name (lambda () (goto-char (point))))
           "* %u %^{Subject}%^{attendees|Randy Lyvers;}p
** Notes
- %?
** Actions
")
          ("ma" "Meeting Agenda" entry
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
  (visual-line-mode t)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '(             ; (python . t)
                                 (emacs-lisp . t)
                                 (shell . t))))
(add-hook 'org-mode-hook 'org-num-mode)
;;; org-modern
(use-package org-modern
  :ensure t
  :commands org-modern-mode
  :init
;;  (when (display-graphic-p)             
    (add-hook 'org-mode-hook #'org-modern-mode)
;;    )
  )

;;; org onenote link
(add-hook 'org-mode-hook #'(lambda () (org-add-link-type "onenote" 'org-onenote-open)))
(defun org-onenote-open (link)
  "Open the OneNote item identified by the unique OneNote URL." 
  (w32-shell-execute
   "open"
   (concat "onenote:" link)))
;;; org-roam
;;
;; (use-package hi-lock
;;   :bind (("M-o l" . highlight-lines-matching-regexp)
;;          ("M-o r" . highlight-regexp)
;;          ("M-o w" . highlight-phrase)))
(message "Loading org-roam")
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/OneDrive/notes")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "daily/")
  (org-roam-extract-new-file-path "%<%Y%m%d>-${slug}.txt")
  (org-roam-file-extensions '("org" "txt"))
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d>-${slug}.txt" "#+title: ${title}
")
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
         ("M-SPC n c" . org-roam-capture)
         ("M-SPC n d c" . org-roam-dailies-capture-today)
         ("M-SPC n d d" . org-roam-dailies-goto-date)
         ("M-SPC n d n" . org-roam-dailies-goto-next-note)
         ("M-SPC n d p" . org-roam-dailies-goto-previous-note)
         ("M-SPC n d t" . org-roam-dailies-goto-today)
         ("M-SPC n f" . org-roam-node-find)
         :map org-mode-map 
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n r" . org-roam-refile)
         ("C-c n t" . org-id-get-create)
         ("M-SPC n b" . org-roam-buffer-toggle)
         ("M-SPC n i" . org-roam-node-insert)
         ("M-SPC n r" . org-roam-refile)
         ("M-SPC n t" . org-id-get-create))
  :config
  (org-roam-setup)
  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

;;; ox-pandoc
(use-package ox-pandoc
  :ensure t
  :commands ox-pandoc
  :config
  (setq org-pandoc-options '((standalone . t)))
  (setq org-pandoc-options-for-docx '((standalone . t)))
  )
(with-eval-after-load 'ox
  (require 'ox-pandoc))

;;; plantuml
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

;;; python

;;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-idle-delay 0.500 "recommended performance value")
  (lsp-log-io nil "set true takes perf hit")
  ;; info on disabling elements https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
   ;; lsp performance adjustment https://emacs-lsp.github.io/lsp-mode/page/performance/
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  :commands (lsp lsp-deferred))

;;;; lsp-jedi
(use-package lsp-jedi
  :ensure t
  :after lsp-mode 
  :hook (python-mode . (lambda () (require 'lsp-jedi)
                         (lsp-deferred))))
(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'jedi))

;;;; lsp-ui
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-delay 2)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-c i" . lsp-ui-imenu)))

;;;; blacken
(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

;;;; python
;; Built-in Python utilities
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python 
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

;;;; pyvenv
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
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

;;;; code-cells
(defun frl-code-cells ()
  "Load code-cell-mode and start REPL for Python."
  (interactive)
  (jupyter-run-repl "python3" nil t nil nil)
  ;; (jupyter-repl-associate-buffer nil)
  (code-cells-mode 1))

(use-package code-cells
  :defer t
  :after 'jupyter
  :commands code-cells-mode
  :bind (:map code-cells-mode-map
              ("M-p" . 'code-cells-backward-cell)
              ("M-n" . 'code-cells-forward-cell)
              ("C-c C-c" . 'code-cells-eval)
              ([remap jupyter-eval-line-or-region] . 'code-cells-eval
               )))

;;;; jupyter
(use-package jupyter
  :ensure t
  :commands (jupyter-repl-associate-buffer
             jupyter-run-server-repl
             jupyter-run-repl
             emacs jupyter-server-list-kernels))
;;;; dap
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

;;;; pytest
 (use-package python-pytest
   :ensure t
   :after python
   :bind ("C-c Pt" . 'python-pytest-dispatch))

;;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; ripgrep
(use-package rg
  :ensure t
  :commands rg-menu
  :bind (("M-SPC g" . 'rg-menu)
         ("C-c g" . 'rg-menu)))

;;; separedit
(use-package separedit
  :ensure t
  :commands separedit
  :bind (:map prog-mode-map ("C-c '" . 'separedit))
  :config
  (setq separedit-default-mode 'markdown-mode)
  )

;;; savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;;; tab jump out
(use-package tab-jump-out
  :ensure t
  :after yasnippet-mode
  :config
  (setq yas-fallback-behavior '(apply tab-jump-out 1)))

;;; treemacs
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

;;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  )


;;; which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


;;; yas-snippet
(message "Loading yasnippet.")

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :demand nil
  :ensure t)

;;; vterm
(use-package vterm
  :ensure t
  :commands vterm
  :bind (:map vterm-mode-map (("C-o" . nil)  ; allow c-o to switch buffers
			      ("M-b" . nil)  ; allow M-b to list buffers
			      ("C-S-x" . nil) ; allow to pass thru
			      ("M-SPC" . nil)
			      ("C-S-v" . vterm-yank) ; Ctrl-Shift-V
			      ))
  :custom ((vterm-shell "zsh")))

;;; yaml
(use-package yaml-mode
  :ensure t
  :commands yaml-mode
  :mode "\\.yaml\\'"
  :bind (:map yaml-mode-map ("\C-m" . 'newline-and-indent))
  )
;;; msys64 -- not using
;; (setq explicit-shell-file-name "c:/Users/versl/msys64/usr/bin/bash.exe")
;; (setq shell-file-name "bash")
;; (setq explicit-bash.exe-args '("--login" "-i"))
;; (setenv "SHELL" shell-file-name)
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;;; custom-file
;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file (expand-file-name "custom-vars.el" user-emacs-directory)))
(load custom-file)

;;; the closing
(message (format "finished running my init.el. Time taken was %s." (emacs-init-time))) 

(provide 'init)
;;; init.el ends here
