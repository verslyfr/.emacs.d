;;; frl-wsl-clipboard.el --- WSL clipboard integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Workaround for WSLg Wayland clipboard synchronization failures.
;; Bypasses Wayland entirely by routing copy and paste commands
;; directly to native Windows executables.
;;
;; Put the following into custom-vars.el to load
;; (use-package frl-wsl-clipboard
;;   :load-path "plugins/")


;;; Code:

(defun frl/wsl-copy (text &optional push)
  "Send killed text directly to the Windows clipboard using clip.exe."
  (let ((process-connection-type nil))
    (let ((proc (start-process "clip.exe" "*Messages*" "clip.exe")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun frl/wsl-paste ()
  "Retrieve yanked text directly from Windows using PowerShell."
  (let ((coding-system-for-read 'dos))
    (string-trim-right 
     (shell-command-to-string "powershell.exe -noprofile -command Get-Clipboard"))))

;;;###autoload
(defun frl/enable-wsl-clipboard ()
  "Enable direct Windows clipboard integration for WSL."
  (interactive)
  (when (getenv "WSL_INTEROP")
    (setq interprogram-cut-function 'frl/wsl-copy)
    (setq interprogram-paste-function 'frl/wsl-paste)))

;; Automatically enable the hooks when this package is loaded in WSL
(frl/enable-wsl-clipboard)

(provide 'frl-wsl-clipboard)
;;; frl-wsl-clipboard.el ends here
