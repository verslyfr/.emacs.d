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

(setq filename "/home/verslyfr/winhome/has a space/foo bar.c")
(string-trim (shell-command-to-string (concat "wslpath -m \"$(realpath '"filename"')\"")))



ssss
