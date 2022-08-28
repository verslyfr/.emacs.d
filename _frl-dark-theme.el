;; _frl-dark
;; This is a mix-in theme which can be used when I have a dark background. It may require editing the theme settings to ensure this theme is first.

;; Examples of emphasis *bold*, /italics/, _underline_, =equals=, ~tilde~, +strikethrough+


;; [[file:../OneDrive/src/linux-setup/emacs/frl-emacs-themes.org::*_frl-dark][_frl-dark:1]]
(deftheme _frl-dark
  "sets up colors for a light theme")
(require 'doom-themes)

(let ((ml-bg "#002E81")
      (ml-fg "yellow")
      (ml-inactive-bg "#424242")
      (ml-inactive-fg "#c2c2c2")
      (ml-highlight-bg (doom-lighten "#002e81" 0.2))
      (height 0.9))
  (custom-theme-set-faces
   '_frl-dark
   `(mode-line-inactive ((t (:background ,ml-inactive-bg :foreground ,ml-inactive-fg :box (:line-width (1 . 2) :color ,ml-inactive-bg :style released-button)))))
   `(mode-line-emphasis ((t (:weight light))))
   `(mode-line-buffer-id ((t (:background ,ml-highlight-bg :foreground ,ml-fg :weight thin))))
   `(mode-line-highlight ((t (:background ,ml-highlight-bg :box (:line-width (1 . 3) :color ,ml-highlight-bg :style released-button)))))
   `(mode-line ((t (:background ,ml-bg :foreground ,ml-fg :height ,height :box (:line-width (1 . 3) :color ,ml-bg :style released-button)))))
   ;; `(company-selection-default ((t (:background "dark red"))))
   ;; `(company-tooltip-selection ((t (:background "dark green"))))
   )
  )

(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
(setq org-emphasis-alist
      '(("*" (bold :foreground "Orange" ))
        ("/" italic)
        ("_" underline)
        ("=" (:background "maroon" :foreground "white"))
        ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
        ("+" (:strike-through t))))

(provide-theme '_frl-dark)
;; _frl-dark:1 ends here
