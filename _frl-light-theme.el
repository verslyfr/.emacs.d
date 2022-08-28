;; _frl-light
;; This is a mix-in theme which can be used when I have a light background. It may require editing the theme settings to ensure this theme is first.

;; Examples of emphasis *bold*, /italics/, _underline_, =equals=, ~tilde~, +strikethrough+


;; [[file:../OneDrive/src/linux-setup/emacs/frl-emacs-themes.org::*_frl-light][_frl-light:1]]
(deftheme _frl-light
  "sets up colors for a light theme")

(let ((ml-bg "#FFE9B5")
      (ml-fg "black")
      (ml-inactive-bg "#d2d2d2")
      (ml-inactive-fg "#727272")
      (tcyellow "dark orange")
      (acyan "dark cyan")
      (height 1.0))
  (custom-theme-set-faces
   '_frl-light
   `(mode-line-inactive ((t (:height ,height :background ,ml-inactive-bg :foreground ,ml-inactive-fg :box (:line-width (1 . 2) :color ,ml-inactive-fg :style released-button)))))
   `(mode-line-emphasis ((t (:weight light))))
   `(mode-line-buffer-id ((t (:background ,ml-inactive-bg :foreground ,ml-fg :weight thin))))
   `(mode-line-highlight ((t (:box (:line-width (1 . 3) :color ,ml-fg :style released-button)))))
   `(mode-line ((t (:background ,ml-bg :foreground ,ml-fg :height ,height :box (:line-width (1 . 3) :color ,ml-fg :style released-button)))))
   `(term-color-yellow ((t (:background ,tcyellow :foreground ,tcyellow))))
   `(ansi-color-yellow ((t (:background ,tcyellow :foreground ,tcyellow))))
   `(ansi-color-cyan ((t (:background ,acyan :foreground ,acyan))))))

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

(provide-theme '_frl-light)
;; _frl-light:1 ends here
