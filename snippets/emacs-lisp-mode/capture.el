# -*- mode: snippet -*-
# name: capture-templates
# key: <capture
# --
(add-to-list 'org-capture-templates
             '("$0"                      ; keybinding
             "<++>"                     ; short name in template
         plain (file+headline
         "<++>"                     ; filename
         "<++>")                       ; heading
         "%?"
         :jump-to-captured t
         :hook (lambda () (yas-expand-snippet (yas-lookup-snippet
         "<++>"                         ; yasnippet name
         'org-mode t)))))