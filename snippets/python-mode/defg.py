# -*- mode: snippet -*-
# Insert Google style docstring and function definition.
# name: Python Google style Docstring
# key: defg
# type: snippet
# contributor: Xaldew
# source: https://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style
# --
def ${1:name}($2):
    \"\"\"$3
    ${2:$(python-args-to-google-docstring yas-text t)}
    ${5:Returns:
        $6
}
    \"\"\"
    ${0:$$(let ((beg yas-snippet-beg)
                (end yas-snippet-end))
        (yas-expand-snippet
          (buffer-substring-no-properties beg end) beg end
              (quote ((yas-indent-line nil) (yas-wrap-around-region nil))))
            (delete-trailing-whitespace beg (- end 1)))}
