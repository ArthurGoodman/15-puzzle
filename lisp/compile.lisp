(load "main.lisp")

(sb-ext:save-lisp-and-die "15-puzzle.exe" :toplevel #'main :executable t)
