
# basic keybinding
- `g w` calls `fill-paragraph` which auto-wraps all items to 80 characters
- `C-x C-c` quit emacs
- `C-g` cancel command
- `M-x` execute extended command, i.e. run any elisp function by name
- `C-h` help
    - `?` help about help, list of potential help topics
    - `k` [command] help about command

essential emacs functions
- eval-print-last-sexp: (normally C-j) evals the elisp before the cursor, outputing to scratch buffer
- eval-last-sexp: (normally C-x C-e) evals the expression before the cursor, outputing to minibuffer
- eval-defun: evaluates everything inside an expression, outputting to minibuffer


# some blogs 
http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
- thinks similar to me, solved many problems I had
  - actually created keeping search terms on with my shortcuts!
  - 
http://mattbriggs.net/blog/2012/02/27/awesome-emacs-plugins-evil-mode/

# org mode
Headers are just items with * in front of them

Commands:
- M-RET: new list item
- z*: works just like vim
- You can make words *bold*, /italic/, _underlined_, =code= and ~verbatim~, and,
    if you must, +strike-through+.
- S-left/right works with TODO headers



