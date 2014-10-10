# evil-lisp-state

Adds a new [evil][evil-link] state for navigating lisp code and edit sexp trees
using [smartparens][smartparens-link] and mnemonic key bindings.

## Install

### MELPA

A [MELPA][melpa-link] package will be available soon. For you have to use the
manual method.

### Manually

Add `evil-lisp-state.el` to your load path. `evil-lisp-state` requires
both `evil` and `smartparens` to be installed.

## Configuration

Example of a configuration overriding the `L` key bindings of `motion state`
in order to trigger the `lisp state`.

```elisp
(require 'evil-lisp-state)
(define-key evil-normal-state-map "L" 'evil-lisp-state)
```

## Key bindings map

While in `lisp state`:

Key Binding   | Function
--------------|------------------------------------------------------------
`$`           | sp-end-of-sexp
`0`           | sp-beginning-of-sexp
`bh`          | sp-backward-barf-sexp
`bl`          | sp-forward-barf-sexp
`c`           | sp-convolute-sexp
`d`           | sp-kill-sexp
`e$`          | evil-lisp-state-eval-sexp-end-of-line
`ef`          | eval-defun
`el`          | eval-last-sexp
`es`          | eval-sexp
`h`           | sp-backward-sexp
`i`           | evil-insert-state
`j`           | sp-down-sexp
`J`           | sp-backward-down-sexp
`k`           | sp-up-sexp
`K`           | sp-backward-up-sexp
`l`           | sp-forward-sexp
`r`           | sp-raise-sexp
`C-r`         | undo-tree-redo
`sa`          | sp-splice-sexp-killing-around
`sb`          | sp-splice-sexp-killing-backward
`sf`          | sp-splice-sexp-killing-forward
`sh`          | sp-backward-slurp-sexp
`sl`          | sp-forward-slurp-sexp
`ss`          | sp-splice-sexp
`u`           | undo-tree-undo
`ESC`         | evil-normal-state

## Thanks

Thanks goes to the creators of `evil` and `smartparens` modes for their awesome
contributions. Without them `evil-lisp-state` would have been a lot harder to
implement.

[evil-link]: https://gitorious.org/evil/pages/Home
[smartparens-link]: https://github.com/Fuco1/smartparens/wiki
[melpa-link]: http://melpa.milkbox.net
