# evil-lisp-state

Adds a new [evil][evil-link] state to navigate lisp code and edit sexp trees
using [smartparens][smartparens-link] and mnemonic key bindings.

## Install

### Package manager

You can either install `evil-lisp-state` from [MELPA][melpa-link]:

```
 M-x package-install evil-list-state
```

Or add it to your `Cask` file:

```elisp
(source melpa)

(depends-on "evil-lisp-state")
```

### Manually

Add `evil-lisp-state.el` to your load path. `evil-lisp-state` requires
both `evil` and `smartparens` to be installed.

## Configuration

Example of a configuration overriding the `L` key bindings of `normal state`
in order to trigger the `lisp state`.

```elisp
(require 'evil-lisp-state)
(define-key evil-normal-state-map "L" 'evil-lisp-state)
```

To change custom variables:

```
M-x customize-group evil-lisp-state
```

### Backward prefix

Corresponding backward version of a command is performed by a common prefix
whose value is determined by the custom variable
`evil-lisp-state-backward-prefix`. Default value is `n`.

For instance, `slurping forward` is performed with `s` and the backward
version `slurping backward` with `ns`.

## Key bindings map

While in `lisp state` (assume that `evil-lisp-state-backward-prefix` is set
to default `n`):

Key Binding   | Function
--------------|------------------------------------------------------------
`$`           | sp-end-of-sexp
`0`           | sp-beginning-of-sexp
`a`           | sp-absorb-sexp
`b`           | sp-forward-barf-sexp
`nb`          | sp-backward-barf-sexp
`c`           | sp-convolute-sexp
`C`           | sp-comment
`dd`          | sp-kill-hybrid-sexp
`dx`          | sp-kill-sexp
`ndx`         | sp-backward-kill-sexp
`ds`x         | sp-kill-symbol
`nds`         | sp-backward-kill-symbol
`dw`          | sp-kill-word
`ndw`         | sp-backward-kill-word
`e$`          | evil-lisp-state-eval-sexp-end-of-line
`ef`          | eval-defun
`el`          | eval-last-sexp
`es`          | eval-sexp
`h`           | sp-backward-sexp
`H`           | evil-backward-char
`i`           | evil-insert-state
`j`           | sp-down-sexp
`J`           | sp-backward-down-sexp
`k`           | sp-up-sexp
`K`           | sp-backward-up-sexp
`l`           | sp-forward-sexp
`L`           | evil-forward-char
`m`           | sp-join-sexp (think about `merge-sexp`)
`p`           | evil-past-after
`P`           | evil-past-before
`r`           | sp-raise-sexp
`R`           | sp-rewrap-sexp
`C-r`         | undo-tree-redo
`s`           | sp-forward-slurp-sexp
`ns`          | sp-backward-slurp-sexp
`S`           | sp-splice-sexp-killing-forward
`nS`          | sp-splice-sexp-killing-backward
`t`           | sp-transpose-sexp
`T`           | sp-transpose-hybrid-sexp
`u`           | undo-tree-undo
`U`           | sp-unwrap-sexp
`nU`          | sp-backward-unwrap-sexp
`y`           | sp-copy-sexp
`ny`          | sp-backward-copy-sexp
`RET`         | sp-newline
`ESC`         | evil-normal-state

## Thanks

Thanks goes to the creators of [evil][evil-link] and [smartparens][smartparens-link]
modes for their awesome contributions. Without them `evil-lisp-state` would
have been a lot harder to implement.

[evil-link]: https://gitorious.org/evil/pages/Home
[smartparens-link]: https://github.com/Fuco1/smartparens/wiki
[melpa-link]: http://melpa.milkbox.net
