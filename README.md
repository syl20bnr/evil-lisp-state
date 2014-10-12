# evil-lisp-state

Adds a new [evil][evil-link] state to navigate lisp code and edit sexp trees
using [smartparens][smartparens-link] and mnemonic key bindings.

**Although the core key bindings should not change a lot, this state is still
_under experimentation_, expect the key bindings to be changed on a regular
basis.**

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [evil-lisp-state](#evil-lisp-state)
    - [Install](#install)
        - [Package manager](#package-manager)
        - [Manually](#manually)
    - [Configuration](#configuration)
        - [Backward prefix](#backward-prefix)
    - [Text selection](#text-selection)
    - [Key bindings map](#key-bindings-map)
    - [Thanks](#thanks)

<!-- markdown-toc end -->

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

## Text selection

Text selection is done with [expand-region][expand-link] by pressing `v`.
It is also possible to select the whole line with `V`.

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
`ds`          | sp-kill-symbol
`nds`         | sp-backward-kill-symbol
`dw`          | sp-kill-word
`ndw`         | sp-backward-kill-word
`e$`          | evil-lisp-state-eval-sexp-end-of-line
`ef`          | eval-defun
`el`          | eval-last-sexp
`es`          | eval-sexp
`h`           | sp-backward-symbol
`H`           | sp-backward-sexp
`i`           | evil-insert-state
`j`           | sp-down-sexp
`J`           | sp-backward-down-sexp
`k`           | sp-up-sexp
`K`           | sp-backward-up-sexp
`l`           | sp-forward-symbol
`L`           | sp-forward-sexp
`m`           | sp-join-sexp (think about `merge-sexp`)
`o`           | switch to `insert state` and add a new line below
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
`v`           | er/expand-region
`V`           | select whole line and switch to `visual state`
`x`           | sp-delete-char
`X`           | sp-backward-delete-char
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
[expand-link]: https://github.com/magnars/expand-region.el
