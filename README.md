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
    - [Simple to grasp navigation model](#simple-to-grasp-navigation-model)
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
`evil-lisp-state-backward-prefix`. Default value is `<tab>`.

For instance, `sp-forward-slurp-sexp` is performed with `s` and the backward
version `sp-backward-slurp-sexp` with `<tab>s`.

## Intuitive navigation model

A lot of experimentation led to the following navigation model which should
hopefully be a lot more accessible than the other models.

`hjkl` behaves like in the default `normal state`.

**Next sexp on the same level (sibling)**
- `L` next sexp
- `H` previous sexp

**Change level (parent/children)**
- `J` go to next sexp one level down
- `K` go to previous one level up

And that's it! All these commands always put the point _at the beginning_ of
the sexp.

## Text selection

Text selection is done with [expand-region][expand-link] by pressing `v`.
It is also possible to select the whole line with `V`.

## Key bindings map

While in `lisp state` (assume that `evil-lisp-state-backward-prefix` is set
to default `<tab>`):

Key Binding   | Function
--------------|------------------------------------------------------------
`(`           | switch to `insert state` and insert "("
`%`           | evil-jump-item (use it to go to the end of sexp)
`$`           | sp-end-of-sexp
`0`           | sp-beginning-of-sexp
`a`           | evil-append
`A`           | sp-absorb-sexp
`b`           | sp-forward-barf-sexp
`<tab>b`      | sp-backward-barf-sexp
`c`           | sp-convolute-sexp
`C`           | sp-comment
`dd`          | sp-kill-hybrid-sexp
`dx`          | sp-kill-sexp
`<tab>dx`     | sp-backward-kill-sexp
`ds`          | sp-kill-symbol
`<tab>ds`     | sp-backward-kill-symbol
`dw`          | sp-kill-word
`<tab>dw`     | sp-backward-kill-word
`D`           | evil-delete-line
`gs`          | go to source of symbol under point
`h`           | next char
`H`           | previous sexp at the same level
`i`           | evil-insert-state
`j`           | next visual line
`J`           | next sexp one level down
`k`           | previous visual line
`K`           | previous sexp one level up
`l`           | next char
`L`           | next sexp of the same level
`m`           | sp-join-sexp (think about `merge-sexp`)
`o`           | insert sexp after on the same level and switch to `insert state`
`O`           | insert sexp before on the same level and switch to `insert state`
`p`           | evil-past-after
`P`           | evil-past-before
`r`           | sp-raise-sexp
`C-r`         | undo-tree-redo
`s`           | sp-forward-slurp-sexp
`<tab>s`      | sp-backward-slurp-sexp
`S`           | sp-splice-sexp-killing-forward
`<tab>S`      | sp-splice-sexp-killing-backward
`t`           | sp-transpose-sexp
`T`           | sp-transpose-hybrid-sexp
`u`           | undo-tree-undo
`<tab>U`      | sp-backward-unwrap-sexp
`v`           | er/expand-region
`V`           | select whole line and switch to `visual state`
`w`           | wrap sexp
`W`           | unwrap sexp
`x$`          | evil-lisp-state-eval-sexp-end-of-line
`xf`          | eval-defun
`xl`          | eval-last-sexp
`xx`          | eval-sexp
`y`           | sp-copy-sexp
`<tab>y`      | sp-backward-copy-sexp
`backspace`   | sp-backward-delete-char
`S-backspace` | sp-delete-char
`RET`         | sp-newline (stay in `lisp state` see `o` to switch to `insert state`)
`ESC`         | evil-normal-state

## Thanks

Thanks goes to the creators of [evil][evil-link] and [smartparens][smartparens-link]
modes for their awesome contributions. Without them `evil-lisp-state` would
have been a lot harder to implement.

[evil-link]: https://gitorious.org/evil/pages/Home
[smartparens-link]: https://github.com/Fuco1/smartparens/wiki
[melpa-link]: http://melpa.milkbox.net
[expand-link]: https://github.com/magnars/expand-region.el
