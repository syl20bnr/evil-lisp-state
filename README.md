# evil-lisp-state

Adds a new [evil][evil-link] state to navigate lisp code and edit sexp trees
using [smartparens][smartparens-link] and mnemonic key bindings.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [evil-lisp-state](#evil-lisp-state)
    - [Install](#install)
        - [Package manager](#package-manager)
        - [Manually](#manually)
    - [Configuration](#configuration)
        - [Backward prefix](#backward-prefix)
    - [Philosophy](#philosophy)
    - [Intuitive navigation model](#intuitive-navigation-model)
    - [Key bindings maps](#key-bindings-maps)
        - [Regular normal state bindings](#regular-normal-state-bindings)
        - [Lisp specific bindings](#lisp-specific-bindings)
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

**Note:** The variable `evil-lisp-state-backward-prefix` must be set before
requiring `evil-lisp-state`.

## Philosophy

`evil-lisp-state` goal is to replace the `normal state` in lisp buffers so
_you should not have the need_ to switch back and forth  between `normal state`
and `lisp state`. In the case you do, please fill an issue.

_Note that some mechanism will be provided in order to  have `insert state`
to optionally go back to `lisp state` when pressing `ESC`. Stay tuned._

To achieve this goal, this mode tries to keep the useful commands from the
`normal state` and add new commands (often with `shift` modifier) for
manipulating the data structure.

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

## Key bindings maps

### Regular normal state bindings

Key Binding   | Function
--------------|------------------------------------------------------------
`a`           | evil-append
`A`           | evil-append-line
`c`           | evil-change
`d`           | evil-delete
`h`           | next char
`i`           | evil-insert-state
`I`           | evil-insert-line
`j`           | next visual line
`k`           | previous visual line
`l`           | next char
`o`           | evil-insert-below
`O`           | evil-insert-above
`p`           | evil-past-after
`P`           | evil-past-before
`r`           | evil-replace
`C-r`         | undo-tree-redo
`u`           | undo-tree-undo
`x`           | evil-delete-char
`X`           | evil-delete-backward-char
`y`           | evil-yank
`ESC`         | evil-normal-state

### Lisp specific bindings

_In this table we assume that `evil-lisp-state-backward-prefix` is set to
default `<tab>`_

Key Binding   | Function
--------------|------------------------------------------------------------
`(`           | insert sibling before sexp and switch to `insert state`
`)`           | insert sibling after sexp and switch to `insert state`
`$`           | sp-end-of-sexp
`0`           | sp-beginning-of-sexp
`b`           | sp-forward-barf-sexp
`B`           | sp-absorb-sexp
`<tab> b`     | sp-backward-barf-sexp
`C`           | sp-convolute-sexp
`Dd`          | sp-kill-hybrid-sexp
`Dx`          | sp-kill-sexp
`<tab> Dx`    | sp-backward-kill-sexp
`Ds`          | sp-kill-symbol
`<tab> Ds`    | sp-backward-kill-symbol
`Dw`          | sp-kill-word
`<tab> Dw`    | sp-backward-kill-word
`E$`          | evil-lisp-state-eval-sexp-end-of-line
`Ee`          | eval-last-sexp
`Ef`          | eval-defun
`gs`          | go to source of symbol under point
`gt`          | sp-transpose-sexp
`gT`          | sp-transpose-hybrid-sexp
`H`           | previous sexp at the same level
`J`           | next sexp one level down
`K`           | previous sexp one level up
`L`           | next sexp of the same level
`M`           | sp-join-sexp (think about `merge-sexp`)
`R`           | sp-raise-sexp
`s`           | sp-forward-slurp-sexp
`<tab> s`     | sp-backward-slurp-sexp
`S`           | sp-splice-sexp-killing-forward
`<tab> S`     | sp-splice-sexp-killing-backward
`w`           | wrap sexp
`W`           | unwrap sexp
`<tab> W`     | sp-backward-unwrap-sexp
`Y`           | sp-copy-sexp
`<tab> y`     | sp-backward-copy-sexp
`backspace`   | sp-backward-delete-char
`S-backspace` | sp-delete-char
`RET`         | indent next line
`S-RET`       | insert new line char and switch to `insert state`

## Thanks

Thanks goes to the creators of [evil][evil-link] and [smartparens][smartparens-link]
modes for their awesome contributions. Without them `evil-lisp-state` would
have been a lot harder to implement.

[evil-link]: https://gitorious.org/evil/pages/Home
[smartparens-link]: https://github.com/Fuco1/smartparens/wiki
[melpa-link]: http://melpa.org/
