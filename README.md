# evil-lisp-state
[![MELPA](http://melpa.org/packages/evil-lisp-state-badge.svg)](http://melpa.org/#/evil-lisp-state)

Adds a new [evil][evil-link] state to navigate lisp code and edit sexp trees
using mnemonic key bindings.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [evil-lisp-state](#evil-lisp-state)
    - [Install](#install)
        - [Package manager](#package-manager)
        - [Manually](#manually)
    - [Principle](#principle)
    - [Commands and key bindings](#commands-and-key-bindings)
        - [hjkl](#hjkl)
        - [Other commands:](#other-commands)
    - [Configuration](#configuration)

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
both `evil`, `evil-leader` and `smartparens` to be installed.

## Principle

To execute a command while in normal state, the evil-leader is used.
By default, the prefix for each command is `<leader> m`.
Each command when executed set the current state to `lisp state`.

By example:

- to slurp three times while in normal state:

    <leader> m s s s

- to wrap a symbol in parenthesis then slurping two times:

    <leader> m w s s

## Commands and key bindings

### hjkl

Evil Lisp state binds the most common used commands on hjkl:

Key Binding   | Function
--------------|------------------------------------------------------------
`h`           | previous symbol
`H`           | forward barf sexp (move the current symbol or sexp outside)
`j`           | next closing parenthesis
`J`           | wrap symbol with parenthesis (down one level)
`k`           | previous opening parenthesis
`K`           | unwrap current sexp (up one level)
`l`           | next symbol
`L`           | forward slurp sexp (move next outside sexp into current one)

So with just hjkl keys you can:
- navigate between symbols and sexps
- slurp and barf symbols and sexps
- wrap and unwrap symbols and sexps

**Notes:**
Slurping, barfing and wrapping are also bound on other mnemonic keys.

### Other commands:

Key Binding   | Function
--------------|------------------------------------------------------------
`(`           | insert expression before (same level as current one)
`)`           | insert expression after (same level as current one)
`a`           | absorb expression
`b`           | forward barf expression
`B`           | backward barf expression
`c`           | convolute expression
`e$`          | evaluate line
`ee`          | evaluate last expression
`ef`          | evaluate function
`i`           | switch to `insert state`
`I`           | go to beginning of current expression and switch to `insert state`
`m`           | merge (join) expression
`p`           | paste after
`P`           | paste before
`q`           | unwrap current expression and kill all symbols after point
`Q`           | unwrap current expression and kill all symbols before point
`r`           | raise expression (replace parent expression by current one)
`s`           | forwared slurp expression
`S`           | backward slurp expression
`T`           | transpose expression
`u`           | undo
`C-r`         | redo
`v`           | switch to `visual state`
`V`           | switch to `visual line state`
`C-v`         | switch to `visual block state`
`w`           | wrap expression with parenthesis
`W`           | unwrap expression
`xs`          | delete symbol
`xw`          | delete word
`xx`          | delete expression
`y`           | copy expression


## Configuration

Key bindings are set only for `emacs-lisp-mode` by default.
It is possible to add major modes with the variable
`evil-lisp-state-major-modes`.

The prefix key is `<leader> m` by default, it is possible to
change the `m` key to anything else with the variable
`evil-lisp-state-leader-prefix`. Set it to an empty string
if you want all the commands to be directly available
under the `<leader>` key.

[evil-link]: https://gitorious.org/evil/pages/Home
[smartparens-link]: https://github.com/Fuco1/smartparens/wiki
[melpa-link]: http://melpa.org/
