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
    - [Configuration](#configuration)

<!-- markdown-toc end -->

## Install

### Package manager

You can either install `evil-lisp-state` from [MELPA][melpa-link]:

```
 M-x package-install evil-lisp-state
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
Commands when executed set the current state to `lisp state`.

Examples:

- to slurp three times while in normal state:

    <leader> m 3 s

- to wrap a symbol in parenthesis then slurping two times:

    <leader> m w 2 s

## Key Bindings

Key Binding                 | Function
----------------------------|------------------------------------------------------------
<kbd>\<leader\> k %</kbd>   | evil jump item
<kbd>\<leader\> k :</kbd>   | ex command
<kbd>\<leader\> k (</kbd>   | insert expression before (same level as current one)
<kbd>\<leader\> k )</kbd>   | insert expression after (same level as current one)
<kbd>\<leader\> k $</kbd>   | go to the end of current sexp
<kbd>\<leader\> k 0</kbd>   | go to the beginning of current sexp
<kbd>\<leader\> k a</kbd>   | absorb expression
<kbd>\<leader\> k b</kbd>   | forward barf expression
<kbd>\<leader\> k B</kbd>   | backward barf expression
<kbd>\<leader\> k c</kbd>   | convolute expression
<kbd>\<leader\> k ds</kbd>  | delete symbol
<kbd>\<leader\> k Ds</kbd>  | backward delete symbol
<kbd>\<leader\> k dw</kbd>  | delete word
<kbd>\<leader\> k Dw</kbd>  | backward delete word
<kbd>\<leader\> k dx</kbd>  | delete expression
<kbd>\<leader\> k Dx</kbd>  | backward delete expression
<kbd>\<leader\> k e</kbd>   | unwrap current expression and kill all symbols after point
<kbd>\<leader\> k E</kbd>   | unwrap current expression and kill all symbols before point
<kbd>\<leader\> k h</kbd>   | previous symbol
<kbd>\<leader\> k i</kbd>   | switch to `insert state`
<kbd>\<leader\> k I</kbd>   | go to beginning of current expression and switch to `insert state`
<kbd>\<leader\> k j</kbd>   | next closing parenthesis
<kbd>\<leader\> k J</kbd>   | join expression
<kbd>\<leader\> k k</kbd>   | previous opening parenthesis
<kbd>\<leader\> k l</kbd>   | next symbol
<kbd>\<leader\> k p</kbd>   | paste after
<kbd>\<leader\> k P</kbd>   | paste before
<kbd>\<leader\> k r</kbd>   | raise expression (replace parent expression by current one)
<kbd>\<leader\> k s</kbd>   | forwared slurp expression
<kbd>\<leader\> k S</kbd>   | backward slurp expression
<kbd>\<leader\> k t</kbd>   | transpose expression
<kbd>\<leader\> k u</kbd>   | undo
<kbd>\<leader\> k C-r</kbd> | redo
<kbd>\<leader\> k v</kbd>   | switch to `visual state`
<kbd>\<leader\> k V</kbd>   | switch to `visual line state`
<kbd>\<leader\> k C-v</kbd> | switch to `visual block state`
<kbd>\<leader\> k w</kbd>   | wrap expression with parenthesis
<kbd>\<leader\> k W</kbd>   | unwrap expression
<kbd>\<leader\> k y</kbd>   | copy expression

## Configuration

Key bindings are set only for `emacs-lisp-mode` by default.
It is possible to add major modes with the variable
`evil-lisp-state-major-modes'.

It is also possible to define the key bindings globally by
setting `evil-lisp-state-global` to t. In this case
`evil-lisp-state-major-modes' has no effect.

The prefix key is `<leader> m` by default, it is possible to
change the `m` key to anything else with the variable
`evil-lisp-state-leader-prefix`. Set it to an empty string
if you want all the commands to be directly available
under the `<leader>` key.

[evil-link]: https://gitorious.org/evil/pages/Home
[smartparens-link]: https://github.com/Fuco1/smartparens/wiki
[melpa-link]: http://melpa.org/
