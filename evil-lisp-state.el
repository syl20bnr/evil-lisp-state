;;; evil-lisp-state.el --- An evil state to navigate Lisp code and modify it with smartparens

;; Copyright (C) 2014 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil smartparens lisp mnemonic
;; Created: 9 Oct 2014
;; Version: 4.2.1
;; Package-Requires: ((evil "1.0.9") (smartparens "1.6.1"))
;; URL: https://github.com/syl20bnr/evil-lisp-state

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds a new Evil state called --LISP-- (<L>) with mnemonics key bindings
;; to navigate Lisp code and edit the sexp tree.

;; Philosophy
;; ----------

;; `evil-lisp-state` goal is to replace the `normal state` in lisp buffers so
;; _you should not have the need_ to switch back and forth  between `normal state`
;; and `lisp state`. In the case you do, please fill an issue.

;; _Note that some mechanism will be provided in order to  have `insert state`
;; to optionally go back to `lisp state` when pressing `ESC`. Stay tuned._

;; To achieve this goal, this mode tries to keep the useful commands from the
;; `normal state` and add new commands (often with `shift` modifier) for
;; manipulating the data structure.

;; Intuitive navigation model
;; --------------------------

;; `hjkl` behaves like in the default `normal state`.

;; **Next sexp on the same level (sibling)**
;; - `L` next sexp
;; - `H` previous sexp

;; **Change level (parent/children)**
;; - `J` go to next sexp one level down
;; - `K` go to previous one level up

;; Example Configuration:
;; ----------------------

;; override the `L` key bindings of evil `motion state`:

;; (require 'evil-lisp-state)
;; (define-key evil-normal-state-map "L" 'evil-lisp-state)

;; More information in the readme of the repository:
;; https://github.com/syl20bnr/evil-lisp-state

;;; Code:

(require 'evil)
(require 'smartparens)

(evil-define-state lisp
  "Lisp state.
 Used to navigate lisp code and manipulate the sexp tree."
  :tag " <L> "
  :enable (motion)
  :cursor (bar . 2)
  ;; force smartparens mode
  (if (evil-lisp-state-p) (smartparens-mode)))

(defgroup evil-lisp-state nil
  "Evil lisp state."
  :group 'emulations
  :prefix 'evil-lisp-state-)

(defcustom evil-lisp-state-backward-prefix "<tab>"
  "Prefix to execute the backward version of a command"
  :type 'string
  :group 'evil-lisp-state)

(defmacro evil-lisp-state-define-key (key command &optional backward)
  "Define a key binding for KEY and COMMAND.

If BACKWARD is not nil then a binding is also created for backward version
of COMMAND.
 The backward binding is prepended with `evil-lisp-state-backward-prefix'"
  (let ((backward-prefix evil-lisp-state-backward-prefix))
  `(let* ((cmdstr ,(symbol-name command))
          (cmdsym (intern (format "sp-%s" cmdstr))))
     (define-key evil-lisp-state-map ,key cmdsym)
     (if ,backward
         (let* ((bcmdstr (if (string-match "forward" cmdstr)
                             (replace-regexp-in-string "forward" "backward" cmdstr)
                           (concat "backward-" cmdstr)))
                (bcmdsym (intern (format "sp-%s" bcmdstr)))
                (bkey ,(concat backward-prefix key)))
           (define-key evil-lisp-state-map (kbd bkey) bcmdsym))))))

;; regular normal state key bindings
(define-key evil-lisp-state-map "1"   'digit-argument)
(define-key evil-lisp-state-map "2"   'digit-argument)
(define-key evil-lisp-state-map "3"   'digit-argument)
(define-key evil-lisp-state-map "4"   'digit-argument)
(define-key evil-lisp-state-map "5"   'digit-argument)
(define-key evil-lisp-state-map "6"   'digit-argument)
(define-key evil-lisp-state-map "7"   'digit-argument)
(define-key evil-lisp-state-map "8"   'digit-argument)
(define-key evil-lisp-state-map "9"   'digit-argument)
(define-key evil-lisp-state-map "a"   'evil-append)
(define-key evil-lisp-state-map "A"   'evil-append-line)
(define-key evil-lisp-state-map "c"   'evil-change)
(define-key evil-lisp-state-map "d"   'evil-delete)
(define-key evil-lisp-state-map "h"   'evil-backward-char)
(define-key evil-lisp-state-map "i"   'evil-insert-state)
(define-key evil-lisp-state-map "I"   'evil-insert-line)
(define-key evil-lisp-state-map "j"   'evil-next-visual-line)
(define-key evil-lisp-state-map "k"   'evil-previous-visual-line)
(define-key evil-lisp-state-map "l"   'evil-forward-char)
(define-key evil-lisp-state-map "o"   'evil-open-below)
(define-key evil-lisp-state-map "O"   'evil-open-above)
(define-key evil-lisp-state-map "p"   'evil-paste-after)
(define-key evil-lisp-state-map "P"   'evil-paste-before)
(define-key evil-lisp-state-map "r"   'evil-replace)
(define-key evil-lisp-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-lisp-state-map "u"   'undo-tree-undo)
(define-key evil-lisp-state-map "x"   'evil-delete-char)
(define-key evil-lisp-state-map "X"   'evil-delete-backward-char)
(define-key evil-lisp-state-map "y"   'evil-yank)
(define-key evil-lisp-state-map [escape]    'evil-normal-state)
;; lisp specific key bindings
(define-key evil-lisp-state-map "("   'evil-lisp-state-insert-sexp-before)
(define-key evil-lisp-state-map ")"   'evil-lisp-state-insert-sexp-after)
(define-key evil-lisp-state-map "$"   'sp-end-of-sexp)
(define-key evil-lisp-state-map "0"   'sp-beginning-of-sexp)
(evil-lisp-state-define-key     "b"    forward-barf-sexp t)
(define-key evil-lisp-state-map "B"   'sp-absorb-sexp)
(define-key evil-lisp-state-map "C"   'sp-convolute-sexp)
(define-key evil-lisp-state-map "Dd"  'sp-kill-hybrid-sexp)
(evil-lisp-state-define-key     "Dx"   kill-sexp t)
(evil-lisp-state-define-key     "Ds"   kill-symbol t)
(evil-lisp-state-define-key     "Dw"   kill-word t)
(define-key evil-lisp-state-map "E$"  'evil-lisp-state-eval-sexp-end-of-line)
(define-key evil-lisp-state-map "Ee"  'eval-last-sexp)
(define-key evil-lisp-state-map "Ef"  'eval-defun)
(define-key evil-lisp-state-map "gs"  'elisp-slime-nav-find-elisp-thing-at-point)
(define-key evil-lisp-state-map "gt"  'sp-transpose-sexp)
(define-key evil-lisp-state-map "gT"  'sp-transpose-hybrid-sexp)
(define-key evil-lisp-state-map "H"   'evil-lisp-state-previous-sexp)
(define-key evil-lisp-state-map "J"   'evil-lisp-state-next-sexp-down)
(define-key evil-lisp-state-map "K"   'sp-backward-up-sexp)
(define-key evil-lisp-state-map "L"   'sp-next-sexp)
(define-key evil-lisp-state-map "M"   'sp-join-sexp)
(define-key evil-lisp-state-map "R"   'sp-raise-sexp)
(evil-lisp-state-define-key     "s"    forward-slurp-sexp t)
(evil-lisp-state-define-key     "S"    splice-sexp-killing-forward t)
(define-key evil-lisp-state-map "w"   '(lambda (&optional arg) (interactive "P")
                                         (sp-wrap-with-pair "(")))
(evil-lisp-state-define-key     "W"    unwrap-sexp t)
(evil-lisp-state-define-key     "Y"    copy-sexp t)
(define-key evil-lisp-state-map (kbd "<backspace>") 'sp-backward-delete-char)
(define-key evil-lisp-state-map (kbd "<S-backspace>") 'sp-delete-char)
(define-key evil-lisp-state-map (kbd "RET") 'evil-lisp-state-indent-next-line)
(define-key evil-lisp-state-map (kbd "<S-return>") 'evil-lisp-state-new-line-insert-state)

(defun evil-lisp-state-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (evil-end-of-line)
    (eval-last-sexp nil)))

(defun evil-lisp-state-insert-left-paren ()
  "Switch to insert state and insert `('"
  (interactive)
  (evil-insert-state)
  (sp-insert-pair "("))

(defun evil-lisp-state-previous-sexp ()
  "Go to the beginning of the previous sexp."
  (interactive)
  (sp-previous-sexp)
  (sp-backward-sexp))

(defun evil-lisp-state-next-sexp-down ()
  "Go to the beginning of the next sexp one level down."
  (interactive)
  (sp-down-sexp 2)
  (sp-backward-up-sexp))

(defun evil-lisp-state-forward-symbol ()
  "Go to the beginning of the next symbol."
  (interactive)
  (let ((n (if (char-equal (char-after) ?\() 1 2)))
    (sp-forward-symbol n)
    (sp-backward-symbol)))

(defun evil-lisp-state-insert-sexp-after ()
  "Insert sexp after the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-up-sexp)
    (evil-insert-state)
    (sp-newline)
    (sp-insert-pair "(")))

(defun evil-lisp-state-insert-sexp-before ()
  "Insert sexp before the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-backward-sexp)
    (evil-insert-state)
    (sp-newline)
    (evil-previous-visual-line)
    (evil-end-of-line)
    (insert " ")
    (sp-insert-pair "(")
    (indent-for-tab-command)))

(defun evil-lisp-state-indent-next-line ()
  "Indent line and go the next visual line."
  (interactive)
  (join-line 1)
  (sp-newline))

(defun evil-lisp-state-new-line-insert-state ()
  "Insert new line char and switch to insert mode."
  (interactive)
  (sp-newline)
  (evil-insert-state))

(provide 'evil-lisp-state)

;;; evil-lisp-state.el ends here
