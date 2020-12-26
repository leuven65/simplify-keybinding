;; -*- coding: utf-8; lexical-binding: t; -*-
;;; simplify-keybinding.el --- simplify keybinding for some long keybindings

;; Author: Jian Wang <leuven65@gmail.com>
;; URL: https://github.com/leuven65/simplify-keybinding
;; Version: 0.1.0
;; Keywords: transient, keymap

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

;;;###autoload
(cl-defmacro SK-define-keymap (key-bindings
                               &optional
                               prefix-message
                               (predicate t))
  "Define transient keymap for the `key-binding'.

`key-bindings':
  list of keybinding definitions, such as,
  ((\"<left>\" winner-undo \"← undo\")
    ( \"<right>\" winner-redo \"→ redo\"))
`prefix-message':
  The message will be showed in the minibuffer
`predicate':
  Predicate when to enable the keybindings, such as,
  (SK-define-keymap
     ((\"C-n\" org-next-visible-heading \"C-n next\"))
     \"Navigate headlines: \"
     (derived-mode-p 'org-mode))
"
  `(let ((keymap (make-sparse-keymap))
         (help-msg ,(concat prefix-message
                            (string-join (seq-map (lambda (_) (seq-elt _ 2)) key-bindings) ";"))))
     ,@(seq-map (lambda (kbs)
                  (let* ((key (seq-elt kbs 0))
                         (fun (seq-elt kbs 1))
                         (adv-fun (intern (format "%s@my-transient"
                                                  (symbol-name fun)))))
                    `(progn
                       ;; add key to the transient keymap
                       (define-key keymap (kbd ,key) #',fun)
                       ;; remove the advice
                       (advice-remove ',fun #',adv-fun)
                       ;; create advice
                       (define-advice ,fun (:after (&rest args) my-transient)
                         (when ,predicate
                           (let ((msg help-msg))
                             (when msg
                               (message msg)))
                           (set-transient-map keymap)))))
                  )
                key-bindings)
     )
  )

(defun SK-setup-default-keymap ()
  "Setup some simplify-keybinding.

1. `winner-undo', `winner-redo'
2. `previous-buffer', `next-buffer'
3. `next-error', `previous-error'
4. `org-next-visible-heading', `org-previous-visible-heading',
   `outline-up-heading', `org-forward-heading-same-level',
   `org-backward-heading-same-level'
"

  (interactive)
  (SK-define-keymap
   (("<left>" winner-undo "← undo")
    ("<right>" winner-redo "→ redo"))
   "Winner: ")

  (SK-define-keymap
   (("<left>" previous-buffer "← prev-buffer")
    ("<right>" next-buffer "→ next-buffer"))
   "Buffer: ")

  (with-eval-after-load 'compile
    (SK-define-keymap
     (("M-n" next-error "M-n next-error")
      ("M-p" previous-error "M-p prev-error"))
     "Compilation error: ")
    )

  (with-eval-after-load 'org
    (SK-define-keymap
     (("C-n" org-next-visible-heading "C-n next")
      ("C-p" org-previous-visible-heading "C-p pre")
      ("C-u" outline-up-heading "C-u up")
      ("C-f" org-forward-heading-same-level "C-f foward")
      ("C-b" org-backward-heading-same-level "C-b backward")
      )
     "Navigate headlines: "
     (derived-mode-p 'org-mode))
    )
  )

(provide 'simplify-keybinding)

;;; simplify-keybinding.el ends here