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
                               key-message
                               (predicate t))
  "Define transient keymap for the `key-binding'"
  `(let ((keymap (make-sparse-keymap)))
     ,@(seq-map (lambda (kbs)
                  (let* ((key (car kbs))
                         (fun (cdr kbs))
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
                           (let ((msg ,key-message))
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
   (("<left>" . winner-undo)
    ( "<right>" . winner-redo))
   "Winner: ← undo; → redo")

  (SK-define-keymap
   (("<left>" . previous-buffer)
    ("<right>" . next-buffer))
   "Buffer: ← prev-buffer; → next-buffer")

  (with-eval-after-load 'compile
    (SK-define-keymap
     (("M-n" . next-error)
      ("M-p" . previous-error))
     "Compilation error: M-n next-error; M-p prev-error")
    )

  (with-eval-after-load 'org
    (SK-define-keymap
     (("C-n" . org-next-visible-heading)
      ("C-p" . org-previous-visible-heading)
      ("C-u" . outline-up-heading)
      ("C-f" . org-forward-heading-same-level)
      ("C-b" . org-backward-heading-same-level)
      )
     "Org mode: C-n next; C-p pre; C-u up; C-f foward; C-b backward"
     (derived-mode-p 'org-mode))
    )
  )

(provide 'simplify-keybinding)

;;; simplify-keybinding.el ends here