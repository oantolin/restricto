;;; restricto.el --- Restrict to current minibuffer completion candidates  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a command to restrict minibuffer completion
;; to the current completion candidates.  This is something several
;; completion frameworks provide: Icicles, Ido, Ivy, Helm, Selectrum
;; all have it.  It enables fast out-of-order matching.  This package
;; is completion framework agnostic and can be used, for example, with
;; the default minibuffer tab completion, or with Icomplete.

;; To use, simply bind `restricto-narrow' in
;; `minibuffer-local-completion-map'.  If you want to be able undo
;; restriction, enable `restricto-mode' and also bind
;; `restricto-widen'.

;; The `restricto-mode' global minor mode will also detect if you use
;; `minibuffer-electric-default-mode' and in that case ensure the
;; default prompt does not reappear upon restricting.

;;; Code:

(defvar restricto--mct-stack nil
  "Stack of minibuffer completion tables.")
 
(defun restricto--clear-mct-stack ()
  (setq restricto--mct-stack nil))

(defun restricto--no-default-if-restricted (&rest _)
  (null restricto--mct-stack))

(define-minor-mode restricto-mode
  "Enable undoing of restriction to current matches.
Restriction to current matches is done by `restricto-narrow' and
undoing is done by `restricto-widen'.  Bind them to keys of your
choosing in `minibuffer-local-completion-map'."
  :global t
  (add-hook 'minibuffer-setup-hook #'restricto--clear-mct-stack)
  (advice-add 'minibuf-eldef-update-minibuffer :before-while
              #'restricto--no-default-if-restricted))

(defun restricto-widen ()
  "Cancel last restriction to current completion candidates."
  (interactive)
  (when restricto--mct-stack
    (setq minibuffer-completion-table (pop restricto--mct-stack))
    (delete-minibuffer-contents)))

(defun restricto-narrow ()
  "Restrict to current completion candidates."
  (interactive)
  (let ((all (completion-all-sorted-completions)))
    (when all
      (when restricto-mode
        (push minibuffer-completion-table restricto--mct-stack))
      (setcdr (last all) nil)
      (when ;; are we completing symbols rather than strings?
          (or (eq minibuffer-completion-table
                  #'help--symbol-completion-table)
              (and (sequencep minibuffer-completion-table)
                   (symbolp (elt minibuffer-completion-table 0))))
        (setq all (mapcar #'intern all)))
      (if (null (cdr all))
          (minibuffer-complete)
        (setq minibuffer-completion-table all)
        (delete-minibuffer-contents)))))

(provide 'restricto)
;;; restricto.el ends here
