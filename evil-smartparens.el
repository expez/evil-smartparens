;;; evil-smartparens.el --- Evil support for smartparens

;; Copyright (C) 2015, Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/evil-smartparens
;; Keywords: evil smartparens
;; Version: 0.3.0
;; Package-Requires: ((evil "1.0") (emacs "24.4") (smartparens "1.6.3"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Provide `evil-smartparens-mode' which enables evil support for `smartparens'.

;;; Code:

(require 'evil)
(require 'smartparens)
(require 'subr-x)
(require 'cl-lib)

(defgroup evil-smartparens nil
  "`evil-mode' compat for `smartparens-mode'"
  :group 'smartparens)

(defcustom evil-smartparens-threshold 2500
  "If the region being operated on is larger than this we cop out.

Quite a bit of work gets done to ensure the region being worked
is in an safe state, so this lets us sarifice safety for a snappy
editor on slower computers.

Even on a large computer you shouldn't set this too high or your
computer will freeze when copying large files out of Emacs."
  :group 'evil-smartparens
  :type 'string)

(defvar evil-sp--override nil
  "Should the next command skip region checks?")

(defvar evil-smartparens-mode-map (make-sparse-keymap))

(defun evil-sp--override ()
  (prog1 (or evil-sp--override
             (evil-sp--region-too-expensive-to-check))
    (setq evil-sp--override nil)))

(defun evil-sp--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun evil-sp--get-endpoint-for-sp-kill-sexp ()
  (unwind-protect
      (progn
        (push major-mode sp-navigate-consider-stringlike-sexp)
        (evil-sp--new-ending (point)
                             (or (ignore-errors
                                   (evil-sp--point-after '(sp-up-sexp 1)
                                                         '(sp-backward-down-sexp 1)))
                                 (point))
                             :no-error))
    (pop sp-navigate-consider-stringlike-sexp)))

(defun evil-sp--get-endpoint-for-killing ()
  "Return the endpoint from POINT upto which `sp-kill-sexp' would kill."
  (if (and (= (evil-sp--depth-at (point))
              (evil-sp--depth-at (point-at-eol)))
           (sp-region-ok-p (point) (point-at-eol)))
      (point-at-eol) ; Act like kill line
    (evil-sp--new-ending (point)
                         (max
                          ;; Greedy killing
                          (evil-sp--get-endpoint-for-sp-kill-sexp)
                          (evil-sp--point-after 'sp-forward-sexp)))))

(defun evil-sp--region-too-expensive-to-check ()
  "When it takes prohobitively long to check region we cop out."
  (when (region-active-p)
    (> (abs (- (region-beginning) (region-end)))
       evil-smartparens-threshold)))

(defun evil-sp-override ()
  (interactive)
  (setq evil-sp--override t))

(defun evil-sp--last-command-was-kill-p (type)
  (and type (listp type)))

(defun evil-sp--no-sexp-between-point-and-eol? ()
  "Check if the region up to eol contains any opening or closing delimiters."
  (not (or (save-excursion
             (re-search-forward (sp--get-opening-regexp) (point-at-eol)
                                :noerror))
           (save-excursion
             (re-search-forward (sp--get-closing-regexp) (point-at-eol)
                                :noerror)))))

(evil-define-operator evil-sp-backward-delete-char (beg end type register)
  :motion evil-backward-char
  (interactive "<R><x>")
  (condition-case user-error
      (if (save-excursion (forward-char) (sp-point-in-empty-sexp))
          (save-excursion (forward-char) (sp-delete-char))
        (evil-sp-delete beg end type register))
    ('error (progn (goto-char end) (evil-sp--fail)))))

(evil-define-operator evil-sp-delete-char (beg end type register)
  :motion evil-forward-char
  (interactive "<R><x>")
  (condition-case user-error
      (if (save-excursion (forward-char) (sp-point-in-empty-sexp))
          (save-excursion (forward-char) (sp-delete-char))
        (evil-sp-delete beg end type register))
    ('error (progn (goto-char beg) (evil-sp--fail)))))

(defun evil-sp--add-bindings ()
  (when smartparens-strict-mode
    (evil-define-key 'normal evil-smartparens-mode-map
      (kbd "d") #'evil-sp-delete
      (kbd "c") #'evil-sp-change
      (kbd "y") #'evil-sp-yank
      (kbd "S") #'evil-sp-change-whole-line
      (kbd "X") #'evil-sp-backward-delete-char
      (kbd "x") #'evil-sp-delete-char)
    (evil-define-key 'visual evil-smartparens-mode-map
      (kbd "X") #'evil-sp-delete
      (kbd "x") #'evil-sp-delete))
  (evil-define-key 'normal evil-smartparens-mode-map
    (kbd "D") #'evil-sp-delete-line
    (kbd "Y") #'evil-sp-yank-line
    (kbd "C") #'evil-sp-change-line)
  (evil-define-key 'insert evil-smartparens-mode-map
    (kbd "DEL") 'sp-backward-delete-char)
  (evil-define-key 'visual evil-smartparens-mode-map
    (kbd "o") #'evil-sp-override)
  (evil-normalize-keymaps))

(defun evil-sp--line-segment-ok-p (start-col end-col)
  "Check if the region between START-COL and END-COL is
  balanced"
  (unless (sp-region-ok-p (evil-sp--point-after `(forward-char ,start-col))
                          (evil-sp--point-after `(forward-char ,end-col)))
    (evil-sp--fail)))

(defun evil-sp--block-is-balanced (beg end)
  (apply-on-rectangle #'evil-sp--line-segment-ok-p beg end))

(evil-define-operator evil-sp-delete (beg end type register yank-handler)
  "Call `evil-delete' with a balanced region"
  (interactive "<R><x><y>")
  (if (or (evil-sp--override)
          (= beg end)
          (and (eq type 'block)
               (evil-sp--block-is-balanced beg end)))
      (evil-delete beg end type register yank-handler)
    (condition-case nil
        (let ((new-beg (evil-sp--new-beginning beg end))
              (new-end (evil-sp--new-ending beg end)))
          (if (and (= new-end end)
                   (= new-beg beg))
              (evil-delete beg end type register yank-handler)
            (evil-delete (evil-sp--safe-beginning new-beg)
                         new-end 'inclusive register yank-handler)))
      (error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                    (end (evil-sp--new-ending beg end)))
               (evil-delete beg end type register yank-handler))))))

(evil-define-operator evil-sp-change (beg end type register yank-handler)
  "Call `evil-change' with a balanced region"
  (interactive "<R><x><y>")
  ;; #20 don't delete the space after a word
  (when (save-excursion (goto-char end) (looking-back " " (- (point) 5)))
    (setq end (1- end)))
  (if (or (evil-sp--override)
          (= beg end)
          (and (eq type 'block)
               (evil-sp--block-is-balanced beg end)))
      (evil-change beg end type register yank-handler)
    (condition-case nil
        (let ((new-beg (evil-sp--new-beginning beg end))
              (new-end (evil-sp--new-ending beg end)))
          (if (and (= new-end end)
                   (= new-beg beg))
              (evil-change beg end type register yank-handler)
            (evil-change (evil-sp--safe-beginning new-beg)
                         new-end 'inclusive register yank-handler)))
      (error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                    (end (evil-sp--new-ending beg end)))
               (evil-change beg end type register yank-handler))))))

(evil-define-operator evil-sp-yank (beg end type register yank-handler)
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if (or (evil-sp--override)
          (= beg end)
          (and (eq type 'block)
               (evil-sp--block-is-balanced beg end)))
      (evil-yank beg end type register yank-handler)
    (condition-case nil
        (let ((new-beg (evil-sp--new-beginning beg end))
              (new-end (evil-sp--new-ending beg end)))
          (if (and (= new-end end)
                   (= new-beg beg))
              (evil-yank beg end type register yank-handler)
            (evil-yank new-beg new-end 'inclusive register yank-handler)))
      ('error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                     (end (evil-sp--new-ending beg end)))
                (evil-yank beg end type register yank-handler))))))

(evil-define-operator evil-sp-change-whole-line
  (beg end type register yank-handler)
  "Emulate `sp-kill-sexp' with universal prefix and enter
`evil-insert-state'."
  :motion nil
  (interactive "<R><x>")
  (evil-first-non-blank)
  (let ((beg (evil-sp--new-beginning (point) end))
        (end (evil-sp--get-endpoint-for-killing)))
    (evil-change beg end 'inclusive register yank-handler)))

(evil-define-operator evil-sp-yank-line (beg end type register yank-handler)
  "Emulate `sp-kill-sexp' with universal prefix but copy to
yank-ring instead of killing."
  :motion evil-line
  :move-point nil
  (evil-yank (point) (evil-sp--get-endpoint-for-killing) 'inclusive
             register yank-handler))

(evil-define-operator evil-sp-delete-line (beg end type register yank-handler)
  "Emulate `sp-kill-sexp' with universal prefix."
  :motion nil
  (interactive "<R><x>")
  (cond
   ((or (sp-point-in-empty-sexp) (sp-point-in-empty-string)) (sp-kill-sexp))
   ((looking-at "\n") (evil-join (point) (1+ (point))))
   (t (evil-delete (point) (evil-sp--get-endpoint-for-killing)
                   'inclusive register yank-handler))))

(evil-define-operator evil-sp-change-line (beg end type register yank-handler)
  "Emulate `sp-kill-sexp' with universal prefix and enter `evil-insert-state'."
  :motion nil
  (interactive "<R><x>")
  (evil-change (point) (evil-sp--get-endpoint-for-killing)
               type
               register yank-handler))

(defun evil-sp--add-evil-surround-operators ()
  "This registers our own operators so `evil-surround' can do
proper dispatching."
  (when (require 'evil-surround nil :noerror)
    (add-to-list 'evil-surround-operator-alist
                 '(evil-sp-change . change))
    (add-to-list 'evil-surround-operator-alist
                 '(evil-sp-delete . delete))))

(defun evil-sp--enable ()
  (evil-sp--add-bindings)
  (evil-sp--add-evil-surround-operators))

;;;###autoload
(define-minor-mode evil-smartparens-mode
  "Toggle evil-smartparens."
  :lighter " es"
  :init-value nil
  :keymap evil-smartparens-mode-map
  (when evil-smartparens-mode
    (evil-sp--enable)))

(defun evil-sp--fast-depth-at (&optional point)
  "Finds the depth at POINT using native code.

Unfortunately this only works for lisps."
  (when (memq major-mode sp--lisp-modes)
    (let ((point (or point (point))))
      (ignore-errors
        (save-excursion
          (beginning-of-defun)
          (let ((parse-state (parse-partial-sexp (point) point)))
            (when parse-state
              (let ((in-string-p (nth 3 parse-state))
                    (depth (cl-first parse-state)))
                (if in-string-p
                    (1+ depth)
                  depth)))))))))

(defun evil-sp--depth-at (&optional point)
  "Return the depth at POINT.

Strings affect depth."
  (save-excursion
    (goto-char point)
    (when (sp-point-in-comment)
      (setq point (1- (car (sp-get-comment-bounds))))))
  (let ((fast-depth (evil-sp--fast-depth-at point))
        (depth 0))
    (if fast-depth
        fast-depth
      (save-excursion
        (when point
          (goto-char point))
        (unwind-protect
            (progn
              (push major-mode sp-navigate-consider-stringlike-sexp)
              (while (and (not (sp-point-in-comment))
                          (ignore-errors (sp-backward-up-sexp)))
                (cl-incf depth)))
          (pop sp-navigate-consider-stringlike-sexp))))
    depth))

(defun evil-sp--new-ending (beg end &optional no-error)
  "Find the largest safe region delimited by BEG END."
  (let ((region (string-trim (buffer-substring-no-properties beg end))))
    (unless (string-blank-p region)
      (cond
       ((sp-point-in-empty-sexp)
        ;; expand region if we're in an empty sexp
        (setf end (save-excursion (sp-up-sexp) (point))))

       ;; reduce region if it's unbalanced due to selecting too much
       (t (while (not (or (sp-region-ok-p beg end)
                          (= beg end)))
            (cl-decf end))))))
  (if (and (not no-error) (= beg end))
      (evil-sp--fail)
    end))

(defun evil-sp--new-beginning (beg end &optional shrink)
  "Return a new value for BEG if POINT is inside an empty sexp.

If SHRINK is t we try to shrink the region until it is balanced
by decrementing BEG."
  (if (not shrink)
      (min beg
           (if (sp-point-in-empty-sexp)
               (evil-sp--point-after 'sp-backward-up-sexp)
             (point-max)))

    (let ((region (string-trim (buffer-substring-no-properties beg end))))
      (unless (string-blank-p region)
        (cond
         ((sp-point-in-empty-sexp)
          ;; expand region if we're in an empty sexp
          (setf end (save-excursion (sp-backward-up-sexp) (point))))

         ;; reduce region if it's unbalanced due to selecting too much
         (t (while (not (or (sp-region-ok-p beg end)
                            (= beg end)))
              (cl-incf beg)))))
      (when (= beg end)
        (evil-sp--fail)))
    beg))

(defun evil-sp--safe-beginning (beg)
  "Return a new value for BEG that does not include indentation."
  (max beg
       (save-excursion
         (goto-char beg)
         (back-to-indentation)
         (point))))

(defun evil-sp--fail ()
  "Error out with a friendly message."
  (user-error "That would leave the buffer unbalanced"))

(provide 'evil-smartparens)
;;; evil-smartparens.el ends here
