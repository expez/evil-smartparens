;;; evil-smartparens.el --- Evil support for smartparens

;; Copyright (C) 2015, Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/evil-smartparens
;; Keywords: evil smartparens
;; Version: 0.1
;; Package-Requires: ((evil "1.0") (cl-lib "0.3") (emacs "24.4") (diminish "0.44") (smartparens "1.6.3)

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

;; Evil support for smartparens

;; Provide `evil-smartparens-mode' which enables evil support for smartparens.

;;; Code:

(require 'evil)
(require 'smartparens)
(require 'diminish)

(defgroup company-quickhelp nil
  "`evil-mode' compat for `smartparens-mode'"
  :group 'smartparens)

(defcustom evil-smartparens-lighter " SP/e"
  "The lighter used for evil-smartparens without strict mode."
  :group 'evil-smartparens
  :type 'string)

(defcustom evil-smartparens-strict-lighter " SP/se"
  "The lighter used for evil-smartparens and strict mode."
  :group 'evil-smartparens
  :type 'string)

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

(defun evil-sp--get-endpoint-for-killing ()
  "Return the endpoint from POINT upto which `sp-kill-sexp'would kill."
  (if (and (= (evil-sp--depth-at (point))
              (evil-sp--depth-at (point-at-eol)))
           (sp-region-ok-p (point) (point-at-eol)))
      (point-at-eol) ; Act like kill line
    (max
     ;; Greedy killing
     (1- (evil-sp--point-after 'sp-up-sexp))
     (evil-sp--point-after 'sp-forward-sexp))))

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

(defun evil-sp--lighter ()
  "Create the lighter for `evil-smartparens'.

We want a different lighter for `smartparens-mode' and
`smartparens-strict-mode'."
  (if smartparens-strict-mode
      evil-smartparens-strict-lighter
    evil-smartparens-lighter))

(defun evil-sp--disable ()
  "Deactive advice and restore modeline."
  (diminish-undo 'smartparens-mode)
  (remove-hook #' smartparens-disabled-hook #'evil-sp--disable))

(defun evil-sp--add-bindings ()
  (when smartparens-strict-mode
    (evil-define-key 'normal evil-smartparens-mode-map
      (kbd "d") #'evil-sp-delete
      (kbd "c") #'evil-sp-change
      (kbd "y") #'evil-sp-yank
      (kbd "S") #'evil-sp-change-whole-line
      (kbd "X") #'sp-backward-delete-char
      (kbd "x") #'sp-delete-char))
  (evil-define-key 'normal evil-smartparens-mode-map
    (kbd "D") #'evil-sp-delete-line
    (kbd "Y") #'evil-sp-yank-line
    (kbd "C") #'evil-sp-change-line)
  (evil-define-key 'visual evil-smartparens-mode-map
    (kbd "o") #'evil-sp-override))

(evil-define-operator evil-sp-delete (beg end type register yank-handler)
  "Call `evil-delete' with a balanced region"
  (interactive "<R><x><y>")
  (if (evil-sp--override)
      (evil-delete beg end type register yank-handler)
    (condition-case nil
        (let* ((beg (evil-sp--new-beginning beg end))
               (end (evil-sp--new-ending beg end)))
          (evil-delete beg end type register yank-handler)))
    ('error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                   (end (evil-sp--new-ending beg end)))
              (evil-delete beg end type yank-handler)))))

(evil-define-operator evil-sp-change (beg end type register yank-handler)
  "Call `evil-change' with a balanced region"
  (interactive "<R><x><y>")
  (if (evil-sp--override)
      (evil-change beg end type yank-handler)
    (condition-case nil
        (let* ((beg (evil-sp--new-beginning beg end))
               (end (evil-sp--new-ending beg end)))
          (evil-change beg end 'inclusive yank-handler))
      ('error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                     (end (evil-sp--new-ending beg end)))
                (evil-change beg end 'inclusive yank-handler))))))

(evil-define-operator evil-sp-yank (beg end type register yank-handler)
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if (evil-sp--override)
      (evil-yank beg end type register yank-handler)
    (condition-case nil
        (let* ((beg (evil-sp--new-beginning beg end))
               (end (evil-sp--new-ending beg end)))
          (evil-yank beg end type register yank-handler))
      ('error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                     (end (evil-sp--new-ending beg end)))
                (evil-yank beg end type yank-handler))))))

(evil-define-operator evil-sp-change-whole-line
  (beg end type register yank-handler)
  "Emulate `sp-kill-sexp' and enter `evil-insert-state'."
  :motion nil
  (interactive "<R><x>")
  (evil-first-non-blank)
  (let ((beg (evil-sp--new-beginning (point) end))
        (end (evil-sp--get-endpoint-for-killing)))
    (evil-change beg end 'inclusive register yank-handler)))

(evil-define-operator evil-sp-yank-line (beg end type register yank-handler)
  "Emulate `sp-kill-sexp' but copy to yank-ring instead of killing."
  :motion evil-line
  :move-point nil
  (evil-yank (point) (evil-sp--get-endpoint-for-killing) 'inclusive
             register yank-handler))

(evil-define-operator evil-sp-delete-line (beg end type register yank-handler)
  "Emulate `sp-kill-sexp'."
  :motion nil
  (interactive "<R><x>")
  (if (looking-at "\n")
      (evil-join (point) (1+ (point)))
    (evil-delete (point) (evil-sp--get-endpoint-for-killing)
                 'inclusive register yank-handler)))

(evil-define-operator evil-sp-change-line (beg end type register yank-handler)
  "Emulate `sp-kill-sexp'."
  :motion nil
  (interactive "<R><x>")
  (evil-change (point) (evil-sp--get-endpoint-for-killing)
               type
               register yank-handler))

(defun evil-sp--enable ()
  "Activate advice and update modeline."
  (diminish 'smartparens-mode)
  (evil-sp--add-bindings)
  (add-hook #' smartparens-disabled-hook #'evil-sp--disable))

;;;###autoload
(define-minor-mode evil-smartparens-mode
  "Toggle evil-smartparens."
  :lighter (:eval (evil-sp--lighter))
  :init-value nil
  :keymap evil-smartparens-mode-map
  (if evil-smartparens-mode
      (evil-sp--enable)
    (evil-sp--disable)))

(defun evil-sp--depth-at (&optional point)
  "Return the depth at POINT.

Strings affect depth."
  (push major-mode sp-navigate-consider-stringlike-sexp)
  (let ((depth 0))
    (save-excursion
      (when point
        (goto-char point))
      (unwind-protect
          (while (and (not (sp-point-in-comment))
                      (sp-backward-up-sexp))
            (incf depth))
        (pop sp-navigate-consider-stringlike-sexp)))
    depth))

(defun evil-sp--new-ending (beg end)
  "Find the largest safe region delimited by BEG END."
  (let ((region (s-trim (buffer-substring-no-properties beg end))))
    (unless (s-blank? region)
      (cond
       ((sp-point-in-empty-sexp)
        ;; expand region if we're in an empty sexp
        (setf end (save-excursion (sp-up-sexp) (point))))

       ;; reduce region if it's unbalanced due to selecting too much
       (t (while (not (or (sp-region-ok-p beg end)
                          (= beg end)))
            (cl-decf end))))))
  (when (= beg end)
    (evil-sp--fail))
  end)

(defun evil-sp--new-beginning (beg end &optional shrink)
  "Return a new value for BEG if POINT is inside an empty sexp.

If SHRINK is t we try to shrink the region until it is balanced
by decrementing BEG."
  (if (not shrink)
      (min beg
           (if (sp-point-in-empty-sexp)
               (evil-sp--point-after 'sp-backward-up-sexp)
             (point-max)))

    (let ((region (s-trim (buffer-substring-no-properties beg end))))
      (unless (s-blank? region)
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

(defun evil-sp--fail ()
  "Error out with a friendly message."
  (error "Can't find a safe region to act on!"))

(provide 'evil-smartparens)
;;; evil-smartparens.el ends here
