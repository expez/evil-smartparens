(require 'ert)
(require 'evil-smartparens)
(require 'evil-tests)
(require 'evil-surround)

(defun evil-sp--enable-for-test (&rest _)
  (unless (eq major-mode 'emacs-lisp-mode)
    (emacs-lisp-mode))
  (smartparens-strict-mode 1)
  (evil-surround-mode 1)
  (evil-smartparens-mode 1))

(add-hook 'evil-mode-hook #'evil-sp--enable-for-test)

(ert-deftest evil-sp-test-delete-word ()
  "Test `evil-delete-word'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([T]his)"
    ("dw" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-2-word ()
  "Test `evil-delete-word' with repeat count"
  :tags '(evil-sp count)
  (evil-test-buffer
    "([T]his)"
    ("2dw" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-WORD ()
  "Test `evil-delete-WORD'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([T]his)"
    ("dW" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-2-WORD ()
  "Test `evil-delete-WORD' with repeat count"
  :tags '(evil-sp count)
  (evil-test-buffer
    "([T]his)"
    ("2dW" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-2-WORD-again ()
  "Test `evil-delete-WORD' with repeat count"
  :tags '(evil-sp count)
  (evil-test-buffer
    "([T]his that)"
    ("2dW" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-char ()
  "Test `evil-delete-char'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([x])"
    ("x" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-char-on-paren ()
  "Test `evil-delete-char' on paren"
  :tags '(evil-sp)
  (evil-test-buffer
    "[(])"
    ("x" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-backward-char ()
  "Test `evil-delete-backward-char'."
  :tags '(evil-sp)
  (evil-test-buffer
    "([x])"
    ("X" [escape])
    "(x)"))

(ert-deftest evil-sp-test-delete-backward-char-on-paren ()
  "Test `evil-delete-backward-char' on paren"
  :tags '(evil-sp)
  (evil-test-buffer
    "([)]"
    ("X" [escape])
    ""))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-kills-garb ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(foo bar)[]     ; Useless comment"
    ("D" [escape])
    "(foo bar)"))

(ert-deftest evil-sp-test-delete-line-is-greedy ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "[(]let [foo (bar baz)
           qux 1
           quux (+ 1 2)]
       (dwim foo qux quux))"
    ("D" [escape])
    ""))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-preserves-comments ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([f]oo bar)     ; Important comment"
    ("D" [escape])
    "()     ; Important comment"))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-deletes-useless-line ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "[(]foo bar)     ; Useless line"
    ("D" [escape])
    ""))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-works-in-string ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(foo \"[b]ar baz\"
           quux)"
    ("D" [escape])
    "(foo \"\"
           quux)"))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-works-in-latex-tags ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "foo ${bar[ ]baz} quux"
    ("D" [escape])
    "foo ${bar} quux"))

(ert-deftest evil-sp-test-delete-whole-line-fails-when-greed-is-futile ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(let [(](foo bar)
       (frobnicate bar)))"
    ("dd" [escape])
    (error nil)))

(ert-deftest evil-sp-test-dd-on-line-with-string ()
  "Test `evil-delete-whole-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "\"foo\" 'bar"
    ("dd" [escape])
    ""))

(ert-deftest evil-sp-test-dd-on-empty-line ()
  "Test `evil-delete-whole-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "[]
"
    ("dd" [escape])
    ""))

(ert-deftest evil-sp-test-x-on-string-pair ()
  "Test `evil-delete-char'"
  :tags '(evil-sp)
  (evil-test-buffer
    "\"[\"]"
    ("x" [escape])
    ""))

(ert-deftest evil-sp-test-dd-on-line-with-empty-string ()
  "Test `evil-delete-whole-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "\"\"[]"
    ("dd" [escape])
    ""))

(ert-deftest evil-sp-test-delete-line-not-too-greedy ()
  "Test `evil-delete-whole-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(foo[ ](bar)
  bas)"
    ("D" [escape])
    "(foo
  bas)"))

(ert-deftest evil-sp-change-works-with-evil-surround ()
  "Test compat with `evil-surround'"
  :tags '(evil-sp evil-surround)
  (evil-test-buffer
    "(f[o]o)"
    ("cs(\"" [escape])
    "\"foo\""))

(ert-deftest evil-sp-delete-works-with-evil-surround ()
  "Test compat with `evil-surround'"
  :tags '(evil-sp evil-surround)
  (evil-test-buffer
    "(f[o]o)"
    ("ds(" [escape])
    "foo"))

(ert-deftest evil-sp-evil-surround-yank-surround ()
  "Test compat with `evil-surround'"
  :tags '(evil-sp evil-surround)
  (evil-test-buffer
    "f[o]o"
    ("ysiw\"" [escape])
    "\"foo\""))

(ert-deftest evil-sp-delete-backward-word ()
  :tags '(evil-sp)
  (evil-test-buffer
    "(f[o]o)"
    ("db" [escape])
    "(oo)"))

(ert-deftest evil-sp-delete-backward-WORD()
  :tags '(evil-sp)
  (evil-test-buffer
    "(fo[o])"
    ("dB" [escape])
    "(o)"))

(ert-deftest evil-sp-delete-backward-to-bol ()
  :tags '(evil-sp)
  (evil-test-buffer
    "(foo (bar[ ]baz))"
    ("d^" [escape])
    "(foo ( baz))"))

(ert-deftest evil-sp-delete-empty-line ()
  :tags '(evil-sp)
  (evil-test-buffer
    "fo[o]
"
    ("jdd" [escape])
    "foo"))
