(add-to-list 'load-path ".")

(require 'ert)
(require 'espuds)
(require 'jsonconfig-mode)

(defvar jsonconfig-test-imenu-output)

(Before
 (setq jsonconfig-test-imenu-output nil)
 (switch-to-buffer
  (get-buffer-create "*jsonconfig-mode*"))
 (electric-indent-mode 0)
 (electric-pair-mode 0)
 (font-lock-mode 1)
 (setq indent-tabs-mode nil)
 (global-set-key (kbd "C-j") 'newline-and-indent)
 (jsonconfig-mode))

(After)
