(add-to-list 'load-path ".")

(require 'ert)
(require 'espuds)
(require 'jsonconfig-mode)

(Before
 (electric-indent-mode 0)
 (electric-pair-mode 0)
 (switch-to-buffer
  (get-buffer-create "*jsonconfig-mode*"))
 (setq indent-tabs-mode nil)
 (global-set-key (kbd "C-j") 'newline-and-indent)
 (jsonconfig-mode))

(After)
