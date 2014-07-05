;;; jsonconfig-mode.el --- Major mode to edit JSON configuration

;; Copyright (C) 2014 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/jsonconfig-mode
;; Version:   0.0.0
;; Package-Requires: ((emacs "23.3"))
;; Keywords:  json
;; Stability: unstable

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'smie)

(defgroup jsonconfig nil
  "Major mode to edit JSON configuration."
  :group 'languages)

(defcustom jsonconfig-basic-offset 2
  "Basic indent offset for JSON."
  :type 'integer
  :group 'jsonconfig)

(defcustom jsonconfig-mode-hook nil
  "Hook called after `jsonconfig-mode' is initialized."
  :type 'hook
  :group 'jsonconfig)

(defconst jsonconfig-grammer
  (smie-prec2->grammar
    (smie-bnf->prec2
     '((prim)
       (object ("{" pairs "}"))
       (pairs (pair "," pairs) (pair))
       (pair (prim ":" elem))
       (array ("[" elems "]"))
       (elems (elem "," elems) (elem))
       (elem (object) (array) (prim)))
     '((assoc ","))
     '((assoc ":")))))

(defvar jsonconfig-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?[ "(]" table)
    (modify-syntax-entry ?] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table))

(defconst jsonconfig-font-lock-keywords
  (list
   (cons (regexp-opt '("null" "true" "false")) font-lock-keyword-face)
   ;; number
   '("-?[[:digit:]]+\\(\\.[[:digit:]]+\\)?\\([Ee][-+]?[[:digit:]]+\\)?"
     . font-lock-constant-face)
   ;; object key
   '("\\(?:^[[:space:]]*\\|[{,][[:space:]]*\\)\\(\"[^\"]*\"\\)[[:space:]]*:"
     1 font-lock-variable-name-face t)))

(defun jsonconfig-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) jsonconfig-basic-offset)
    (`(,_ . ",") (smie-rule-separator kind))))

;;;###autoload
(define-derived-mode jsonconfig-mode prog-mode "JSON config"
  "Major mode to edit JSON configuration."
  (set-syntax-table jsonconfig-syntax-table)
  (setq-local comment-start "")
  (smie-setup jsonconfig-grammer 'jsonconfig-smie-rules)
  (setq font-lock-defaults (list jsonconfig-font-lock-keywords))
  (run-hooks 'jsonconfig-mode-hook))

(provide 'jsonconfig-mode)
;;; jsonconfig-mode.el ends here
