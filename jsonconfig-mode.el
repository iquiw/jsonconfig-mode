;;; jsonconfig-mode.el --- Major mode to edit JSON configuration

;; Copyright (C) 2014-2018 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/jsonconfig-mode
;; Version:   0.0.0
;; Package-Requires: ((emacs "24"))
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

(require 'button)
(require 'smie)
(require 'jsonconfig-spec)

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
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table))

(defconst jsonconfig-number-regexp
  "-?[[:digit:]]+\\(\\.[[:digit:]]+\\)?\\([Ee][-+]?[[:digit:]]+\\)?")

(defconst jsonconfig-property-name-regexp
  (concat
   "^[[:space:]]*[{,]?[[:space:]]*"
   "\\(\"\\(?:\\\\\"\\\|\\(?:[^\\]\\\\\"\\|[^\"]\\)*\\)\"\\)"
   "[[:space:]]*:"))

(defconst jsonconfig-url-regexp
  "\\(https?\\|git\\)://\\([][]\\|[-[:alnum:]%._~:/?#@!$&'()*+,;=]\\)+")

(defconst jsonconfig-font-lock-keywords
  (list
   (cons (regexp-opt '("null" "true" "false")) font-lock-keyword-face)
   (cons jsonconfig-number-regexp font-lock-constant-face)
   (list jsonconfig-property-name-regexp 1 font-lock-variable-name-face t)
   (list 'jsonconfig--activate-link 0 ''link t)))

(defvar jsonconfig-file-type nil)
(make-variable-buffer-local 'jsonconfig-file-type)
(defvar jsonconfig-file-spec nil)
(make-variable-buffer-local 'jsonconfig-file-spec)

(defun jsonconfig--activate-link (limit)
  "Activate URL like string from the current point to LIMIT as button."
  (when (re-search-forward jsonconfig-url-regexp limit t)
    (make-button (match-beginning 0) (match-end 0)
                 'action 'jsonconfig--open-link)
    t))

(defun jsonconfig--open-link (button)
  "Open URL specified by BUTTON by browser."
  (browse-url (buffer-substring-no-properties
               (button-start button) (button-end button))))

(defun jsonconfig--smie-rules (kind token)
  "Specify SMIE rules for JSON according to KIND and TOKEN."
  (pcase (cons kind token)
    (`(:elem . args) (- (current-indentation) (current-column)))
    (`(:elem . basic) jsonconfig-basic-offset)
    (`(,_ . ",") (let ((smie-rule-separator-outdent jsonconfig-basic-offset))
                   (smie-rule-separator kind)))))

(defun jsonconfig--buffer-object-p ()
  "Return whether the buffer is JSON object."
  (goto-char (point-min))
  (skip-syntax-forward " ")
  (looking-at-p "{"))

(defun jsonconfig-create-imenu-index ()
  "Create `imenu' index from top level property names."
  (when (jsonconfig--buffer-object-p)
    (let (result)
      (while (re-search-forward jsonconfig-property-name-regexp nil t)
        (when (= 1 (nth 0 (syntax-ppss)))
          (let ((s (match-string-no-properties 1))
                (p (match-beginning 1)))
            (push (cons (substring s 1 (- (length s) 1)) p) result))))
      (list (cons "Variables" (nreverse result))))))

(defun jsonconfig--guess-file-type ()
  "Guess known JSON file type from the name."
  (let* ((path (buffer-file-name))
         (file (and path (file-name-nondirectory path))))
    (cond
     ((string= file "package.json")
      (setq jsonconfig-file-type 'package-json)
      (setq jsonconfig-file-spec (jsonconfig-spec "package-json")))
     ((string= file "bower.json")
      (setq jsonconfig-file-type 'bower-json)
      (setq jsonconfig-file-spec (jsonconfig-spec "bower-json")))
     (t
      (setq jsonconfig-file-type nil)))))

(defun jsonconfig-completion-at-point ()
  "Complete JSON object property name according to JSON file type."
  (when jsonconfig-file-type
    (let ((ppss (syntax-ppss)))
      (when (and (= (nth 0 ppss) 1)
                 (looking-back "[{,][[:space:]\n]*\"\\(\\|[[:word:]]+\\>\\)" nil))
        (list (match-beginning 1)
              (point)
              jsonconfig-file-spec)))))

(defun jsonconfig-find-next-property ()
  (let ((ppss (syntax-ppss)))
    (when (nth 3 ppss)
      (goto-char (nth 8 ppss)))))

(defun jsonconfig-forward-token ()
  (skip-syntax-forward " ")
  (cond
   ((looking-at (regexp-opt '("{" "[" ":" ",")))
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at "\"")
    (buffer-substring-no-properties
     (point)
     (progn
       (forward-sexp)
       (point))))))

;;;###autoload
(define-derived-mode jsonconfig-mode prog-mode "JSON config"
  "Major mode to edit JSON configuration."
  (set-syntax-table jsonconfig-syntax-table)
  (set (make-local-variable 'comment-start) "")
  (smie-setup jsonconfig-grammer #'jsonconfig--smie-rules)
  (setq font-lock-defaults (list jsonconfig-font-lock-keywords))
  (setq imenu-create-index-function #'jsonconfig-create-imenu-index)
  (jsonconfig--guess-file-type)
  (add-hook 'completion-at-point-functions
            #'jsonconfig-completion-at-point nil t))

(provide 'jsonconfig-mode)
;;; jsonconfig-mode.el ends here
