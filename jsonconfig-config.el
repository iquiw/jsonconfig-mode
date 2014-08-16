;;; jsonconfig-config --- jsonconfig-mode configuration.

;; Copyright (C) 2014 by Iku Iwasa

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

(autoload 'jsonconfig-mode "jsonconfig-mode" nil t)

(dolist (pattern '("\\.json\\'"
                   "\\.csslintrc\\'"
                   "\\.jshintrc\\'"
                   "\\.jscsrc\\'"
                   "\\.tern-\\(config\\|project\\)\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'jsonconfig-mode)))

(provide 'jsonconfig-config)
;;; jsonconfig-config.el ends here
