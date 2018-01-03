;;; jsonconfig-spec --- jsonconfig-mode defintion for specific JSON files.

;; Copyright (C) 2014-2018 by Iku Iwasa

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

(defun jsonconfig-spec (type)
  "Return JSON spec for given TYPE."
  (let ((sym (intern (concat "jsonconfig-spec-" type))))
    (when (boundp sym) (symbol-value sym))))

;; package.json
;; https://github.com/npm/npm/blob/master/doc/files/package.json.md
(defconst jsonconfig-spec-package-json
  '(("name" :type (string))
    ("version" :type (string))
    ("description" :type (string))
    ("keywords" :type ((string)))
    ("homepage" :type (string))
    ("bugs" :type (string object))
    ("license" :type (string))
    ("author" :type (string object))
    ("contributors" :type ((string object)))
    ("maintainers" :type ((string object)))
    ("files" :type ((string)))
    ("main" :type (string))
    ("bin" :type (string object))
    ("man" :type (string (string)))
    ("repository" :type (object))
    ("scripts" :type (object))
    ("config" :type (object))
    ("dependencies" :type (object))
    ("devDependencies" :type (object))
    ("peerDependencies" :type (object))
    ("bundledDependencies" :type ((string)))
    ("optionalDependencies" :type (object))
    ("engines" :type (object))
    ("engineStrict" :type (boolean))
    ("os" :type ((string)))
    ("cpu" :type ((string)))
    ("preferGlobal" :type (boolean))
    ("private" :type (boolean))
    ("publishConfig" :type (object))
    ))

;; bower.json
;; https://github.com/bower/bower.json-spec
(defconst jsonconfig-spec-bower-json
  '(("name" :type (string))
    ("description" :type (string))
    ("version" :type ())
    ("main" :type (string (string)))
    ("license" :type (string))
    ("ignore" :type ((string)))
    ("keywords" :type ((string)))
    ("authors" :type ((string) (object)))
    ("homepage" :type (string))
    ("repository" :type (object))
    ("dependencies" :type (object))
    ("devDependencies" :type (object))
    ("resolutions" :type (object))
    ("private" :type (boolean))
    ))

(provide 'jsonconfig-spec)
;;; jsonconfig-spec.el ends here
