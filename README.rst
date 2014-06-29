==================
 JSON config mode
==================

Emacs major mode to edit JSON format configuration, such as `package.json`_.


Feature
=======
* Auto-indentation based on SMIE_.
* Highlighting.


Installation
============

Setup from Git
--------------
1. Install from Git::

     git clone https://github.com/iquiw/jsonconfig-mode.git

2. Setup to use ``jsonconfig-mode`` for JSON files.

   .. code:: emacs-lisp

      (add-to-list 'load-path "/path/to/jsonconfig-mode")
      (add-to-list 'auto-mode-alist '(".json\\'" . jsonconfig-mode))


License
=======
Licensed under the GPL 3+ license.


.. _package.json: https://www.npmjs.org/doc/package.json.html
.. _SMIE: http://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html
