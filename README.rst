===========================
 JSON config mode |travis|
===========================

Emacs major mode to edit JSON format configuration, such as `package.json`_, `bower.json`_.


Feature
=======
Auto-indentation
----------------
Based on SMIE_. Comma first style is also supported.

Auto-completion
---------------
Top-level property names can be auto-completed for specific file types
by ``M-x completion-at-point`` or ``company-capf``, etc.

Supported file type by completion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* `package.json`_
* `bower.json`_

Highlighting
------------
Property name, string, number, boolean, null.

Hyperlink
---------
Button'ize URL-like strings.

Imenu
-----
Top-level property names are indexed.

Installation
============

Setup from Git
--------------
1. Install from Git::

     git clone https://github.com/iquiw/jsonconfig-mode.git

2. Setup to use ``jsonconfig-mode`` for JSON files.

   .. code:: emacs-lisp

      (add-to-list 'load-path "/path/to/jsonconfig-mode")
      (require 'jsonconfig-config)


License
=======
Licensed under the GPL 3+ license.


.. _package.json: https://www.npmjs.org/doc/package.json.html
.. _bower.json: http://bower.io/docs/creating-packages/#bowerjson
.. _SMIE: http://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html
.. |travis| image:: https://api.travis-ci.org/iquiw/jsonconfig-mode.svg?branch=master
            :target: https://travis-ci.org/iquiw/jsonconfig-mode
