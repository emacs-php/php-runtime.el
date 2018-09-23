;;; php-runtime-test.el --- Unit tests for php-runtime package.

;; Copyright (C) 2017 USAMI Kenta
;; Copyright (C) 2018 Eric James Michael Ritz

;; Authors: USAMI Kenta <tadsan@zonu.me>
;; Created: 28 Aug 2017
;; Version: 0.0.1
;; Keywords: processes php
;; URL: https://github.com/emacs-php/php-runtime.el
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; ## How to Run tests
;;
;; see https://www.gnu.org/software/emacs/manual/html_node/ert/How-to-Run-Tests.html
;;


;;; Code:

(require 'php-runtime)
(require 'ert)

(defun run-simple-php (code)
  "Utility function to run CODE for simple PHP testing.

This function accepts PHP code as a string and runs it using
whichever PHP executable is available on the system.  In order to
reduce sources of potential problems, we do not load any PHP INI
or other configurations.

The function returns the result of the code as a string."
  ;; We call PHP with two options:
  ;;
  ;; 1. `-n` - Causes PHP to ignore any system-wide INI, because we
  ;;    don't want any such configuration screwing up our tests.
  ;;
  ;; 2. `-r` - Tells PHP to run the code which follows, which we must
  ;;    quote appropriately as a shell argument.
  (shell-command-to-string (concat "php -n -r" (shell-quote-argument code))))

(ert-deftest runtime-expressions ()
  "Test using `php-runtime-expr'.

We can make explicit invocations of the PHP executable to obtain
the values of various runtime expressions.  These tests compare
those to the values we obtain via `php-runtime-expr', which
should always be the same."
  ;; Testing simple constants.
  (should (string= (php-runtime-expr "PHP_VERSION")
		   (run-simple-php "echo PHP_VERSION;")))
  ;; Testing simple function calls.
  (should (string= (php-runtime-expr "strtoupper('foobar')")
		   (run-simple-php "echo strtoupper('foobar');")))
  ;; Testing basic math.
  (should (string= "200" (php-runtime-expr "(1+1)*100")))
  ;; Testing string concatenation.
  (should (string= "foo" (php-runtime-expr "'f' . 'oo'")))
  ;; Converting output to numbers.
  (should (= (string-to-number (php-runtime-expr "PHP_INT_MAX"))
	     (string-to-number (run-simple-php "echo PHP_INT_MAX;")))))

(ert-deftest php-syntax-errors ()
  "Test that PHP code with errors cause appropriate failures."
  (should-error (php-runtime-expr "a:b")))

(provide 'php-runtime-test)
;;; php-runtime-test.el ends here
