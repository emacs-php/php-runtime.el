;;; php-runtime-test.el --- Unit tests for php-runtime package.

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 28 Aug 2017
;; Version: 0.2.0
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

(ert-deftest php-runtime-test-eval-with-stdin ()
  (dolist (v (list '("Input buffer as string"
                     :expected "1 apple\n2 orange\n3 banana\n"
                     :code "$i = 0;
while (($line = fgets(STDIN)) !== false) {
    echo ++$i, ' ', trim($line), \"\n\";
}"
                     :input-buf "apple\norange\nbanana")))
    (let ((description (car v))
          (expected (plist-get (cdr v) :expected))
          (code (plist-get (cdr v) :code))
          (input-buf (plist-get (cdr v) :input-buf)))
      (should (string= expected (php-runtime-eval code input-buf))))))

(ert-deftest php-runtime-test-eval-without-stdin ()
  (dolist (v (list '("No input buffer"
                     :expected ""
                     :code "$i = 0;
while (($line = fgets(STDIN)) !== false) {
    echo ++$i, ' ', trim($line), \"\n\";
}")))
    (let ((description (car v))
          (expected (plist-get (cdr v) :expected))
          (code (plist-get (cdr v) :code)))
      (should (string= expected (php-runtime-eval code))))))

(ert-deftest php-runtime-test-expr ()
  "Test that PHP expressions are evaluated and get results."
  (should (string= "200" (php-runtime-expr "(1+1)*100")))
  (should (string= "foo" (php-runtime-expr "'f' . 'oo'"))))

(ert-deftest php-runtime-test-expr-syntax-error ()
  "Test that PHP code syntax errors are reported as errors in Emacs."
  (should-error (php-runtime-expr "a:b")))

(ert-deftest php-runtime-test-has-null-byte ()
  "Test that PHP code contains null byte."
  (should (eq t (php-runtime-string-has-null-byte "a\0b"))))

(ert-deftest php-runtime-test-quote-string ()
  "Test that PHP expressions are evaluated and get results."
  (dolist (v `(("Empty string"
                :input ""
                :expected "''"
                :length 0)
               ("Just only あ"
                :input "あ"
                :expected "'あ'"
                :length 3)
               ("Just only '"
                :input "'"
                :expected "'\\\''"
                :length 1)
               ("Just only \\"
                :input "\\"
                :expected "'\\\\'"
                :length 1)
               ("Just only null"
                :input "\0"
                :expected "'\0'"
                :length 1)
               ("Just only newline (LF)"
                :input "
"
                :expected "'
'"
                :length 1)

               ))
    (let* ((description (car v))
           (expected (plist-get (cdr v) :expected))
           (length (plist-get (cdr v) :length))
           (php-literal (php-runtime-quote-string (plist-get (cdr v) :input))))
      (should (string= expected php-literal))
      (should (eq length (string-to-number
                          (php-runtime-expr (format "strlen(%s)" php-literal))))))))

(provide 'php-runtime-test)
;;; php-runtime-test.el ends here
