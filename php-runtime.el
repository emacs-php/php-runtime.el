;;; php-runtime.el --- Language binding bridge to PHP -*- lexical-binding:t -*-

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
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

;; Execute PHP code.  This package requires `php' command in runtime.
;;
;;     (string-to-number (php-runtime-eval "echo PHP_INT_MAX;"))
;;     ;; => 9.223372036854776e+18
;;
;;     (string-to-number (php-runtime-expr "PHP_INT_MAX")) ; short hand
;;


;;; Code:
(require 'cl-lib)
(require 'eieio)

(defgroup php-runtime nil
  "Language binding bridge to PHP"
  :tag "PHP Runtime"
  :group 'processes
  :group 'php)

(defcustom php-runtime-php-executable (and (executable-find "php") "php")
  "A command name or path to PHP executable."
  :group 'php-runtime
  :type 'string)

(defconst php-runtime-php-open-tag "<?php ")

(defconst php-runtime-error-buffer-name "*PHP Error Messages*")

(defvar php-runtime--kill-temp-output-buffer t)


;; PHP Execute class

;;;###autoload
(defclass php-runtime-execute nil
  ((executable :initarg :executable :type string)
   (code   :initarg :code   :type string)
   (stdin  :initarg :stdin  :type (or null buffer) :initform nil)
   (stdout :initarg :stdout :type (or null buffer list) :initform nil)
   (stderr :initarg :stderr :type (or null buffer list) :initform nil)))

(defmethod php-runtime-run-in-command-line ((php php-runtime-execute))
  "Execute PHP process."
  (call-process (oref php executable)
                (oref php stdin)
                (cons (php-runtime-stdout-buffer php)
                      (oref php stderr))
                nil ; suppress display
                (concat "-r" (oref php code))))

(defmethod php-runtime-stdout-buffer ((php php-runtime-execute))
  "Return output buffer."
  (let ((buf (oref php stdout)))
    (if (and buf (buffer-live-p buf))
        buf
      (oset php stdout (get-buffer-create (format "*PHP output %s*" (cl-gensym)))))))


;; Utility functions
(defun php-runtime--temp-buffer ()
  "Return new temp buffer."
  (get-buffer-create (format "*PHP temp %s*" (cl-gensym))))


;; PHP Execute wrapper function

;;;###autoload
(defun php-runtime-expr (php-expr &optional input-buffer)
  "Evalute and echo PHP expression `PHP-EXPR'.

Pass `INPUT-BUFFER' to PHP executable as STDIN."
  (php-runtime-eval (format "echo %s;" php-expr) input-buffer))

;;;###autoload
(defun php-runtime-eval (code &optional input-buffer)
  "Evalute PHP code `CODE' without open tag, and return buffer.

Pass `INPUT-BUFFER' to PHP executable as STDIN."
  (let ((execute (php-runtime-execute :code code
                           :executable php-runtime-php-executable
                           :stderr (get-buffer-create php-runtime-error-buffer-name))))
    (when input-buffer
      (oset execute stdin input-buffer))

    (php-runtime-run-in-command-line execute)

    (prog1 (with-current-buffer (php-runtime-stdout-buffer execute)
             (buffer-substring-no-properties (point-min) (point-max)))
      (when php-runtime--kill-temp-output-buffer
        (kill-buffer (php-runtime-stdout-buffer execute))))))

(provide 'php-runtime)
;;; php-runtime.el ends here
