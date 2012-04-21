;;; nunit.el --- using nunit-console2 with Emacs

;; Copyright (C) 2010  Merinov Nikolay

;; Author: Merinov Nikolay <kim.roader@gmail.com>
;; Keywords: languages

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

;; This is collection of the simple procedures, that call nunit-console2 for running nunit tests.
;; Maybe in the future it wold be released with nunit-mode, but not now.
;;
;; This file describe 3 interactive functions:
;;
;; nunit-run-all-tests -- ask witch program compiled with test and run all tests
;; nunit-run-test-by-name -- ask program name and test name and run choosed test in this program
;; nunit-run-all-tests-throught-compile -- experemental. Run all tests using `compile' module
;;
;; Installation:
;;
;; Put nunit.el into some place in your `load-path' and add (require 'nunit)
;; in your init file
;;
;; Usage:
;;
;; M-x nunit-run-all-tests    -- when you work with any file of the project
;; M-x nunit-run-test-by-name -- when you point inside of [Test] function 

;;; Code:

(defun nunit-run-tests-with-args (filename &optional arguments)
  "Call nunit-console2 with `arguments' arguments and show results"
  (let ((buffer (get-buffer-create "*unit-tests*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (buffer-disable-undo)
      (erase-buffer)
      (apply #'call-process "nunit-console2" nil t t "-nologo" "-nodots"
      	     filename arguments)
      ;; fix output
      (replace-string "\r" "" nil (point-min) (point-max))
      (replace-regexp "^\\([[:digit:]]+)[^:]*: \\)" "\n\\1\n" nil (point-min) (point-max))
      ;; colorize
      (highlight-regexp "^[[:digit:]]+).*" 'font-lock-warning-face)
      ;; other stuff
      (not-modified)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun nunit-filename (default temp)
  "if temp == default-directory, then return default. Else return temp"
  (if (string= default-directory temp)
      (expand-file-name default)
    (expand-file-name temp)))

(defun nunit-default-filename ()
  "Get nearest exe file. First in current directory, then in subdirs, then subsubdirs.
After this parent dir and subdirs of the parent dir"
  (car (or (file-expand-wildcards "*.exe")
	   (file-expand-wildcards "*/*.exe")
	   (file-expand-wildcards "*/*/*.exe")
	   (file-expand-wildcards "../*.exe")
	   (file-expand-wildcards "../*/*.exe"))))

(defun nunit-request-filename ()
  (let* ((default-filename (nunit-default-filename))
	 (formated-string (if default-filename
			      (format "Program name (default: %s): " default-filename)
			    "Program name: ")))
    (read-file-name formated-string default-directory default-filename t)))

(defun nunit-default-testname ()
  (save-excursion
    (search-backward "[Test]")
    (search-forward "(")
    (backward-char)
    (backward-sexp)
    (current-word)))

(defvar nunit-history-testname nil)
(defun nunit-request-testname ()
  (let* ((default-testname (nunit-default-testname))
	 (formated-string (if default-testname
			      (format "Test name (default: %s): " default-testname)
			    "Test name: ")))
    (read-string formated-string nil 'nunit-history-testname default-testname)))

(defun nunit-run-all-tests (filename)
  "Run all tests"
  (interactive (list (nunit-request-filename)))
    (nunit-run-tests-with-args filename))

(defun nunit-run-test-by-name (filename testname)
  "run test with name TESTNAME"
  (interactive (list (nunit-request-filename)
		     (nunit-request-testname)))
  (let ((args (list (concat "-run=" testname))))
    (nunit-run-tests-with-args filename args)))

(defun nunit-run-all-tests-throught-compile (filename)
  (interactive (list (nunit-request-filename)))
    (compile (concat "nunit-console2 -nodots -nologo " (shell-quote-argument filename)) t))

(provide 'nunit)
;;; nunit.el ends here