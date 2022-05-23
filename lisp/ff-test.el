;;; ff-test.el --- Find a test/implementation file related to current file -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2019 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; Keywords: files
;; URL: https://github.com/terlar/emacs-config/blob/main/lisp/ff-test.el
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; ff-test is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ff-test is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Find related test file from implementation and vice-versa.

;;; Code:

;; TODO Use `ff-file-created-hook', if several search folders, ask where to create (e.g. unit/integration/acceptance)

(require 'find-file)
(require 'seq)

(defgroup ff-test nil
  "Find a test/implementation file related to current file."
  :group 'find-file)

(defcustom ff-test-suffixes '("_test" "_spec"
                            "-test" "-spec"
                            ".test" ".spec")
  "List of test suffixes to look associate with implementation file."
  :type '(repeat string)
  :group 'ff-test
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom ff-test-search-implementation-directories '(".")
  "List of directories to look for implementation files."
  :type '(repeat directory)
  :group 'ff-test
  :safe (lambda (x) (seq-every-p #'file-directory-p x)))

(defcustom ff-test-search-implementation-project-directories '("src")
  "List of project relative directories to look for related implementation files."
  :type '(repeat string)
  :group 'ff-test
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom ff-test-search-test-directories '(".")
  "List of directories to look for test files."
  :type '(repeat directory)
  :group 'ff-test
  :safe (lambda (x) (seq-every-p #'file-directory-p x)))

(defcustom ff-test-search-test-project-directories '("test" "spec")
  "List of project relative directories to look for associated test files."
  :type '(repeat string)
  :group 'ff-test
  :safe (lambda (x) (seq-every-p #'stringp x)))

(defcustom ff-test-project-root-function #'ff-test-project-root-from-current-project
  "Function used to get project roots."
  :type 'function
  :group 'ff-test)

(make-variable-buffer-local 'ff-test-search-implementation-directories)
(make-variable-buffer-local 'ff-test-search-implementation-project-directories)
(make-variable-buffer-local 'ff-test-search-test-directories)
(make-variable-buffer-local 'ff-test-search-test-project-directories)
(make-variable-buffer-local 'ff-test-suffixes)

(defvar-local ff-test-expanded-search-implementation-project-directories nil
  "List of expanded `ff-test-search-implementation-project-directories'.
Using `ff-test-project-root-function'.")
(defvar-local ff-test-expanded-search-test-project-directories nil
  "List of expanded `ff-test-search-test-project-directories'.
Using `ff-test-project-root-function'.")

;; Optional dependencies to set project local directories.
(autoload 'project-root "project")
(autoload 'project-current "project")

(defun ff-test-project-root-from-current-project ()
  "Use `project' to resolve project roots."
  (project-root (project-current t)))

(defun ff-test-file-regexp (extension)
  "Return regular expression for EXTENSION matching `ff-test-suffixes'."
  (concat "\\(" (mapconcat #'regexp-quote ff-test-suffixes "\\|") "\\)" "." extension "$"))

(defun ff-test-test-file-p (file-name)
  "Return t if FILE-NAME is a test file."
  (string-match-p (ff-test-file-regexp (file-name-extension file-name))
                  file-name))

;;;###autoload
(defun ff-test-project-dirs (dirs)
  "Return DIRS as a list of expanded project directories."
  (let ((root (expand-file-name (funcall ff-test-project-root-function))))
    (mapcar (lambda (dir) (concat root dir))
            dirs)))

;;;###autoload
(defun ff-test-converter (file-name)
  "Possible related test/implementation files for FILE-NAME."
  (let* ((file-ext (file-name-extension file-name))
         (file-dir (file-name-directory file-name))
         (test-regexp (ff-test-file-regexp file-ext))
         (test-file-p (ff-test-test-file-p file-name))
         file-targets
         search-directories)
    (cond
     (test-file-p
      (setq file-targets
            (list (replace-regexp-in-string test-regexp
                                            (concat "." file-ext)
                                            (file-name-nondirectory file-name)))
            search-directories ff-test-expanded-search-test-project-directories))
     (t
      (setq file-targets (mapcar (lambda (suffix)
                                   (concat (file-name-base file-name) suffix "." file-ext))
                                 ff-test-suffixes)
            search-directories ff-test-expanded-search-implementation-project-directories)))

    (apply 'append
           file-targets
           (mapcar (lambda (dir) (let ((relative-dir
                                   (replace-regexp-in-string
                                    (replace-regexp-in-string "\\\\\\*" "[^/]+" (regexp-quote (concat dir "/")))
                                    ""
                                    file-dir)))
                              (unless (string= relative-dir file-dir)
                                (mapcar (lambda (target) (concat relative-dir target))
                                        file-targets))))
                   search-directories))))

;;;###autoload
(defun ff-test-other-file-name ()
  "Return the name of the corresponding test/implementation file."
  (let* ((file-name (ff-buffer-file-name))
         (file-ext (file-name-extension file-name))
         (test-regexp (ff-test-file-regexp file-ext))
         (implementation-regxp (concat "\\." file-ext "$"))
         (test-file-p (ff-test-test-file-p file-name))
         (ff-test-expanded-search-implementation-project-directories
          (ff-test-project-dirs ff-test-search-implementation-project-directories))
         (ff-test-expanded-search-test-project-directories
          (ff-test-project-dirs ff-test-search-test-project-directories))
         (ff-search-directories
          (cond
           (test-file-p
            (append ff-test-search-implementation-directories
                    ff-test-expanded-search-implementation-project-directories))
           (t
            (append ff-test-search-test-directories
                    ff-test-expanded-search-test-project-directories))))
         (ff-other-file-alist
          `((,test-regexp ,'ff-test-converter)
            (,implementation-regxp ,'ff-test-converter))))
    (ff-find-the-other-file)))

;;;###autoload
(defun ff-test-find-other-file (&optional in-other-window)
  "Find the test or implementation file corresponding to this file.

If optional IN-OTHER-WINDOW is non-nil, find the file in the other window.

Variables of interest include:

 - `ff-test-search-implementation-directories'
   List of directories searched through for implementation files.

 - `ff-test-search-test-directories' List of directories searched
   through for test files, using each suffix specified in
   `ff-test-suffixes'."
  (interactive "P")
  (let* ((file-name (ff-buffer-file-name))
         (file-ext (file-name-extension file-name))
         (test-regexp (ff-test-file-regexp file-ext))
         (implementation-regxp (concat "\\." file-ext "$"))
         (test-file-p (ff-test-test-file-p file-name))
         (ff-test-expanded-search-implementation-project-directories
          (ff-test-project-dirs ff-test-search-implementation-project-directories))
         (ff-test-expanded-search-test-project-directories
          (ff-test-project-dirs ff-test-search-test-project-directories))
         (ff-search-directories
          (cond
           (test-file-p
            (append ff-test-search-implementation-directories
                    ff-test-expanded-search-implementation-project-directories))
           (t
            (append ff-test-search-test-directories
                    ff-test-expanded-search-test-project-directories))))
         (ff-other-file-alist
          `((,test-regexp ,'ff-test-converter)
            (,implementation-regxp ,'ff-test-converter))))
    (ff-find-other-file in-other-window t)))

(provide 'ff-test)
;;; ff-test.el ends here
