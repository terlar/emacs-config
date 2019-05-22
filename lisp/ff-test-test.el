;;; ff-test-test.el --- Tests for ff-test -*- coding: utf-8; -*-

;;; Code:

(load-file "ff-test.el")

(ert-deftest implementation-file-given ()
  (let ((ff-test-suffixes '("-test" "_test")))
    (should (equal (ff-test-converter "/tmp/p/src/file.el")
                   '("file-test.el" "file_test.el")))))

(ert-deftest test-file-given ()
  (let ((ff-test-suffixes '("-test")))
    (should (equal (ff-test-converter "/tmp/p/src/file-test.el")
                   '("file.el")))))

(ert-deftest implementation-file-given-using-search-directories ()
  (let ((ff-test-suffixes '("-test" "_test"))
        (ff-test-expanded-search-implementation-project-directories '("/tmp/p/src"))
        (ff-test-expanded-search-test-project-directories '("/tmp/p/test")))
    (should (equal (ff-test-converter "/tmp/p/src/path/file.el")
                   '("file-test.el" "file_test.el" "path/file-test.el" "path/file_test.el")))))

(ert-deftest test-file-with-test-extension-given-using-search-directories ()
  (let ((ff-test-suffixes '("-test" "_test"))
        (ff-test-expanded-search-implementation-project-directories '("/tmp/p/src"))
        (ff-test-expanded-search-test-project-directories '("/tmp/p/test")))
    (should (equal (ff-test-converter "/tmp/p/test/path/file-test.el")
                   '("file.el" "path/file.el")))))

(ert-deftest test-file-with-test-extension-given-using-search-in-subdirectories ()
  (let ((ff-test-suffixes '("-test" "_test"))
        (ff-test-expanded-search-implementation-project-directories '("/tmp/p/src"))
        (ff-test-expanded-search-test-project-directories '("/tmp/p/test/unit" "/tmp/p/test/integration")))
    (should (equal (ff-test-converter "/tmp/p/test/unit/path/file-test.el")
                   '("file.el" "path/file.el")))))

(provide 'ff-test-file-test)
;;; ff-test-file-test.el ends here
