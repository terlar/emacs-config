;;; ff-test-test.el --- Tests for ff-test -*- coding: utf-8; -*-

;;; Code:

(load-file "ff-test.el")

(ert-deftest implementation-file-given ()
  (should (equal (ff-test-converter "/tmp/p/src/file.ts")
                 '("file.test.ts" "file.spec.ts"))))

(ert-deftest test-file-with-test-extension-given ()
  (should (equal (ff-test-converter "/tmp/p/src/file.test.ts")
                 '("file.ts"))))

(ert-deftest test-file-with-spec-extension-given ()
  (should (equal (ff-test-converter "/tmp/p/src/file.spec.ts")
                 '("file.ts"))))

(ert-deftest implementation-file-given-using-search-directories ()
  (should (equal (let ((ff-search-directories '("." "/tmp/p/src" "/tmp/p/tests" "/tmp/p/tests/*")))
                   (ff-test-converter "/tmp/p/src/path/file.ts"))
                 '("file.test.ts" "file.spec.ts" "path/file.test.ts" "path/file.spec.ts"))))

(ert-deftest test-file-with-test-extension-given-using-search-directories ()
  (should (equal (let ((ff-search-directories '("." "/tmp/p/src" "/tmp/p/tests" "/tmp/p/tests/*")))
                   (ff-test-converter "/tmp/p/tests/unit/path/file.test.ts"))
                 '("file.ts" "unit/path/file.ts" "path/file.ts"))))

(ert-deftest test-file-with-spec-extension-given-using-search-directories ()
  (should (equal (let ((ff-search-directories '("." "/tmp/p/src" "/tmp/p/specs" "/tmp/p/specs/*")))
                   (ff-test-converter "/tmp/p/specs/unit/path/file.spec.ts"))
                 '("file.ts" "unit/path/file.ts" "path/file.ts"))))

(provide 'ff-test-file-test)
;;; ff-test-file-test.el ends here
