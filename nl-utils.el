;;; nl-utils --- utilities

;;; Commentary:

;; Utility functions required at one point to help in refactoring.

;;; Code:

;; used to refactor Jasmine test suites
(defun nl/test-suite-mixin-fix (importText injectRegexArgs extendRegexArgs)
  (goto-char (point-min))
  (when (re-search-forward "^import" nil t)
    (beginning-of-line-text)
    (insert importText))
  (when (re-search-forward (elt injectRegexArgs 0) nil t)
    (beginning-of-line-text)
    (replace-regexp (elt injectRegexArgs 1) (elt injectRegexArgs 2) nil
                    (line-beginning-position) (line-end-position)))
  (when (re-search-forward (elt extendRegexArgs 0) nil t)
    (beginning-of-line-text)
    (replace-regexp (elt extendRegexArgs 0) (elt extendRegexArgs 1) nil
                    (line-beginning-position) (line-end-position))))


(defun nl/shipment-test-suite-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { ShippingComponentTestSuiteMixin } from 'test/mixins/ShippingComponentTestSuiteMixin';\n"
   '("inject.*ShippingComponentTestSuiteMixin" "ShippingComponentTestSuiteMixin\[,\s-\]*" "")
   '("_\.extend" "Object.assign")))

(defun nl/component-test-suite-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { ComponentTestSuiteMixin } from 'test/mixins/ComponentTestSuiteMixin';\n"
   '("inject.*ComponentTestSuiteMixin" "ComponentTestSuiteMixin\[,\s-\]*" "")
   '("_\.extend" "Object.assign")))

(defun nl/directive-test-suite-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { DirectiveTestSuiteMixin } from 'test/mixins/DirectiveTestSuiteMixin';\n"
   '("inject.*DirectiveTestSuiteMixin" "DirectiveTestSuiteMixin\[,\s-\]*" "")
   '("_\.extend" "Object.assign")))

(defun nl/files-test-suite-mixin-fix (fix-func files)
  (interactive)
  (loop for file in files do
        (progn
          (setq default-directory "/home/nelson/src/cbsr/scala/bbweb")
          (find-file file)
          (funcall fix-func)
          (save-buffer))))

(nl/files-test-suite-mixin-fix
 'nl/directive-test-suite-mixin-fix
 '("app/assets/javascripts/common/components/truncateToggle/truncateToggleComponentSpec.js"
   "app/assets/javascripts/common/components/panelButtons/panelButtonsComponentSpec.js"
   "app/assets/javascripts/common/directives/updateRemoveButtons/updateRemoveButtonsDirectiveSpec.js"
   "app/assets/javascripts/common/directives/positiveFloat/positiveFloatDirectiveSpec.js"
   "app/assets/javascripts/common/directives/infoUpdateRemoveButtons/infoUpdateRemoveButtonsDirectiveSpec.js"
   "app/assets/javascripts/common/directives/smartFloat/smartFloatDirectiveSpec.js"
   "app/assets/javascripts/common/directives/naturalNumber/naturalNumberDirectiveSpec.js"
   "app/assets/javascripts/common/directives/integer/integerDirectiveSpec.js"
   "app/assets/javascripts/users/directives/passwordCheck/passwordCheckDirectiveSpec.js"))
