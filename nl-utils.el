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

(defun nl/test-utils-fix ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "inject.*TestUtils" nil t)
    (beginning-of-line-text)
    (replace-regexp "\[,\s-\]*TestUtils\[,\s-\]*" "" nil
                    (line-beginning-position) (line-end-position))))


(defun nl/shipment-test-suite-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { ShippingComponentTestSuiteMixin } from 'test/mixins/ShippingComponentTestSuiteMixin';\n"
   '("inject.*ShippingComponentTestSuiteMixin" "\[,\s-\]*ShippingComponentTestSuiteMixin\[,\s-\]*" "")
   '("_\.extend" "Object.assign")))

(defun nl/component-test-suite-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { ComponentTestSuiteMixin } from 'test/mixins/ComponentTestSuiteMixin';\n"
   '("inject.*ComponentTestSuiteMixin" "\[,\s-\]*ComponentTestSuiteMixin\[,\s-\]*" "")
   '("_\.extend" "Object.assign")))

(defun nl/directive-test-suite-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { DirectiveTestSuiteMixin } from 'test/mixins/DirectiveTestSuiteMixin';\n"
   '("inject.*DirectiveTestSuiteMixin" "\[,\s-\]*DirectiveTestSuiteMixin\[,\s-\]*" "")
   '("_\.extend" "Object.assign")))

(defun nl/modal-test-suite-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { ModalTestSuiteMixin } from 'test/mixins/ModalTestSuiteMixin';\n"
   '("inject.*ModalTestSuiteMixin" "\[,\s-\]*ModalTestSuiteMixin\[,\s-\]*" "")
   '("_\.extend" "Object.assign")))

(defun nl/server-reply-mixin-fix ()
  (interactive)
  (nl/test-suite-mixin-fix
   "import { ServerReplyMixin } from 'test/mixins/ServerReplyMixin';\n"
   '("inject.*ServerReplyMixin" "\[,\s-\]*ServerReplyMixin\[,\s-\]*" "")
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

(nl/files-test-suite-mixin-fix
 'nl/modal-test-suite-mixin-fix
 '("app/assets/javascripts/centres/services/centreLocationsModal/centreLocationsModalServiceSpec.js"
   "app/assets/javascripts/centres/services/shipmentSkipToSentModal/shipmentSkipToSentModalServiceSpec.js"
   "app/assets/javascripts/centres/services/shipmentSkipToUnpackedModal/shipmentSkipToUnpackedModalServiceSpec.js"
   "app/assets/javascripts/common/services/modalService/modalServiceSpec.js"
   "app/assets/javascripts/common/services/asyncInputModal/asyncInputModalServiceSpec.js"
   "app/assets/javascripts/common/modules/modalInput/modalInputModuleSpec.js"
   "app/assets/javascripts/collection/services/specimenAddModal/specimenAddModalServiceSpec.js"
   "app/assets/javascripts/admin/modules/common/services/annotationTypeUpdateModal/annotationTypeUpdateModalServiceSpec.js"))

(nl/files-test-suite-mixin-fix
 'nl/server-reply-mixin-fix
 '("app/assets/javascripts/centres/components/shipmentSpecimensAdd/shipmentSpecimensAddComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentUnpack/unpackedShipmentUnpackComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentInfo/unpackedShipmentInfoComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentItems/unpackedShipmentItemsComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentExtra/unpackedShipmentExtraComponentSpec.js"
   "app/assets/javascripts/users/components/registerUser/registerUserComponentSpec.js"
   "app/assets/javascripts/users/services/usersService/userServiceSpec.js"
   "app/assets/javascripts/base/services/biobankApi/biobankApiServiceSpec.js"
   "app/assets/javascripts/domain/participants/Participant/ParticipantSpec.js"
   "app/assets/javascripts/domain/participants/CollectionEvent/CollectionEventSpec.js"
   "app/assets/javascripts/domain/access/PermissionName/PermissionNameSpec.js"
   "app/assets/javascripts/domain/access/Role/RoleSpec.js"
   "app/assets/javascripts/domain/access/UserMembership/UserMembershipSpec.js"
   "app/assets/javascripts/domain/access/Membership/MembershipSpec.js"
   "app/assets/javascripts/domain/access/RoleName/RoleNameSpec.js"
   "app/assets/javascripts/domain/access/accessItemNameFactory/accessItemNameFactorySpec.js"
   "app/assets/javascripts/domain/access/Permission/PermissionSpec.js"
   "app/assets/javascripts/domain/access/MembershipName/MembershipNameSpec.js"
   "app/assets/javascripts/domain/study/CollectionEventTypeName/CollectionEventTypeNameSpec.js"
   "app/assets/javascripts/domain/study/StudyCounts/StudyCountsSpec.js"
   "app/assets/javascripts/domain/study/CollectionEventType/CollectionEventTypeSpec.js"
   "app/assets/javascripts/domain/study/StudyName/StudyNameSpec.js"
   "app/assets/javascripts/domain/study/Study/StudySpec.js"
   "app/assets/javascripts/domain/user/UserName/UserNameSpec.js"
   "app/assets/javascripts/domain/user/User/UserSpec.js"
   "app/assets/javascripts/domain/user/UserCounts/UserCountsSpec.js"
   "app/assets/javascripts/domain/centre/CentreName/CentreNameSpec.js"
   "app/assets/javascripts/domain/centre/Shipment/ShipmentSpec.js"
   "app/assets/javascripts/domain/centre/Centre/CentreSpec.js"
   "app/assets/javascripts/domain/centre/ShipmentSpecimen/ShipmentSpecimenSpec.js"))

(nl/files-test-suite-mixin-fix
 'nl/test-utils-fix
 '("app/assets/javascripts/centres/components/shipmentAddItems/shipmentAddItemsComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewReceived/shipmentViewReceivedComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewPacked/shipmentViewPackedComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewSent/shipmentViewSentComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewCompleted/shipmentViewCompletedComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentView/unpackedShipmentViewComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentAdd/shipmentAddComponentSpec.js"
   "app/assets/javascripts/centres/services/centreLocationsModal/centreLocationsModalServiceSpec.js"
   "app/assets/javascripts/centres/services/shipmentSkipToSentModal/shipmentSkipToSentModalServiceSpec.js"
   "app/assets/javascripts/centres/services/shipmentSkipToUnpackedModal/shipmentSkipToUnpackedModalServiceSpec.js"
   "app/assets/javascripts/common/directives/updateRemoveButtons/updateRemoveButtonsDirectiveSpec.js"
   "app/assets/javascripts/domain/participants/Specimen/SpecimenSpec.js"
   "app/assets/javascripts/domain/study/ProcessingDto/ProcessingDtoSpec.js"
   "app/assets/javascripts/domain/study/SpecimenLinkType/SpecimenLinkTypeSpec.js"
   "app/assets/javascripts/domain/annotations/Annotation/AnnotationSpec.js"
   "app/assets/javascripts/collection/components/ceventGetType/ceventGetTypeComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/locationsPanel/locationsPanelComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centreStudiesPanel/centreStudiesPanelComponentSpec.js"
   "app/assets/javascripts/admin/modules/common/services/annotationTypeUpdateModal/annotationTypeUpdateModalServiceSpec.js"
   "app/assets/javascripts/admin/modules/studies/directives/specimenGroupsPanel/specimenGroupsPanelDirectiveSpec.js"
   "app/assets/javascripts/admin/modules/studies/controllers/SpcLinkTypeEdit/SpcLinkTypeEditCtrlSpec.js"
   "app/assets/javascripts/admin/modules/studies/controllers/processingTypesPanel/processingTypesPanelDirectiveSpec.js"
   "app/assets/javascripts/admin/modules/studies/controllers/spcLinkTypesPanel/spcLinkTypesPanelDirectiveSpec.js"
   "app/assets/javascripts/shipmentSpecimens/components/specimenTableAction/specimenTableActionComponentSpec.js"))
