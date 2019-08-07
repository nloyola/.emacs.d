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


(defun nl/scalatest-timestamp-fix ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "(beEntityWithTimeAddedWithinSeconds" nil t)
    (beginning-of-line-text)
    (replace-string "(beEntityWithTimeAddedWithinSeconds" "beEntityWithTimeStamps" nil (line-beginning-position) (line-end-position))
    (replace-regexp ",.*and" ", " nil (line-beginning-position) (line-end-position))
    (hungry-delete-forward 1)
    (beginning-of-line-text)
    (replace-string "beEntityWithTimeModifiedWithinSeconds(" "" nil (line-beginning-position) (line-end-position))
    (move-end-of-line nil)
    (hungry-delete-backward 1)))

(defun nl/files-scalatest-timestamp-fix (fix-func files)
  (interactive)
  (loop for file in files do
        (progn
          (setq default-directory "/home/nelson/src/cbsr/scala/bbweb")
          (find-file file)
          (funcall fix-func)
          (save-buffer))))

(nl/files-scalatest-timestamp-fix
 'nl/scalatest-timestamp-fix
 '("test/org/biobank/domain/users/UserSpec.scala"
   "test/org/biobank/domain/JsonHelper.scala"
   "test/org/biobank/domain/participants/CollectionEventSpec.scala"
   "test/org/biobank/domain/participants/SpecimenSpec.scala"
   "test/org/biobank/domain/containers/ContainerSchemaSpec.scala"
   "test/org/biobank/domain/studies/StudySpec.scala"
   "test/org/biobank/domain/studies/ProcessingTypeSpec.scala"
   "test/org/biobank/domain/studies/CollectionEventTypeSpec.scala"
   "test/org/biobank/controllers/centres/CentresControllerSpec.scala"
   "test/org/biobank/controllers/centres/ShipmentSpecimensControllerSpec.scala"
   "test/org/biobank/controllers/centres/ShipmentsControllerSpec.scala"
   "test/org/biobank/controllers/users/UsersControllerSpec.scala"
   "test/org/biobank/controllers/participants/ParticipantsControllerSpec.scala"
   "test/org/biobank/controllers/participants/SpecimensControllerSpec.scala"
   "test/org/biobank/controllers/participants/CollectionEventsControllerSpec.scala"
   "test/org/biobank/controllers/access/AccessControllerMembershipSpec.scala"
   "test/org/biobank/controllers/access/AccessControllerSpec.scala"
   "test/org/biobank/controllers/studies/CeventTypesControllerSpec.scala"
   "test/org/biobank/controllers/studies/StudiesControllerSpec.scala"))


(defun nl/jssuite-create-controller-fix ()
  (interactive)
  (goto-char (point-min))
  ;;(while (re-search-forward "ComponentTestSuiteMixin.createController.call" nil t)
  (re-search-forward "TestSuiteMixin.createController.call" nil t)
  (beginning-of-line-text)
  ;; (replace-regexp "\\w+ComponentTestSuiteMixin\\.createController\\.call(\n\\s-+this,\n"
  ;;                 "this.createControllerInternal(\n"
  ;;                 nil
  ;;                 (line-beginning-position)
  ;;                 (line-end-position 3))

  ;; (replace-regexp "DirectiveTestSuiteMixin\\.createController\\.call(\n\\s-+this,\s-+\n"
  ;;                 "this.createControllerInternal("
  ;;                 nil
  ;;                 (line-beginning-position)
  ;;                 (line-end-position 3)))

  (replace-regexp "DirectiveTestSuiteMixin\\.createController\\.call(\n\\s-+this,\n"
                  "this.createControllerInternal(\n"
                  nil
                  (line-beginning-position)
                  (line-end-position 3)))

(defun nl/files-jssuite-create-controller-fix (fix-func files)
  (interactive)
  (loop for file in files do
        (progn
          (setq default-directory "/home/nelson/src/cbsr/scala/bbweb")
          (find-file file)
          (funcall fix-func)
          (save-buffer))))

(nl/files-jssuite-create-controller-fix
 'nl/jssuite-create-controller-fix
 '("app/assets/javascripts/centres/components/shipmentViewLost/shipmentViewLostComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentSpecimensAdd/shipmentSpecimensAddComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentAddItems/shipmentAddItemsComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewReceived/shipmentViewReceivedComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentUnpack/unpackedShipmentUnpackComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentsOutgoing/shipmentsOutgoingComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewPacked/shipmentViewPackedComponentSpec.js"
   "app/assets/javascripts/centres/components/selectCentre/selectCentreComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewSent/shipmentViewSentComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentViewCompleted/shipmentViewCompletedComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentsIncoming/shipmentsIncomingComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentView/unpackedShipmentViewComponentSpec.js"
   "app/assets/javascripts/centres/components/centreShipments/centreShipmentsComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentsTable/shipmentsTableComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentInfo/unpackedShipmentInfoComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentItems/unpackedShipmentItemsComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentAdd/shipmentAddComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentsCompleted/shipmentsCompletedComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentView/shipmentViewComponentSpec.js"
   "app/assets/javascripts/centres/components/shippingInfoView/shippingInfoViewComponentSpec.js"
   "app/assets/javascripts/centres/components/unpackedShipmentExtra/unpackedShipmentExtraComponentSpec.js"
   "app/assets/javascripts/centres/components/shipmentSpecimensView/shipmentSpecimensViewComponentSpec.js"
   "app/assets/javascripts/centres/components/specimenTableAction/specimenTableActionComponentSpec.js"
   "app/assets/javascripts/common/components/tagsList/tagsListComponentSpec.js"
   "app/assets/javascripts/common/components/dateTimePicker/dateTimePickerComponentSpec.js"
   "app/assets/javascripts/common/components/collapsiblePanel/collapsiblePanelComponentSpec.js"
   "app/assets/javascripts/common/components/debouncedTextInput/debouncedTextInputComponentSpec.js"
   "app/assets/javascripts/common/components/statusLine/statusLineComponentSpec.js"
   "app/assets/javascripts/common/components/tagsInput/tagsInputComponentSpec.js"
   "app/assets/javascripts/common/components/breadcrumbs/breadcrumbsComponentSpec.js"
   "app/assets/javascripts/common/components/progressTracker/progressTrackerComponentSpec.js"
   "app/assets/javascripts/common/components/nameAndStateFilters/nameAndStateFiltersComponentSpec.js"
   "app/assets/javascripts/common/components/nameEmailStateFilters/nameEmailStateFiltersComponentSpec.js"
   "app/assets/javascripts/common/components/nameFilter/nameFilterComponentSpec.js"
   "app/assets/javascripts/users/components/passwordSent/passwordSentComponentSpec.js"
   "app/assets/javascripts/users/components/login/loginComponentSpec.js"
   "app/assets/javascripts/users/components/forgotPassword/forgotPasswordComponentSpec.js"
   "app/assets/javascripts/collection/components/specimenView/specimenViewComponentSpec.js"
   "app/assets/javascripts/collection/components/ceventsAddAndSelect/ceventsAddAndSelectComponentSpec.js"
   "app/assets/javascripts/collection/components/ceventsList/ceventsListComponentSpec.js"
   "app/assets/javascripts/collection/components/ceventGetType/ceventGetTypeComponentSpec.js"
   "app/assets/javascripts/collection/components/selectStudy/selectStudyComponentSpec.js"
   "app/assets/javascripts/collection/components/ceventAdd/ceventAddComponentSpec.js"
   "app/assets/javascripts/collection/components/participantView/participantViewComponentSpec.js"
   "app/assets/javascripts/collection/components/participantSummary/participantSummaryComponentSpec.js"
   "app/assets/javascripts/collection/components/participantGet/participantGetComponentSpec.js"
   "app/assets/javascripts/collection/components/ceventSpecimensView/ceventSpecimensViewComponentSpec.js"
   "app/assets/javascripts/collection/components/participantAdd/participantAddComponentSpec.js"
   "app/assets/javascripts/collection/components/ceventView/ceventViewComponentSpec.js"
   "app/assets/javascripts/test/mixins/DirectiveTestSuiteMixin.js"
   "app/assets/javascripts/home/components/contact/contactComponentSpec.js"
   "app/assets/javascripts/home/components/about/aboutComponentSpec.js"
   "app/assets/javascripts/home/components/biobankFooter/biobankFooterComponentSpec.js"
   "app/assets/javascripts/home/components/biobankHeader/biobankHeaderComponentSpec.js"
   "app/assets/javascripts/home/components/home/homeComponentSpec.js"
   "app/assets/javascripts/home/components/resourceNotFound/resourceNotFoundComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/locationsPanel/locationsPanelComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centreSummary/centreSummaryComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centreLocationView/centreLocationViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centresAdmin/centresAdminComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centreStudiesPanel/centreStudiesPanelComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centresPagedList/centresPagedListComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centreView/centreViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centreLocationAdd/centreLocationAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/centres/components/centreAdd/centreAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/common/components/annotationTypeSummary/annotationTypeSummaryComponentSpec.js"
   "app/assets/javascripts/admin/modules/common/components/annotationTypeView/annotationTypeViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/common/components/annotationTypeAdd/annotationTypeAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/common/components/locationAdd/locationAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/common/components/biobankAdmin/biobankAdminComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/userProfile/userProfileComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/membershipView/membershipViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/userAdmin/userAdminComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/membershipAdd/membershipAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/membershipAdmin/membershipAdminComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/usersPagedList/usersPagedListComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/membershipsPagedList/membershipsPagedListComponentSpec.js"
   "app/assets/javascripts/admin/modules/users/components/manageUsers/manageUsersComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/collectionEventAnnotationTypeView/collectionEventAnnotationTypeViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/studyParticipantsTab/studyParticipantsTabComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/collectionSpecimenDefinitionView/collectionSpecimenDefinitionViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/inputSpecimenProcessingSummary/inputSpecimenProcessingSummaryComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeAdd/processingTypeInputComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeAdd/processingTypeInformationComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/studiesPagedList/studiesPagedListComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeAdd/processingTypeAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/participantAnnotationTypeAdd/participantAnnotationTypeAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/ceventTypesAddAndSelect/ceventTypesAddAndSelectComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeInputForm/processingTypeInputFormComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeAnnotationTypeView/processingTypeAnnotationTypeViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeOutputForm/processingTypeOutputFormComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeAnnotationTypeAdd/processingTypeAnnotationTypeAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/collectionEventAnnotationTypeAdd/collectionEventAnnotationTypeAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/collectionSpecimenDefinitionAdd/collectionSpecimenDefinitionAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/outputSpecimenProcessingSummary/outputSpecimenProcessingSummaryComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/participantAnnotationTypeView/participantAnnotationTypeViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/studyView/studyViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/ceventTypeView/ceventTypeViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/ceventTypeAdd/ceventTypeAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeAddAndSelect/processingTypesAddAndSelectComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/collectionSpecimenDefinitionSummary/collectionSpecimenDefinitionSummaryComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/studyAdd/studyAddComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/processingTypeView/processingTypeViewComponentSpec.js"
   "app/assets/javascripts/admin/modules/studies/components/studySummary/studySummaryComponentSpec.js"
   "app/assets/javascripts/common/components/truncateToggle/truncateToggleComponentSpec.js"
   "app/assets/javascripts/common/components/panelButtons/panelButtonsComponentSpec.js"
   "app/assets/javascripts/common/directives/updateRemoveButtons/updateRemoveButtonsDirectiveSpec.js"
   "app/assets/javascripts/common/directives/positiveFloat/positiveFloatDirectiveSpec.js"
   "app/assets/javascripts/common/directives/infoUpdateRemoveButtons/infoUpdateRemoveButtonsDirectiveSpec.js"
   "app/assets/javascripts/common/directives/smartFloat/smartFloatDirectiveSpec.js"
   "app/assets/javascripts/common/directives/naturalNumber/naturalNumberDirectiveSpec.js"
   "app/assets/javascripts/common/directives/integer/integerDirectiveSpec.js"
   "app/assets/javascripts/common/directives/str2integer/str2integerDirectiveSpec.js"
   "app/assets/javascripts/users/directives/passwordCheck/passwordCheckDirectiveSpec.js"))
