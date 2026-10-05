/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package com.ilscipio.scipio.humanres.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FormsPerfReviewForms {

    @Form(
        name = "FindPerfReviews",
        location = "component://humanres/widget/forms/PerfReviewForms.xml",
        target = "FindPerfReviews",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PerfReview", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "perfReviewId", lookup = @LookupField(targetFormName = "LookupPerfReview")),
            @FormField(name = "employeeRoleTypeId", hidden = @HiddenField),
            @FormField(name = "managerRoleTypeId", hidden = @HiddenField),
            @FormField(name = "employeePartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "managerPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "paymentId", title = "${uiLabelMap.FormFieldTitle_paymentId}", lookup = @LookupField(targetFormName = "LookupPayment")),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.FormFieldTitle_emplPositionId}", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPerfReviews {}

    @Form(
        name = "ListPerfReviews",
        location = "component://humanres/widget/forms/PerfReviewForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "PerfReview",
        paginate = "true",
        paginateTarget = "FindPerfReviews",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PerfReview", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "employeePartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${lastName} ${firstName} ${middleName}", subHyperlink = @SubHyperlink(target = "EmployeeProfile?partyId=${employeePartyId}", description = "[${employeePartyId}]"))),
            @FormField(name = "employeeRoleTypeId", ignored = @IgnoredField),
            @FormField(name = "managerPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${lastName} ${firstName} ${middleName}", subHyperlink = @SubHyperlink(target = "EmployeeProfile?partyId=${managerPartyId}", description = "[${managerPartyId}]"))),
            @FormField(name = "managerRoleTypeId", ignored = @IgnoredField),
            @FormField(name = "perfReviewId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditPerfReview", description = "${perfReviewId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "perfReviewId"), @ParameterDef(paramName = "employeePartyId"), @ParameterDef(paramName = "employeeRoleTypeId")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PerfReview"), @FieldMap(fieldName = "orderBy", value = "perfReviewId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPerfReviews {}

    @Form(
        name = "EditPerfReview",
        location = "component://humanres/widget/forms/PerfReviewForms.xml",
        target = "updatePerfReview",
        defaultMapName = "perfReview",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePerfReview", mapName = "perfReview")
        },
        fields = {
            @FormField(name = "perfReviewId", useWhen = "perfReview==null", text = @TextField),
            @FormField(name = "perfReviewId", useWhen = "perfReview!=null", ignored = @IgnoredField),
            @FormField(name = "employeePartyId", useWhen = "perfReview==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "employeePartyId", useWhen = "perfReview!=null", ignored = @IgnoredField),
            @FormField(name = "employeeRoleTypeId", hidden = @HiddenField(value = "EMPLOYEE")),
            @FormField(name = "managerPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "managerRoleTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "paymentId", title = "${uiLabelMap.FormFieldTitle_paymentId}", lookup = @LookupField(targetFormName = "LookupPayment")),
            @FormField(name = "emplPositionId", title = "${uiLabelMap.FormFieldTitle_emplPositionId}", lookup = @LookupField(targetFormName = "LookupEmplPosition")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "perfReview==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "perfReview!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "perfReview==null", target = "createPerfReview")
        }
    )
    public interface EditPerfReview {}

    @Form(
        name = "ListPerfReviewItems",
        location = "component://humanres/widget/forms/PerfReviewForms.xml",
        type = FormType.LIST,
        target = "updatePerfReviewItem",
        title = "updatePerfReviewItem",
        paginateTarget = "findPerfReviewItems",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePerfReviewItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "employeePartyId", ignored = @IgnoredField),
            @FormField(name = "employeeRoleTypeId", ignored = @IgnoredField),
            @FormField(name = "perfReviewId", ignored = @IgnoredField),
            @FormField(name = "perfReviewItemTypeId", displayEntity = @DisplayEntityField(entityName = "PerfReviewItemType")),
            @FormField(name = "perfRatingTypeId", displayEntity = @DisplayEntityField(entityName = "PerfRatingType")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePerfReviewItem", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "perfReviewId"), @ParameterDef(paramName = "employeePartyId"), @ParameterDef(paramName = "employeeRoleTypeId"), @ParameterDef(paramName = "perfReviewItemSeqId")}))
        }
    )
    public interface ListPerfReviewItems {}

    @Form(
        name = "AddPerfReviewItem",
        location = "component://humanres/widget/forms/PerfReviewForms.xml",
        target = "createPerfReviewItem",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPerfReviewItem")
        },
        fields = {
            @FormField(name = "perfReviewId", hidden = @HiddenField),
            @FormField(name = "perfReviewItemSeqId", ignored = @IgnoredField),
            @FormField(name = "employeePartyId", hidden = @HiddenField),
            @FormField(name = "employeeRoleTypeId", hidden = @HiddenField),
            @FormField(name = "perfReviewItemTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PerfReviewItemType", description = "${description}", keyFieldName = "perfReviewItemTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "perfRatingTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PerfRatingType", description = "${description}", keyFieldName = "perfRatingTypeId", orderBy = {@EntityOrderBy(fieldName = "-perfRatingTypeId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPerfReviewItem {}

}
