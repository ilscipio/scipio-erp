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
public class FormsPartyQualForms {

    @Form(
        name = "FindPartyQuals",
        location = "component://humanres/widget/forms/PartyQualForms.xml",
        target = "FindPartyQuals",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PartyQual", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyQualTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyQualType", description = "${description}", keyFieldName = "partyQualTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "HR_DEGREE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "verifStatusId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTYQUAL_VERIFY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPartyQuals {}

    @Form(
        name = "ListPartyQuals",
        location = "component://humanres/widget/forms/PartyQualForms.xml",
        type = FormType.MULTI,
        target = "updatePartyQualExt?partyId=${partyId}",
        listName = "listIt",
        paginateTarget = "FindPartyQuals",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyQual")
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "partyQualTypeId", displayEntity = @DisplayEntityField(entityName = "PartyQualType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "qualificationDesc", text = @TextField),
            @FormField(name = "title", text = @TextField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "HR_DEGREE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "verifStatusId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTYQUAL_VERIFY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", hidden = @HiddenField(value = "Y")),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyQual", description = "${uiLabelMap.CommonDelete}", alsoHidden = false))
        },
        altTargets = {
            @AltTarget(useWhen = "insideEmployee==null", target = "updatePartyQual")
        },
        actions = @FormActions(set = {@SetAction(field = "insideEmployee", fromField = "parameters.insideEmployee")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "partyQualCtx"), @FieldMap(fieldName = "entityName", value = "PartyQual"), @FieldMap(fieldName = "orderBy", value = "fromDate"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPartyQuals {}

    @Form(
        name = "AddPartyQual",
        location = "component://humanres/widget/forms/PartyQualForms.xml",
        target = "createPartyQual",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createPartyQual")
        },
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", useWhen = "partyQual!=null", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", useWhen = "partyQual==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyQualTypeId", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyQualType", description = "${description}", keyFieldName = "partyQualTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "HR_DEGREE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "verifStatusId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PARTYQUAL_VERIFY")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "insideEmployee != null", target = "createPartyQualExt")
        },
        actions = @FormActions(set = {@SetAction(field = "insideEmployee", fromField = "parameters.insideEmployee")})
    )
    public interface AddPartyQual {}

}
