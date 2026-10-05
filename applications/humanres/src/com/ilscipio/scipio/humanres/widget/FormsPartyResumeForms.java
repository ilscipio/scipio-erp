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
public class FormsPartyResumeForms {

    @Form(
        name = "FindPartyResumes",
        location = "component://humanres/widget/forms/PartyResumeForms.xml",
        target = "FindPartyResumes",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PartyResume", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "contentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "resumeId", lookup = @LookupField(targetFormName = "LookupPartyResume")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPartyResumes {}

    @Form(
        name = "ListPartyResumes",
        location = "component://humanres/widget/forms/PartyResumeForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "PartyResume",
        paginate = "true",
        paginateTarget = "FindPartyResumes",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PartyResume", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "resumeId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditPartyResumes", description = "${resumeId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "resumeId"), @ParameterDef(paramName = "partyId")})),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyResume", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "resumeId"), @ParameterDef(paramName = "partyId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PartyResume"), @FieldMap(fieldName = "orderBy", value = "resumeId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPartyResumes {}

    @Form(
        name = "EditPartyResume",
        location = "component://humanres/widget/forms/PartyResumeForms.xml",
        target = "createPartyResume",
        defaultMapName = "partyResume",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "resumeId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "partyResume!=null", display = @DisplayField),
            @FormField(name = "resumeId", useWhen = "partyResume==null", requiredField = true, text = @TextField),
            @FormField(name = "contentId", lookup = @LookupField(targetFormName = "LookupContent")),
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_partyId}", useWhen = "partyId==null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_partyId}", useWhen = "partyId!=null", hidden = @HiddenField),
            @FormField(name = "resumeDate", dateTime = @DateTimeField),
            @FormField(name = "resumeText", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "partyResume==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "partyResume!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "partyResume != null", target = "updatePartyResume")
        }
    )
    public interface EditPartyResume {}

}
