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
package com.ilscipio.scipio.party.widget;

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
public class PartymgrPartyClassificationForms {

    @Form(
        name = "ListPartyClassifications",
        location = "component://party/widget/partymgr/PartyClassificationForms.xml",
        type = FormType.LIST,
        target = "updatePartyClassification",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField),
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", displayEntity = @DisplayEntityField(entityName = "PartyClassificationGroup", keyFieldName = "partyClassificationGroupId", description = "${description} [${partyClassificationGroupId}]")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyClassification", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyClassificationGroupId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListPartyClassifications {}

    @Form(
        name = "ListPartyClassificationGroups",
        location = "component://party/widget/partymgr/PartyClassificationForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditPartyClassificationGroup", description = "${partyClassificationGroupId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyClassificationGroupId")})),
            @FormField(name = "partyClassificationTypeId", displayEntity = @DisplayEntityField(entityName = "PartyClassificationType", keyFieldName = "partyClassificationTypeId", description = "${description}")),
            @FormField(name = "parentGroupId", display = @DisplayField),
            @FormField(name = "description", title = " ", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyClassificationGroup", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyClassificationGroupId")}))
        }
    )
    public interface ListPartyClassificationGroups {}

    @Form(
        name = "EditPartyClassificationGroup",
        location = "component://party/widget/partymgr/PartyClassificationForms.xml",
        target = "updatePartyClassificationGroup",
        defaultMapName = "partyClassificationGroup",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "partyClassificationGroup!=null", display = @DisplayField),
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", useWhen = "partyClassificationGroup==null&&partyClassificationGroupId==null", ignored = @IgnoredField),
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${partyClassificationGroupId}]", useWhen = "partyClassificationGroup==null&&partyClassificationGroupId!=null", display = @DisplayField),
            @FormField(name = "partyClassificationTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyClassificationType", description = "${description}", keyFieldName = "partyClassificationTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "parentGroupId", lookup = @LookupField(targetFormName = "LookupPartyClassificationGroup")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField(size = 55)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "partyClassificationGroup==null", target = "createPartyClassificationGroup")
        }
    )
    public interface EditPartyClassificationGroup {}

    @Form(
        name = "AddPartyClassification",
        location = "component://party/widget/partymgr/PartyClassificationForms.xml",
        target = "createPartyClassification",
        defaultMapName = "partyClassification",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField),
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyClassificationGroup", description = "${description}", keyFieldName = "partyClassificationGroupId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddPartyClassification {}

    @Form(
        name = "AddPartyClassificationParty",
        location = "component://party/widget/partymgr/PartyClassificationForms.xml",
        target = "createPartyClassificationParty",
        defaultMapName = "partyClassification",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", displayEntity = @DisplayEntityField(entityName = "PartyClassificationGroup", keyFieldName = "partyClassificationGroupId", description = "${description}")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddPartyClassificationParty {}

    @Form(
        name = "ListPartyClassificationGroupParties",
        location = "component://party/widget/partymgr/PartyClassificationForms.xml",
        type = FormType.LIST,
        target = "updatePartyClassificationParty",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.PartyClassificationGroupId}", displayEntity = @DisplayEntityField(entityName = "PartyClassificationGroup", keyFieldName = "partyClassificationGroupId", description = "${description} [${partyClassificationGroupId}]")),
            @FormField(name = "partyId", title = "${uiLabelMap.Party}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyClassification", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyClassificationGroupId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListPartyClassificationGroupParties {}

}
