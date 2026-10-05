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
public class FormsPartySkillForms {

    @Form(
        name = "FindPartySkills",
        location = "component://humanres/widget/forms/PartySkillForms.xml",
        target = "FindPartySkills",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PartySkill", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "skillTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "yearsExperience", textFind = @TextFindField),
            @FormField(name = "rating", textFind = @TextFindField),
            @FormField(name = "skillLevel", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPartySkills {}

    @Form(
        name = "ListPartySkills",
        location = "component://humanres/widget/forms/PartySkillForms.xml",
        type = FormType.LIST,
        target = "updatePartySkillExt",
        listName = "listIt",
        paginateTarget = "FindPartySkills",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartySkill", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "skillTypeId", displayEntity = @DisplayEntityField(entityName = "SkillType", description = "${description}")),
            @FormField(name = "yearsExperience", text = @TextField),
            @FormField(name = "rating", text = @TextField),
            @FormField(name = "skillLevel", text = @TextField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartySkill", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "skillTypeId"), @ParameterDef(paramName = "partyId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListPartySkills {}

    @Form(
        name = "AddPartySkills",
        location = "component://humanres/widget/forms/PartySkillForms.xml",
        target = "createPartySkill",
        defaultMapName = "partySkill",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "partyId", useWhen = "partySkill != null", hidden = @HiddenField),
            @FormField(name = "partyId", useWhen = "partySkill == null", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "skillTypeId", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SkillType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "yearsExperience", text = @TextField),
            @FormField(name = "rating", text = @TextField),
            @FormField(name = "skillLevel", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "insideEmployee != null", target = "createPartySkillExt")
        },
        actions = @FormActions(set = {@SetAction(field = "insideEmployee", fromField = "parameters.insideEmployee")})
    )
    public interface AddPartySkills {}

}
