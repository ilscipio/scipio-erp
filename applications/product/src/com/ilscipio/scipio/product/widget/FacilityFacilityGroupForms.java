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
package com.ilscipio.scipio.product.widget;

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
public class FacilityFacilityGroupForms {

    @Form(
        name = "FindFacilityGroup",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        type = FormType.LIST,
        listName = "facilityGroups",
        paginateTarget = "FindFacilityGroup",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FacilityGroup", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "facilityGroupId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFacilityGroup", description = "${facilityGroupId}", parameters = {@ParameterDef(paramName = "facilityGroupId")})),
            @FormField(name = "facilityGroupTypeId", displayEntity = @DisplayEntityField(entityName = "FacilityGroupType")),
            @FormField(name = "description", display = @DisplayField)
        }
    )
    public interface FindFacilityGroup {}

    @Form(
        name = "EditFacilityGroup",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        target = "updateFacilityGroup",
        defaultMapName = "facilityGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateFacilityGroup")
        },
        fields = {
            @FormField(name = "facilityGroupId", tooltip = "${uiLabelMap.ProductNotModificationRecrationFacilityGroup}", useWhen = "facilityGroup!=null", display = @DisplayField),
            @FormField(name = "facilityGroupId", useWhen = "facilityGroup==null", hidden = @HiddenField),
            @FormField(name = "facilityGroupTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FacilityGroupType", description = "${description}", keyFieldName = "facilityGroupTypeId"))),
            @FormField(name = "primaryParentGroupId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FacilityGroup", description = "${facilityGroupName}", keyFieldName = "facilityGroupId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "facilityGroup==null", target = "createFacilityGroup")
        }
    )
    public interface EditFacilityGroup {}

    @Form(
        name = "UpdateFacilityGroupRollupTo",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        type = FormType.LIST,
        target = "updateFacilityGroupToGroup",
        listName = "currentGroupRollups",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateFacilityGroupToGroup")
        },
        fields = {
            @FormField(name = "showFacilityGroupId", hidden = @HiddenField(value = "${facilityGroupId}")),
            @FormField(name = "facilityGroupId", hidden = @HiddenField(value = "${facilityGroupId}")),
            @FormField(name = "parentFacilityGroupId", displayEntity = @DisplayEntityField(entityName = "FacilityGroup", keyFieldName = "facilityGroupId", description = "${facilityGroupName}", subHyperlink = @SubHyperlink(target = "EditFacilityGroup", description = "[${parentFacilityGroupId}]", parameters = {@ParameterDef(paramName = "facilityGroupId", fromField = "parentFacilityGroupId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFacilityGroupFromGroup", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "showFacilityGroupId", fromField = "facilityGroupId"), @ParameterDef(paramName = "facilityGroupId"), @ParameterDef(paramName = "parentFacilityGroupId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateFacilityGroupRollupTo {}

    @Form(
        name = "AddFacilityGroupRollupFrom",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        target = "addFacilityGroupToGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addFacilityGroupToGroup")
        },
        fields = {
            @FormField(name = "facilityGroupId", hidden = @HiddenField),
            @FormField(name = "parentFacilityGroupId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FacilityGroup", description = "${facilityGroupName} [${facilityGroupId}]", keyFieldName = "facilityGroupId", orderBy = {@EntityOrderBy(fieldName = "facilityGroupName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFacilityGroupRollupFrom {}

    @Form(
        name = "UpdateFacilityGroupRollupFrom",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        type = FormType.LIST,
        target = "updateFacilityGroupToGroup",
        listName = "parentGroupRollups",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateFacilityGroupToGroup")
        },
        fields = {
            @FormField(name = "showFacilityGroupId", hidden = @HiddenField(value = "${parentFacilityGroupId}")),
            @FormField(name = "parentFacilityGroupId", hidden = @HiddenField),
            @FormField(name = "facilityGroupId", displayEntity = @DisplayEntityField(entityName = "FacilityGroup", description = "${facilityGroupName}", subHyperlink = @SubHyperlink(target = "EditFacilityGroup", description = "[${facilityGroupId}]", parameters = {@ParameterDef(paramName = "facilityGroupId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFacilityGroupFromGroup", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "showFacilityGroupId", fromField = "parentFacilityGroupId"), @ParameterDef(paramName = "facilityGroupId"), @ParameterDef(paramName = "parentFacilityGroupId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateFacilityGroupRollupFrom {}

    @Form(
        name = "AddFacilityGroupRollupTo",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        target = "addFacilityGroupToGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addFacilityGroupToGroup")
        },
        fields = {
            @FormField(name = "showFacilityGroupId", hidden = @HiddenField(value = "${facilityGroupId}")),
            @FormField(name = "parentFacilityGroupId", hidden = @HiddenField(value = "${facilityGroupId}")),
            @FormField(name = "facilityGroupId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FacilityGroup", description = "${facilityGroupName} [${facilityGroupId}]", orderBy = {@EntityOrderBy(fieldName = "facilityGroupName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFacilityGroupRollupTo {}

    @Form(
        name = "UpdateFacilityGroupMembers",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        type = FormType.LIST,
        target = "updateFacilityToGroup",
        listName = "facilityGroupMembers",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateFacilityToGroup")
        },
        fields = {
            @FormField(name = "facilityGroupId", hidden = @HiddenField(value = "${facilityGroupId}")),
            @FormField(name = "facilityId", displayEntity = @DisplayEntityField(entityName = "Facility", keyFieldName = "facilityId", description = "${facilityName}", subHyperlink = @SubHyperlink(target = "EditFacility", description = "[${facilityId}]", parameters = {@ParameterDef(paramName = "facilityId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFacilityFromGroup", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "facilityGroupId"), @ParameterDef(paramName = "facilityId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateFacilityGroupMembers {}

    @Form(
        name = "AddFacilityGroupMember",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        target = "addFacilityToGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addFacilityToGroup")
        },
        fields = {
            @FormField(name = "facilityGroupId", hidden = @HiddenField),
            @FormField(name = "facilityId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFacilityGroupMember {}

    @Form(
        name = "UpdateFacilityGroupRoles",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        type = FormType.LIST,
        target = "removePartyFromFacilityGroup",
        listName = "facilityRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "removePartyFromFacilityGroup")
        },
        fields = {
            @FormField(name = "facilityGroupId", hidden = @HiddenField(value = "${facilityGroupId}")),
            @FormField(name = "partyId", displayEntity = @DisplayEntityField(entityName = "Party", keyFieldName = "partyId", description = "${partyId}", subHyperlink = @SubHyperlink(target = "viewProfile", description = "[${partyId}]", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "roleTypeId", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId", description = "${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removePartyFromFacilityGroup", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "facilityGroupId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface UpdateFacilityGroupRoles {}

    @Form(
        name = "AddFacilityGroupRole",
        location = "component://product/widget/facility/FacilityGroupForms.xml",
        target = "addPartyToFacilityGroup",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "addPartyToFacilityGroup")
        },
        fields = {
            @FormField(name = "facilityGroupId", hidden = @HiddenField),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description} [${roleTypeId}]", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFacilityGroupRole {}

}
