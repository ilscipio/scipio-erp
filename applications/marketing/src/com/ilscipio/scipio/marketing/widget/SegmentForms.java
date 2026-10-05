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
package com.ilscipio.scipio.marketing.widget;

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
public class SegmentForms {

    @Form(
        name = "FindSegmentGroup",
        location = "component://marketing/widget/SegmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindSegmentGroup",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "viewSegmentGroup", description = "${segmentGroupId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId")})),
            @FormField(name = "segmentGroupTypeId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupTypeId}", displayEntity = @DisplayEntityField(entityName = "SegmentGroupType", description = "${description}")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.MarketingSegmentGroupProductStoreId}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSegmentGroup", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SegmentGroup"), @FieldMap(fieldName = "noConditionFind", value = "Y"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface FindSegmentGroup {}

    @Form(
        name = "EditSegmentGroup",
        location = "component://marketing/widget/SegmentForms.xml",
        target = "updateSegmentGroup",
        defaultMapName = "segmentGroup",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupId}", useWhen = "segmentGroup==null&&segmentGroupId==null", ignored = @IgnoredField),
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupId}", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${segmentGroupId}]", useWhen = "segmentGroup==null&&segmentGroupId!=null", display = @DisplayField),
            @FormField(name = "segmentGroupTypeId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupTypeId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "SegmentGroupType", description = "${description}", keyFieldName = "segmentGroupTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "productStoreId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField(size = 55)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        },
        altTargets = {
            @AltTarget(useWhen = "segmentGroup==null", target = "createSegmentGroup")
        }
    )
    public interface EditSegmentGroup {}

    @Form(
        name = "AddSegmentGroupClass",
        location = "component://marketing/widget/SegmentForms.xml",
        target = "createSegmentGroupClassification",
        defaultMapName = "segmentGroupClass",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSegmentGroupClassification")
        },
        fields = {
            @FormField(name = "segmentGroupId", hidden = @HiddenField),
            @FormField(name = "partyClassificationGroupId", lookup = @LookupField(targetFormName = "LookupPartyClassificationGroup")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddSegmentGroupClass {}

    @Form(
        name = "listSegmentGroupClass",
        location = "component://marketing/widget/SegmentForms.xml",
        type = FormType.LIST,
        listName = "segmentGroupClassList",
        paginateTarget = "listSegmentGroupClass",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", hidden = @HiddenField),
            @FormField(name = "partyClassificationGroupId", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSegmentGroupClassification", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId"), @ParameterDef(paramName = "partyClassificationGroupId")}))
        }
    )
    public interface listSegmentGroupClass {}

    @Form(
        name = "AddSegmentGroupGeo",
        location = "component://marketing/widget/SegmentForms.xml",
        target = "createSegmentGroupGeo",
        defaultMapName = "segmentGroupGeo",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSegmentGroupGeo")
        },
        fields = {
            @FormField(name = "segmentGroupId", hidden = @HiddenField),
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddSegmentGroupGeo {}

    @Form(
        name = "listSegmentGroupGeo",
        location = "component://marketing/widget/SegmentForms.xml",
        type = FormType.LIST,
        listName = "segmentGroupGeos",
        paginateTarget = "listSegmentGroupGeo",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", hidden = @HiddenField),
            @FormField(name = "geoId", title = "${uiLabelMap.CommonGeoId}", displayEntity = @DisplayEntityField(entityName = "Geo", description = "${geoName} [Code:${geoCode}][ID:${geoId}]")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSegmentGroupGeo", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId"), @ParameterDef(paramName = "geoId")}))
        }
    )
    public interface listSegmentGroupGeo {}

    @Form(
        name = "AddSegmentGroupRole",
        location = "component://marketing/widget/SegmentForms.xml",
        target = "createSegmentGroupRole",
        defaultMapName = "segmentGroupRole",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createSegmentGroupRole")
        },
        fields = {
            @FormField(name = "segmentGroupId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface AddSegmentGroupRole {}

    @Form(
        name = "listSegmentGroupRole",
        location = "component://marketing/widget/SegmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "listSegmentGroupRole",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${lastName} ${groupName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.PartyRoleTypeId}", displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSegmentGroupRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SegmentGroupRole"), @FieldMap(fieldName = "noConditionFind", value = "Y"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listSegmentGroupRole {}

}
