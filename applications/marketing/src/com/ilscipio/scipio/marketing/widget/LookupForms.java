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
public class LookupForms {

    @Form(
        name = "LookupSegmentGroup",
        location = "component://marketing/widget/LookupForms.xml",
        target = "LookupSegmentGroup",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupId}", textFind = @TextFindField),
            @FormField(name = "segmentGroupTypeId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupTypeId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.MarketingSegmentGroupProductStoreId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupSegmentGroup {}

    @Form(
        name = "listLookupSegmentGroup",
        location = "component://marketing/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupSegmentGroup",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${segmentGroupId}')", urlMode = UrlMode.PLAIN, description = "${segmentGroupId}", alsoHidden = false)),
            @FormField(name = "segmentGroupTypeId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupTypeId}", display = @DisplayField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "productStoreId", title = "${uiLabelMap.MarketingSegmentGroupProductStoreId}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SegmentGroup"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupSegmentGroup {}

    @Form(
        name = "listSegmentGroupClass",
        location = "component://marketing/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "listSegmentGroupClass",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.MarketingSegmentGroupSegmentGroupId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "viewSegmentGroup", description = "${segmentGroupId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId")})),
            @FormField(name = "partyClassificationGroupId", title = "${uiLabelMap.MarketingSegmentGroupPartyClassificationGroupId}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSegmentGroupClass", description = "[${uiLabelMap.CommonDelete}]", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId"), @ParameterDef(paramName = "partyClassificationGroupId")}))
        }
    )
    public interface listSegmentGroupClass {}

    @Form(
        name = "LookupSalesForecast",
        location = "component://marketing/widget/LookupForms.xml",
        target = "LookupSalesForecast",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SalesForecast", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "currencyUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "createdByUserLoginId", hidden = @HiddenField),
            @FormField(name = "modifiedByUserLoginId", hidden = @HiddenField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupSalesForecast {}

    @Form(
        name = "ListLookupSalesForecast",
        location = "component://marketing/widget/LookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupSalesForecast",
        oddRowStyle = "alternate-row",
        viewSize = 10,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SalesForecast", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "salesForecastId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${salesForecastId}')", urlMode = UrlMode.PLAIN, description = "${salesForecastId}", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SalesForecast"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupSalesForecast {}

}
