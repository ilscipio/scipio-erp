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
public class FormsForecastForms {

    @Form(
        name = "FindSalesForecast",
        location = "component://marketing/widget/sfa/forms/ForecastForms.xml",
        target = "FindSalesForecast",
        extendsForm = "LookupSalesForecast",
        extendsResource = "component://marketing/widget/LookupForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindSalesForecast {}

    @Form(
        name = "SalesForecastSearchResults",
        location = "component://marketing/widget/sfa/forms/ForecastForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindForecasts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        viewSize = 5,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SalesForecast", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "salesForecastId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditSalesForecast", description = "${salesForecastId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "salesForecastId")})),
            @FormField(name = "percentOfQuotaForecast", hidden = @HiddenField),
            @FormField(name = "percentOfQuotaClosed", hidden = @HiddenField),
            @FormField(name = "pipelineAmount", hidden = @HiddenField),
            @FormField(name = "createdByUserLoginId", hidden = @HiddenField),
            @FormField(name = "modifiedByUserLoginId", hidden = @HiddenField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "SalesForecast"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface SalesForecastSearchResults {}

    @Form(
        name = "EditSalesForecast",
        location = "component://marketing/widget/sfa/forms/ForecastForms.xml",
        target = "updateSalesForecast",
        defaultMapName = "salesForecast",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateSalesForecast")
        },
        fields = {
            @FormField(name = "salesForecastId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "salesForecastId!=null", display = @DisplayField),
            @FormField(name = "salesForecastId", useWhen = "salesForecast==null&&salesForecastId==null", ignored = @IgnoredField),
            @FormField(name = "salesForecastId", tooltip = "${uiLabelMap.CommonCannotBeFound}: [${salesForecastId}]", useWhen = "salesForecast==null&&salesForecastId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "parentSalesForecastId", lookup = @LookupField(targetFormName = "LookupSalesForecast")),
            @FormField(name = "organizationPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "internalPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "currencyUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "createdByUserLoginId", hidden = @HiddenField),
            @FormField(name = "modifiedByUserLoginId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSave}", useWhen = "salesForecast==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "salesForecast!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "salesForecast==null", target = "createSalesForecast")
        }
    )
    public interface EditSalesForecast {}

    @Form(
        name = "ListSalesForecastDetails",
        location = "component://marketing/widget/sfa/forms/ForecastForms.xml",
        type = FormType.LIST,
        target = "updateSalesForecastDetail",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        viewSize = 10,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SalesForecastDetail", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "salesForecastId", hidden = @HiddenField),
            @FormField(name = "salesForecastDetailId", display = @DisplayField),
            @FormField(name = "quantityUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "[${typeDescription}] ${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "uomTypeId")}))),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductProductCategoryId}", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteSalesForecastDetail", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "salesForecastId"), @ParameterDef(paramName = "salesForecastDetailId")}))
        }
    )
    public interface ListSalesForecastDetails {}

    @Form(
        name = "AddSalesForecastDetail",
        location = "component://marketing/widget/sfa/forms/ForecastForms.xml",
        target = "createSalesForecastDetail",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SalesForecastDetail")
        },
        fields = {
            @FormField(name = "salesForecastId", hidden = @HiddenField),
            @FormField(name = "salesForecastDetailId", hidden = @HiddenField),
            @FormField(name = "quantityUomId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "[${typeDescription}] ${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "uomTypeId")}))),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productCategoryId", title = "${uiLabelMap.ProductProductCategoryId}", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddSalesForecastDetail {}

}
