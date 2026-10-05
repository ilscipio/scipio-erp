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
package com.ilscipio.scipio.accounting.widget;

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
public class ControllingCostForms {

    @Form(
        name = "ListCostComponentCalc",
        location = "component://accounting/widget/controlling/CostForms.xml",
        type = FormType.LIST,
        target = "updateCostComponentCalc",
        listName = "allCostComponentCalcs",
        paginateTarget = "EditCostCalcs",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CostComponentCalc", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "costComponentCalcId", widgetStyle = "${styles.link_nav_info_id}"),
            @FormField(name = "costGlAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId"))),
            @FormField(name = "offsettingGlAccountTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId"))),
            @FormField(name = "updateCostComponentCalc", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "costCustomMethodId", title = "${uiLabelMap.CommonMethod}", displayEntity = @DisplayEntityField(entityName = "CustomMethod", keyFieldName = "customMethodId", description = "${description}")),
            @FormField(name = "deleteCostComponentCalc", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteCostComponentCalc", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "costComponentCalcId")}))
        }
    )
    public interface ListCostComponentCalc {}

    @Form(
        name = "AddCostComponentCalc",
        location = "component://accounting/widget/controlling/CostForms.xml",
        target = "createCostComponentCalc",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createCostComponentCalc")
        },
        fields = {
            @FormField(name = "costGlAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId"))),
            @FormField(name = "offsettingGlAccountTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", keyFieldName = "glAccountTypeId"))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "costCustomMethodId", title = "${uiLabelMap.CommonMethod}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustomMethod", description = "${description}", keyFieldName = "customMethodId", constraints = {@EntityConstraint(name = "customMethodTypeId", value = "COST_FORMULA")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddCostComponentCalc {}

}
