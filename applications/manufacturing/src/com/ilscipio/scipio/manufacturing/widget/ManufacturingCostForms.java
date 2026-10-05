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
package com.ilscipio.scipio.manufacturing.widget;

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
public class ManufacturingCostForms {

    @Form(
        name = "ListCostComponentCalc",
        location = "component://manufacturing/widget/manufacturing/CostForms.xml",
        type = FormType.LIST,
        listName = "allCostComponentCalcs",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CostComponentCalc", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "costComponentCalcId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditCostCalcs", description = "${costComponentCalcId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "costComponentCalcId")})),
            @FormField(name = "costGlAccountTypeId", hidden = @HiddenField),
            @FormField(name = "offsettingGlAccountTypeId", hidden = @HiddenField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId")),
            @FormField(name = "costCustomMethodId", displayEntity = @DisplayEntityField(entityName = "CustomMethod", keyFieldName = "customMethodId")),
            @FormField(name = "removeCostComponentCalcAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeCostComponentCalc", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "costComponentCalcId")}))
        }
    )
    public interface ListCostComponentCalc {}

    @Form(
        name = "EditCostComponentCalc",
        location = "component://manufacturing/widget/manufacturing/CostForms.xml",
        target = "updateCostComponentCalc",
        defaultMapName = "costComponentCalc",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CostComponentCalc", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "costComponentCalcId", display = @DisplayField),
            @FormField(name = "costGlAccountTypeId", hidden = @HiddenField),
            @FormField(name = "offsettingGlAccountTypeId", hidden = @HiddenField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "costCustomMethodId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustomMethod", description = "${description}", keyFieldName = "customMethodId", constraints = {@EntityConstraint(name = "customMethodTypeId", value = "COST_FORMULA")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "costComponentCalc==null", target = "createCostComponentCalc")
        }
    )
    public interface EditCostComponentCalc {}

}
