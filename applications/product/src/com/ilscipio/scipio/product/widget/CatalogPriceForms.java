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
public class CatalogPriceForms {

    @Form(
        name = "FindProductPriceRules",
        location = "component://product/widget/catalog/PriceForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindProductPriceRules",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productPriceRuleId", title = "${uiLabelMap.ProductPriceRule}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditProductPriceRules", description = "${ruleName} [${productPriceRuleId}]", parameters = {@ParameterDef(paramName = "productPriceRuleId")})),
            @FormField(name = "isSale", title = "${uiLabelMap.ProductSaleRule}?", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditProductPriceRules", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "productPriceRuleId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "noConditionFind", value = "Y"), @SetAction(field = "parameters.productPriceRuleId"), @SetAction(field = "parameters.ruleName")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "ProductPriceRule"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface FindProductPriceRules {}

    @Form(
        name = "AddPriceRules",
        location = "component://product/widget/catalog/PriceForms.xml",
        target = "createProductPriceRule",
        fields = {
            @FormField(name = "ruleName", title = "${uiLabelMap.ProductName}", requiredField = true, text = @TextField(size = 30)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPriceRules {}

    @Form(
        name = "EditProductPriceRule",
        location = "component://product/widget/catalog/PriceForms.xml",
        type = FormType.LIST,
        target = "updateProductPriceRule",
        listName = "productPriceRules",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productPriceRuleId", hidden = @HiddenField),
            @FormField(name = "ruleName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", text = @TextField(size = 60)),
            @FormField(name = "isSale", title = "${uiLabelMap.ProductSaleRule}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", useWhen = "org.ofbiz.base.util.UtilValidate.isEmpty(productPriceConds) && org.ofbiz.base.util.UtilValidate.isEmpty(productPriceActions)", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductPriceRule", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "productPriceRuleId")}))
        }
    )
    public interface EditProductPriceRule {}

    @Form(
        name = "EditProductPriceRulesCond",
        location = "component://product/widget/catalog/PriceForms.xml",
        type = FormType.LIST,
        target = "updateProductPriceCond",
        listName = "productPriceConds",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productPriceRuleId", hidden = @HiddenField),
            @FormField(name = "productPriceCondSeqId", hidden = @HiddenField),
            @FormField(name = "inputParamEnumId", title = "${uiLabelMap.ProductInput}", dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PROD_PRICE_IN_PARAM")}))),
            @FormField(name = "operatorEnumId", title = "${uiLabelMap.ProductOperator}", dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PROD_PRICE_COND")}))),
            @FormField(name = "condValueInput", entryName = "condValue", title = "${uiLabelMap.ProductValue}", text = @TextField(size = 10)),
            @FormField(name = "condValue", title = " ", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductPriceCond", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "productPriceRuleId", fromField = "productPriceCond.productPriceRuleId"), @ParameterDef(paramName = "productPriceCondSeqId", fromField = "productPriceCond.productPriceCondSeqId")}))
        }
    )
    public interface EditProductPriceRulesCond {}

    @Form(
        name = "AddProductPriceRulesCond",
        location = "component://product/widget/catalog/PriceForms.xml",
        type = FormType.LIST,
        target = "createProductPriceCond",
        listName = "productPriceCondAdd",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productPriceRuleId", hidden = @HiddenField),
            @FormField(name = "new", title = "${uiLabelMap.CommonNew}", display = @DisplayField(defaultValue = "${uiLabelMap.ProductPriceRulesNewCond}")),
            @FormField(name = "inputParamEnumId", title = "${uiLabelMap.ProductInput}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PROD_PRICE_IN_PARAM")}))),
            @FormField(name = "operatorEnumId", title = "${uiLabelMap.ProductOperator}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PROD_PRICE_COND")}))),
            @FormField(name = "condValueInput", title = "${uiLabelMap.ProductValue}", text = @TextField(size = 10)),
            @FormField(name = "condValue", title = " ", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductPriceRulesCond {}

    @Form(
        name = "EditProductPriceRulesAction",
        location = "component://product/widget/catalog/PriceForms.xml",
        type = FormType.LIST,
        target = "updateProductPriceAction",
        listName = "productPriceActions",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productPriceRuleId", hidden = @HiddenField),
            @FormField(name = "productPriceActionSeqId", hidden = @HiddenField),
            @FormField(name = "productPriceActionTypeId", title = "${uiLabelMap.ProductActionType}", dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "ProductPriceActionType", description = "${description}", keyFieldName = "productPriceActionTypeId"))),
            @FormField(name = "amount", title = "${uiLabelMap.ProductValue}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductPriceAction", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "productPriceRuleId", fromField = "productPriceAction.productPriceRuleId"), @ParameterDef(paramName = "productPriceActionSeqId", fromField = "productPriceAction.productPriceActionSeqId")}))
        }
    )
    public interface EditProductPriceRulesAction {}

    @Form(
        name = "AddProductPriceRulesAction",
        location = "component://product/widget/catalog/PriceForms.xml",
        type = FormType.LIST,
        target = "createProductPriceAction",
        listName = "productPriceActionAdd",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "productPriceRuleId", hidden = @HiddenField),
            @FormField(name = "new", title = "${uiLabelMap.CommonNew}", display = @DisplayField(defaultValue = "${uiLabelMap.ProductPriceRulesNewAction}")),
            @FormField(name = "productPriceActionTypeId", title = "${uiLabelMap.ProductActionType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPriceActionType", description = "${description}", keyFieldName = "productPriceActionTypeId"))),
            @FormField(name = "amount", title = "${uiLabelMap.ProductValue}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddProductPriceRulesAction {}

}
