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
public class ManufacturingBomForms {

    @Form(
        name = "RunBomSimulation",
        location = "component://manufacturing/widget/manufacturing/BomForms.xml",
        target = "runBomSimulation",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "lookupFlag", hidden = @HiddenField(value = "Y")),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "bomType", title = "${uiLabelMap.ManufacturingBomType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductAssocType", description = "${description}", keyFieldName = "productAssocTypeId", constraints = {@EntityConstraint(name = "parentTypeId", value = "PRODUCT_COMPONENT")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField(defaultValue = "1")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", text = @TextField(defaultValue = "0")),
            @FormField(name = "type", dropDown = @DropDownField(options = {@Option(key = "0", description = "${uiLabelMap.ManufacturingExplosion}"), @Option(key = "1", description = "${uiLabelMap.ManufacturingExplosionSingleLevel}"), @Option(key = "2", description = "${uiLabelMap.ManufacturingExplosionManufacturing}"), @Option(key = "3", description = "${uiLabelMap.ManufacturingImplosion}")})),
            @FormField(name = "facilityId", title = "${uiLabelMap.ProductFacilityId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", constraints = {@EntityConstraint(name = "facilityTypeId", value = "WAREHOUSE")}))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.ProductCurrencyUomId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_begin}", submit = @SubmitField)
        }
    )
    public interface RunBomSimulation {}

    @Form(
        name = "ListProductManufacturingRules",
        location = "component://manufacturing/widget/manufacturing/BomForms.xml",
        type = FormType.LIST,
        target = "UpdateProductManufacturingRule",
        listName = "manufacturingRules",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductManufacturingRule", mapName = "manufacturingRule")
        },
        fields = {
            @FormField(name = "ruleId", display = @DisplayField),
            @FormField(name = "ruleSeqId", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "productIdFor", display = @DisplayField),
            @FormField(name = "productIdIn", display = @DisplayField),
            @FormField(name = "productIdInSubst", display = @DisplayField),
            @FormField(name = "productFeature", display = @DisplayField),
            @FormField(name = "quantity", display = @DisplayField),
            @FormField(name = "ruleOperator", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "updateAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditProductManufacturingRules", description = "${uiLabelMap.CommonSelect}", parameters = {@ParameterDef(paramName = "ruleId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "DeleteProductManufacturingRule", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "ruleId")}))
        }
    )
    public interface ListProductManufacturingRules {}

    @Form(
        name = "UpdateProductManufacturingRule",
        location = "component://manufacturing/widget/manufacturing/BomForms.xml",
        target = "UpdateProductManufacturingRule",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductManufacturingRule", mapName = "manufacturingRule")
        },
        fields = {
            @FormField(name = "ruleId", useWhen = "ruleId!=null", display = @DisplayField),
            @FormField(name = "ruleId", useWhen = "ruleId==null", ignored = @IgnoredField),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productIdFor", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productIdIn", lookup = @LookupField(targetFormName = "LookupVirtualProduct")),
            @FormField(name = "productIdInSubst", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productFeature", lookup = @LookupField(targetFormName = "LookupProductFeature")),
            @FormField(name = "ruleOperator", dropDown = @DropDownField(options = {@Option(key = "OR", description = "${uiLabelMap.CommonOr}"), @Option(key = "AND", description = "${uiLabelMap.CommonAnd}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "ruleId==null", target = "AddProductManufacturingRule")
        }
    )
    public interface UpdateProductManufacturingRule {}

    @Form(
        name = "findBom",
        location = "component://manufacturing/widget/manufacturing/BomForms.xml",
        target = "FindBom",
        fields = {
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productIdTo", title = "${uiLabelMap.ProductProductIdTo}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productAssocTypeId", title = "${uiLabelMap.ManufacturingBomType}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "MANUF_COMPONENT", description = "${uiLabelMap.ManufacturingBillOfMaterials}"), @Option(key = "ENGINEER_COMPONENT", description = "${uiLabelMap.ManufacturingEngineeringBillOfMaterials}")})),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submit", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface findBom {}

    @Form(
        name = "ListBom",
        location = "component://manufacturing/widget/manufacturing/BomForms.xml",
        type = FormType.LIST,
        listName = "ListProductBom",
        paginateTarget = "FindBom",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productIdFrom", title = "${uiLabelMap.ManufacturingProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductBom", description = "${productIdFrom}", parameters = {@ParameterDef(paramName = "productId", value = "${productIdFrom}"), @ParameterDef(paramName = "productAssocTypeId")})),
            @FormField(name = "productId", title = "${uiLabelMap.ManufacturingProductIdTo}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductBom", description = "${productId}", parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "productAssocTypeId")})),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductProductName}", display = @DisplayField),
            @FormField(name = "productAssocTypeId", title = "${uiLabelMap.ManufacturingBomType}", displayEntity = @DisplayEntityField(entityName = "ProductAssocType", keyFieldName = "productAssocTypeId", description = "${description}"))
        }
    )
    public interface ListBom {}

}
