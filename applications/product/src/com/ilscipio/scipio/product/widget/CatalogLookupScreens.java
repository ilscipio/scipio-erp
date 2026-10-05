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

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CatalogLookupScreens {

    @Screen(name = "LookupProduct", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupProduct}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "entityName", value = "Product")
    @Action(type = ActionType.SET, field = "searchFields", value = "[productId, internalName, brandName]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupProduct", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupProduct", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupProduct {}

    @Screen(name = "LookupVirtualProduct", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupProductVirtual}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "entityName", value = "Product")
    @Action(type = ActionType.SET, field = "searchFields", value = "[productId, brandName, internalName]")
    @Action(type = ActionType.SET, field = "andCondition", value = "${groovy: return org.ofbiz.entity.condition.EntityCondition.makeCondition(\"isVirtual\", \"Y\")}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupVirtualProduct", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupVirtualProduct", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupVirtualProduct {}

    @Screen(name = "LookupVariantProduct", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupProductVariant}")
    @Action(type = ActionType.SET, field = "entityName", value = "Product")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/lookup/LookupVariantProduct.groovy")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/lookup/LookupVariantProduct.ftl"
            )})
        }
    )
    public interface LookupVariantProduct {}

    @Screen(name = "LookupProductAndPrice", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupProductPrice}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SET, field = "entityName", value = "ProductAndPriceView")
    @Action(type = ActionType.SET, field = "searchFields", value = "[productId, internalName]")
    @Action(type = ActionType.SET, field = "displayFields", value = "[productId, internalName, price, currencyUomId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupProductAndPrice", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupProductAndPrice", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupProductAndPrice {}

    @Screen(name = "LookupProductCategory", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupProductCategory}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "entityName", value = "ProductCategory")
    @Action(type = ActionType.SET, field = "searchFields", value = "[productCategoryId, categoryName, description]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupProductCategory", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupProductCategory", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupProductCategory {}

    @Screen(name = "LookupProductFeature", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupProductFeature}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "entityName", value = "ProductFeature")
    @Action(type = ActionType.SET, field = "searchFields", value = "[productFeatureId, description, productFeatureCategoryId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupProductFeature", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupProductFeature", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupProductFeature {}

    @Screen(name = "LookupProductStore", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupProductStore}")
    @Action(type = ActionType.SET, field = "entityName", value = "ProductStore")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "entityName", value = "ProductStore")
    @Action(type = ActionType.SET, field = "searchFields", value = "[productStoreId, companyName, storeName]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupProductStore", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupProductStore", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupProductStore {}

    @Screen(name = "LookupSupplierProduct", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "pnv")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupSupplierProduct} ${pnv.firstName} ${pnv.middleName} ${pnv.lastName} ${pnv.groupName} [${parameters.partyId}]")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "entityName", value = "SupplierProductAndProduct")
    @Action(type = ActionType.SET, field = "searchFields", value = "[productId, partyId, brandName, internalName]")
    @Action(type = ActionType.SET, field = "andCondition", value = "${groovy: return (parameters.partyId ? org.ofbiz.entity.condition.EntityCondition.makeCondition(org.ofbiz.base.util.UtilMisc.toMap('partyId', parameters.partyId)) : null)}")
    @Action(type = ActionType.SET, field = "searchDistinct", value = "true")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupSupplierProduct", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupSupplierProduct", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupSupplierProduct {}

    @Screen(name = "LookupCostComponentCalc", location = "component://product/widget/catalog/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"catalogPermissionCheck", "VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupCostComponentCalc}")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SET, field = "entityName", value = "CostComponentCalc")
    @Action(type = ActionType.SET, field = "searchFields", value = "[costComponentCalcId, description, costGlAccountTypeId, offsettingGlAccountTypeId]")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupCostComponentCalc", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupCostComponentCalc", location = "component://product/widget/catalog/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupCostComponentCalc {}

}
