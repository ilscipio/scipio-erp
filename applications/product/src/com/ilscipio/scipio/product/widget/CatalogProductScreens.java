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
public class CatalogProductScreens {

    @Screen(name = "FindProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindProduct")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindProduct")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindProduct")
    @Action(type = ActionType.SET, field = "labelTitlePropertyFull", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "isSpecificProduct", value = "false", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"acctgAgreementPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindProduct", location = "component://product/widget/catalog/ProductForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProducts", location = "component://product/widget/catalog/ProductForms.xml"
                    )}))})), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.AccountingViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface FindProduct {}

    @Screen(name = "EditProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProduct")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "lookupProduct")
    @Action(type = ActionType.SET, field = "product", fromField = "lookupProduct", global = true)
    @Action(type = ActionType.SET, field = "labelTitlePropertyFull", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "isCreateProduct", value = "${groovy: !(context.product || (parameters.productId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateProduct ? 'ProductNewProduct' : 'ProductProduct'}")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioEditProduct", location = "component://product/widget/catalog/ProductScreens.xml"
                    )})})})
        }
    )
    public interface EditProduct {}

    @Screen(name = "ViewProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewProduct")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "parameters.bypassIfNoProduct", value = "true")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProduct")
    @Action(type = ActionType.SET, field = "labelTitlePropertyFull", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"product"})}), widgets = @InlineWidgets(containers = {
                        @Container(style = "${styles.grid_row}", containers = {
                            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                                @IncludeScreen(name = "ScipioProductInfo", location = "component://product/widget/catalog/ProductScreens.xml"
                            ),
                            @IncludeScreen(name = "ScipioProductCategory", location = "component://product/widget/catalog/ProductScreens.xml"
                        ),
                        @IncludeScreen(name = "ScipioProductDates", location = "component://product/widget/catalog/ProductScreens.xml"
                    ),
                    @IncludeScreen(name = "ScipioProductRates", location = "component://product/widget/catalog/ProductScreens.xml"
                ),
                @IncludeScreen(name = "ScipioProductShoppingCart", location = "component://product/widget/catalog/ProductScreens.xml"
            ),
            @IncludeScreen(name = "ScipioProductMisc", location = "component://product/widget/catalog/ProductScreens.xml"
            )}),
            @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                @IncludeScreen(name = "ScipioProductInventory", location = "component://product/widget/catalog/ProductScreens.xml"
            ),
            @IncludeScreen(name = "ScipioProductAmount", location = "component://product/widget/catalog/ProductScreens.xml"
            ),
            @IncludeScreen(name = "ScipioProductMeasures", location = "component://product/widget/catalog/ProductScreens.xml"
            ),
            @IncludeScreen(name = "ScipioProductShipping", location = "component://product/widget/catalog/ProductScreens.xml"
            )})})}), failWidgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderProductNotFound} (${productId})", style = "common-msg-error"
            )}))})
        }
    )
    public interface ViewProduct {}

    @Screen(name = "EditProductPrices", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductPrices")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductPrices")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductPrices")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductPrice", list = "productPrices", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"productStoreGroupId", "productPricePurposeId", "productPriceTypeId", "currencyUomId", "fromDate"})
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductPrices}", includeForms = {
                    @IncludeForm(name = "AddProductPrice", location = "component://product/widget/catalog/ProductForms.xml", position = 1
                )}, labels = {
                    @Label(text = "${uiLabelMap.ProductPricesWarning}", position = 0
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductPrices}", includeForms = {
                    @IncludeForm(name = "UpdateProductPrice", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductPrices {}

    @Screen(name = "ViewProductAgreements", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewProductAgreements")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewProductAgreements")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAgreements")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "SupplierProduct", list = "supplierProductAgreements", conditions = {@ConditionExpr(fieldName = "productId", fromField = "productId"), @ConditionExpr(fieldName = "agreementId", operator = "not-equals", fromField = "nullField")}, orderBy = {"availableFromDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementItemAndProductAppl", list = "salesAgreements", fieldMaps = {@FieldMap(fieldName = "productId"), @FieldMap(fieldName = "agreementTypeId", value = "SALES_AGREEMENT")}, orderBy = {"fromDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementItemAndProductAppl", list = "productAgreements", fieldMaps = {@FieldMap(fieldName = "productId"), @FieldMap(fieldName = "agreementTypeId", value = "PRODUCT_AGREEMENT")}, orderBy = {"fromDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "AgreementItemAndProductAppl", list = "commissionAgreements", fieldMaps = {@FieldMap(fieldName = "productId"), @FieldMap(fieldName = "agreementTypeId", value = "COMMISSION_AGREEMENT")}, orderBy = {"fromDate"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleEditAgreement}", style = "${styles.link_nav} ${styles.action_update}", target = "/accounting/control/EditAgreement"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.ProductPurchases}", includeForms = {
                        @IncludeForm(name = "ListSupplierProductAgreements", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 1),
                    @Screenlet(title = "${uiLabelMap.ProductSales}", includeForms = {
                        @IncludeForm(name = "ListSalesAgreements", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 2),
                    @Screenlet(title = "${uiLabelMap.ProductCommissions}", includeForms = {
                        @IncludeForm(name = "ListCommissionAgreements", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 3),
                    @Screenlet(title = "${uiLabelMap.ProductProducts}", includeForms = {
                        @IncludeForm(name = "ListProductAgreements", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 4)})
        }
    )
    public interface ViewProductAgreements {}

    @Screen(name = "EditProductCategories", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductCategories")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCategories")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "ScipioAddProductCategoryMember", location = "component://product/widget/catalog/ProductScreens.xml"
                )}),
                @Screenlet(includeScreens = {
                    @IncludeScreen(name = "ScipioProductCategoryMemberList", location = "component://product/widget/catalog/ProductScreens.xml"
                )})})
        }
    )
    public interface EditProductCategories {}

    @Screen(name = "ScipioAddProductCategoryMember", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioAddProductCategoryMember.ftl")})}))
    public interface ScipioAddProductCategoryMember {}

    @Screen(name = "ScipioProductCategoryMemberList", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategoryMember", list = "productCategoryMemberList", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"sequenceNum", "productCategoryId"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"productCategoryMemberList"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductCategories}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductCategoryMemberList.ftl")})}))
    public interface ScipioProductCategoryMemberList {}

    @Screen(name = "EditProductConfigs", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductConfigs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductConfigs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductConfigs")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductConfig", list = "productConfigs", fieldMaps = {@FieldMap(fieldName = "productId")}, orderBy = {"sequenceNum", "fromDate"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductConfigs}", includeForms = {
                    @IncludeForm(name = "AddProductConfig", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductConfigs}", includeForms = {
                    @IncludeForm(name = "UpdateProductConfig", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductConfigs {}

    @Screen(name = "EditProductAssetUsage", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductAssetUsage")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductAssetUsage")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAssetUsage")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "FixedAssetProduct", list = "fixedAssetProducts", conditions = {@ConditionExpr(fieldName = "productId", fromField = "productId")})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductAssetUsage}", includeForms = {
                    @IncludeForm(name = "EditProductAssetUsage", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductAssetUsage}", includeForms = {
                    @IncludeForm(name = "ListProductFixedAssets", location = "component://product/widget/catalog/ProductForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleCreateProductAssetUsage}", style = "${styles.link_nav} ${styles.action_add}", target = "newFixedAssetProduct"
                    )}, position = 0)})})
        }
    )
    public interface EditProductAssetUsage {}

    @Screen(name = "showFixedAssetProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductAssetUsage")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductAssetUsage")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAssetUsage")
    @Action(type = ActionType.SET, field = "extraFunctionName", value = "uiLabelMap.AccountingFixedAssetProductUpd")
    @Action(type = ActionType.ENTITY_ONE, entityName = "FixedAssetProduct", valueField = "fixedAssetProduct")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "showFixedAssetProduct", location = "component://product/widget/catalog/ProductForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.AccountingFixedAssetNew}", style = "${styles.link_nav} ${styles.action_add}", target = "newFixedAssetProduct"
                    )}, position = 0)})})
        }
    )
    public interface showFixedAssetProduct {}

    @Screen(name = "newFixedAssetProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddProductAssetUsage")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductAssetUsage")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAssetUsage")
    @Action(type = ActionType.SET, field = "extraFunctionName", value = "uiLabelMap.AccountingFixedAssetProductAdd")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "addFixedAssetProduct", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface newFixedAssetProduct {}

    @Screen(name = "ViewProductManufacturing", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewProductManufacturing")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewProductManufacturing")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductManufacturing")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SET, field = "productAssocTypeId", value = "MANUF_COMPONENT")
    @Action(type = ActionType.SET, field = "workEffortGoodStdTypeId", value = "ROU_PROD_TEMPLATE")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductAssoc", list = "components", fieldMaps = {@FieldMap(fieldName = "productId"), @FieldMap(fieldName = "productAssocTypeId")}, orderBy = {"sequenceNum"})
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductAssoc", list = "parents", fieldMaps = {@FieldMap(fieldName = "productIdTo", fromField = "productId"), @FieldMap(fieldName = "productAssocTypeId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "WorkEffortGoodStandard", list = "routings", fieldMaps = {@FieldMap(fieldName = "productId"), @FieldMap(fieldName = "workEffortGoodStdTypeId")})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductLowLevelCode}: ${product.billOfMaterialLevel}", style = "heading"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductRoutings}", includeForms = {
                    @IncludeForm(name = "ListProductRoutings", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.ProductComponents}", includeForms = {
                    @IncludeForm(name = "ListProductComponents", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 2),
                @Screenlet(title = "${uiLabelMap.ProductParent}", includeForms = {
                    @IncludeForm(name = "ListProductParents", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 3)})
        }
    )
    public interface ViewProductManufacturing {}

    @Screen(name = "EditProductCosts", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductCosts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductCosts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductCosts")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SET, field = "productCostComponentId", fromField = "parameters.productCostComponentId")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CostComponent", valueField = "costComponent", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "costComponentId", fromField = "productCostComponentId")})
    @Action(type = ActionType.ENTITY_AND, entityName = "CostComponent", list = "costs", fieldMaps = {@FieldMap(fieldName = "productId")}, orderBy = {"-fromDate"})
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductCostComponentCalc", list = "productCostComponentCalcs", fieldMaps = {@FieldMap(fieldName = "productId")}, orderBy = {"sequenceNum"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.FilterProductCosts}", includeForms = {
                    @IncludeForm(name = "FilterCostComponents", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.ProductCosts}", includeForms = {
                    @IncludeForm(name = "ListCostComponents", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 2),
                @Screenlet(title = "${uiLabelMap.ProductAddCostComponent}", includeForms = {
                    @IncludeForm(name = "EditCostComponent", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 3),
                @Screenlet(title = "${uiLabelMap.ProductCostComponentCalcs}", includeForms = {
                    @IncludeForm(name = "ListProductCostComponentCalcs", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 4),
                @Screenlet(title = "${uiLabelMap.AddProductCostComponentCalc}", includeForms = {
                    @IncludeForm(name = "AddProductCostComponentCalc", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 5),
                @Screenlet(title = "${uiLabelMap.ProductAutoCreateCosts}", includeForms = {
                    @IncludeForm(name = "CalculateProductCosts", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 6)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"parameters.costUomId"
                    }),
                    @Condition(type = NotEmpty.class, params = {"parameters.costComponentTypePrefix"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.SERVICE, serviceName = "getProductCost", resultMapName = "totalCostMap", fieldMaps = {
                        @FieldMap(fieldName = "productId"),
                        @FieldMap(fieldName = "currencyUomId", fromField = "parameters.costUomId"
                    ),
                    @FieldMap(fieldName = "costComponentTypePrefix", fromField = "parameters.costComponentTypePrefix"
                )})}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonTotal}: ${totalCostMap.productCost}", style = "heading"
                )}), position = 1)})
        }
    )
    public interface EditProductCosts {}

    @Screen(name = "QuickAddVariants", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleQuickAddProductVariants")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "QuickAddVariants")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductQuickAddVariants")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/QuickAddVariants.groovy")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductQuickAdmin}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/product/QuickAddVariants.ftl"
                )})})
        }
    )
    public interface QuickAddVariants {}

    @Screen(name = "EditProductQuickAdmin", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductQuickAdmin")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProduct")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductQuickAdmin")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/EditProductQuickAdmin.groovy")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/EditProductQuickAdmin.ftl"
            )})
        }
    )
    public interface EditProductQuickAdmin {}

    @Screen(name = "EditProductFacilities", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductFacilities")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductFacilities")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductFacilities")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductFacility", list = "productFacilities", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"facilityId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Facility", list = "facilities", orderBy = {"facilityName"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddFacility}", includeForms = {
                    @IncludeForm(name = "AddProductFacility", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductFacilities}", includeForms = {
                    @IncludeForm(name = "UpdateProductFacilities", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductFacilities {}

    @Screen(name = "EditProductFacilityLocations", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductFacilityLocations")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductFacilityLocations")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductFacilityLocations")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductFacilityLocation", list = "productFacilityLocations", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"facilityId", "locationSeqId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Facility", list = "facilities", orderBy = {"facilityName"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.ProductFacilityLocation}", includeForms = {
                    @IncludeForm(name = "AddProductFacilityLocation", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductFacilityLocations}", includeForms = {
                    @IncludeForm(name = "UpdateProductFacilityLocations", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductFacilityLocations {}

    @Screen(name = "EditProductKeyword", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductKeywords")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductKeyword")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductKeywords")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.ENTITY_AND, entityName = "Enumeration", list = "keywordTypeList", fieldMaps = {@FieldMap(fieldName = "enumTypeId", value = "KEYWORD_TYPE")}, orderBy = {"sequenceId"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.ITERATE_SECTION, list = "keywordTypeList", entry = "keywordType", name = "EditProductKeyword-iterate1", location = "component://product/widget/catalog/ProductScreens.xml"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductReInduceKeywords}", style = "${styles.link_run_sys} ${styles.action_update}", target = "forceIndexProductKeywords"
                ),
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductDeleteAllKeywords}", style = "${styles.link_run_sys} ${styles.action_remove}", target = "deleteProductKeywords"
            )}, position = 0)}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddProductKeyword}", includeForms = {
                    @IncludeForm(name = "AddProductKeyword", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditProductKeyword {}

    @Screen(name = "EditProductKeyword-iterate1", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "keywordTypeId", fromField = "keywordType.enumId")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleEditProductKeywords} : ${keywordType.description}", includeForms = {@IncludeForm(name = "UpdateProductKeyword", location = "component://product/widget/catalog/ProductForms.xml")})}))
    public interface EditProductKeyword_iterate1 {}

    @Screen(name = "EditProductInventoryItems", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductInventoryItems")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductInventoryItems")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductInventorySummary")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/EditProductInventoryItems.groovy")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"product.isVirtual", "equals", "Y", "String"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/EditVirtualProductInventory.ftl"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/ProductInventorySummary.ftl", position = 0
                ),
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/EditProductInventoryItems.ftl", position = 2
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductOutstandingPurchaseOrders}", includeForms = {
                    @IncludeForm(name = "OutstandingPurchaseOrders", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 1)}))})
        }
    )
    public interface EditProductInventoryItems {}

    @Screen(name = "EditProductGoodIdentifications", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductGoodIdentifications")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductGoodIdentifications")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductGoodIdentification")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GoodIdentification", list = "goodIdentifications", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"goodIdentificationTypeId", "idValue"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GoodIdentificationType", list = "goodIdentificationTypes", useCache = true, orderBy = {"description"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleCreateProductGoodIdentifications}", includeForms = {
                    @IncludeForm(name = "AddProductGoodIdentification", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductGoodIdentifications}", includeForms = {
                    @IncludeForm(name = "UpdateProductGoodIdentifications", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductGoodIdentifications {}

    @Screen(name = "EditProductGlAccounts", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductGlAccounts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductGlAccounts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductGlAccounts")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductGlAccount", list = "productGlAccounts", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"glAccountTypeId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GlAccountType", list = "productGlAccountTypes", useCache = true, orderBy = {"description"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "GlAccount", list = "glAccounts", useCache = true, orderBy = {"accountCode"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductGlAccounts}", sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"product"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductGlAccounts", location = "component://product/widget/catalog/ProductForms.xml"
                        )}))}),
                        @Screenlet(title = "${uiLabelMap.ProductAddGlAccount}", sections = {
                            @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                @Condition(type = Empty.class, params = {"product"})}), widgets = @WidgetsForContainer(value = {
                                    @Widget(type = WidgetType.INCLUDE_FORM, name = "AddProductGlAccount", location = "component://product/widget/catalog/ProductForms.xml"
                                )}))})})
        }
    )
    public interface EditProductGlAccounts {}

    @Screen(name = "EditProductPaymentMethodTypes", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductPaymentMethodType")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductPaymentMethodTypes")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductPaymentTypes")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPaymentMethodType", list = "productPaymentMethodTypes", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"productPricePurposeId", "paymentMethodTypeId", "fromDate"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductPaymentMethodType}", includeForms = {
                    @IncludeForm(name = "AddProductPaymentMethodType", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductPaymentMethodType}", includeForms = {
                    @IncludeForm(name = "UpdateProductPaymentMethodType", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductPaymentMethodTypes {}

    @Screen(name = "EditProductFeatures", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductFeatures")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductFeatures")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductFeatures")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/EditProductFeatures.groovy")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductFeatureIactn", list = "featureInteractions", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "parameters.productId")})
    @DecoratorScreen(
        name = "CommonProductFeaturesDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/product/EditProductFeatures.ftl"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.ProductListFeatureInteractions}", includeForms = {
                        @IncludeForm(name = "ListFeatureInteractions", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 1),
                    @Screenlet(title = "${uiLabelMap.ProductAddFeatureInteraction}", includeForms = {
                        @IncludeForm(name = "AddFeatureInteraction", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 2),
                    @Screenlet(title = "${uiLabelMap.ProductFeatureAttributes}", includeForms = {
                        @IncludeForm(name = "AddProductFeatureApplAttr", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 3),
                    @Screenlet(title = "${uiLabelMap.PageTitleListProductFeatureApplAttrs}", includeForms = {
                        @IncludeForm(name = "ListProductFeatureApplAttrs", location = "component://product/widget/catalog/ProductForms.xml"
                    )}, position = 4)})
        }
    )
    public interface EditProductFeatures {}

    @Screen(name = "EditSupplierProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSupplierProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSupplierProduct")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductSuppliers")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SET, field = "orderBy", fromField = "parameters.sortField", defaultValue = "partyId")
    @Action(type = ActionType.ENTITY_AND, entityName = "SupplierProduct", list = "productSuppliers", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"${orderBy}"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "SupplierProduct", valueField = "supplierProduct")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditSupplierProduct}", includeForms = {
                    @IncludeForm(name = "ListSupplierProducts", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductSupplier}", includeForms = {
                    @IncludeForm(name = "AddSupplierProduct", location = "component://product/widget/catalog/ProductForms.xml"
                )}, position = 2)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"supplierProduct"})}
                    ), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewProductSupplier}", style = "${styles.link_nav} ${styles.action_add}", target = "EditProductSuppliers"
                    )}), position = 1)})
        }
    )
    public interface EditSupplierProduct {}

    @Screen(name = "EditProductContent", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductContent")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/shop/images/productAdditionalView.js", global = true)
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/EditProductContent.groovy")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/content/images/ScpContentCommon.js", global = true)
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductContent}", includeForms = {
                    @IncludeForm(name = "ListProductContentInfos", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductCreateNewProductContent}", includeForms = {
                    @IncludeForm(name = "PrepareAddProductContentAssoc", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductAddContentProduct}", includeForms = {
                    @IncludeForm(name = "AddProductContentAssoc", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductOverrideSimpleFields}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/product/EditProductContent.ftl"
                )}),
                @Screenlet(title = "${uiLabelMap.CommonUpdateLocalizedFields}", includeScreens = {
                    @IncludeScreen(name = "EditProductStcLocFields", location = "component://product/widget/catalog/ProductScreens.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductAddAdditionalImages}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/product/AddAdditionalImages.ftl"
                )})})
        }
    )
    public interface EditProductContent {}

    @Screen(name = "EditProductContentContent", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/product/WEB-INF/actions/generated/EditProductContentContent_script1.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductContent")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductContent", valueField = "productContent")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/EditProductContentContent.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/EditProductSEO.groovy")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductContent}", includeForms = {
                    @IncludeForm(name = "${contentFormName}", location = "component://product/widget/catalog/ProductForms.xml", position = 1
                )}, includeMenus = {
                    @IncludeMenu(name = "ProductContentSectionSubTabBar", location = "component://product/widget/catalog/CatalogMenus.xml", position = 0
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"content.contentId"}
                    )}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditAltLocaleSimpleTextContent"
                    )}, screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleListAssociatedContentInfos} (${uiLabelMap.CommonAll})", includeForms = {
                            @IncludeForm(name = "ListAssociatedContentInfos", location = "component://product/widget/catalog/ProductForms.xml"
                        )})}))})
        }
    )
    public interface EditProductContentContent {}

    @Screen(name = "EditAltLocaleSimpleTextContent", location = "component://product/widget/catalog/ProductScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"productContent"}), @Condition(type = NotEmpty.class, params = {"productId"}), @Condition(type = NotEmpty.class, params = {"contentId"}), @Condition(type = NotEmpty.class, params = {"content.contentId"})}))
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleCreateSimpleTextContentForAlternateLocale}", includeForms = {@IncludeForm(name = "ListSimpleTextContentForAlternateLocale", location = "component://product/widget/catalog/ProductForms.xml"), @IncludeForm(name = "CreateSimpleTextContentForAlternateLocale", location = "component://product/widget/catalog/ProductForms.xml")})}))
    public interface EditAltLocaleSimpleTextContent {}

    @Screen(name = "EditProductAttributes", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductAttributes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductAttributes")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAttributes")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductAttribute", list = "productAttributes", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"attrType", "attrName"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductAttributeShortInstructions}", style = "common-msg-info-important"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddProductAttributeNameValueType}", includeForms = {
                    @IncludeForm(name = "AddProductAttribute", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductAttributes}", includeForms = {
                    @IncludeForm(name = "UpdateProductAttribute", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductAttributes {}

    @Screen(name = "EditProductAssoc", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductAssociations")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductAssoc")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAssociations")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/EditProductAssoc.groovy")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Empty.class, params = {"productAssoc"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "AddProductAssoc", location = "component://product/widget/catalog/ProductForms.xml"
                        )}), failWidgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "EditProductAssoc", location = "component://product/widget/catalog/ProductForms.xml"
                        )}), position = 0)}, screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.ProductAssociationsFromProduct}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/product/ProductFromAssocs.ftl"
                
                        )}, position = 1),
                        @ScreenletNested(title = "${uiLabelMap.ProductAssociationsToProduct}", htmlTemplates = {
                    @HtmlTemplate(location = "component://product/webapp/catalog/product/ProductToAssocs.ftl"
                
                    )}, position = 2)})})
        }
    )
    public interface EditProductAssoc {}

    @Screen(name = "ApplyFeaturesFromCategory", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleApplyFeaturesFromCategory")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAddProductFeatureFromCategory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductFeatures")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/ApplyFeaturesFromCategory.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/ApplyFeaturesFromGroup.groovy")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/ApplyFeaturesFromCategory.ftl"
            )})
        }
    )
    public interface ApplyFeaturesFromCategory {}

    @Screen(name = "CreateVirtualWithVariantsForm", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProduct")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCreateVirtualWithVariants")
    @Action(type = ActionType.SET, field = "labelTitlePropertyFull", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleCreateVirtualWithVariants")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CreateVirtualWithVariantsFormInclude", location = "component://product/widget/catalog/ProductScreens.xml"
            )})
        }
    )
    public interface CreateVirtualWithVariantsForm {}

    @Screen(name = "CreateVirtualWithVariantsFormInclude", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductFeature", list = "hazmatFeatures", conditions = {@ConditionExpr(fieldName = "productFeatureTypeId", value = "HAZMAT")})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/CreateVirtualWithVariantsForm.ftl")}))
    public interface CreateVirtualWithVariantsFormInclude {}

    @Screen(name = "EditProductMaints", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductMaintenance")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductMaintenance")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductMaints")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductMaintenance}", includeForms = {
                    @IncludeForm(name = "AddProductMaint", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductMaintenance}", includeForms = {
                    @IncludeForm(name = "ListProductMaints", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductMaints {}

    @Screen(name = "EditProductMeters", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductMeters")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductMeters")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductMeters")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductMeters}", includeForms = {
                    @IncludeForm(name = "AddProductMeter", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductMeters}", includeForms = {
                    @IncludeForm(name = "ListProductMeters", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductMeters {}

    @Screen(name = "EditProductGeos", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductGeos")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductGeos")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductGeos")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductGeos}", includeForms = {
                    @IncludeForm(name = "AddProductGeo", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductGeos}", includeForms = {
                    @IncludeForm(name = "ListProductGeos", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductGeos {}

    @Screen(name = "EditProductSubscriptionResources", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductSubscriptionResources")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductSubscriptionResources")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductSubscriptionResources")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductSubscriptionResources}", includeForms = {
                    @IncludeForm(name = "ListProductSubscriptionResources", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductSubscriptionResources}", includeForms = {
                    @IncludeForm(name = "AddProductSubscriptionResource", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductSubscriptionResources {}

    @Screen(name = "EditProductWorkEfforts", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductWorkEffort")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductWorkEfforts")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductWorkEffort")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductWorkEffort}", includeForms = {
                    @IncludeForm(name = "AddProductWorkEffort", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductWorkEffort}", includeForms = {
                    @IncludeForm(name = "ListProductWorkEfforts", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductWorkEfforts {}

    @Screen(name = "ProductBarCode.fo", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.SET, field = "productName", fromField = "product.productName", defaultValue = "${product.internalName}")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ProductBarCode.fo.ftl", platform = "xsl-fo")}))
    public interface ProductBarCode_fo {}

    @Screen(name = "EditProductParties", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductParties")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductParties")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyParties")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductRole", list = "productRoles", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")}, orderBy = {"roleTypeId", "partyId"})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAssociatePartyToProduct}", includeForms = {
                    @IncludeForm(name = "AddProductRole", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductParties}", includeForms = {
                    @IncludeForm(name = "UpdateProductRole", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductParties {}

    @Screen(name = "EditVendorProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditVendorProduct")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditVendorProduct")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyVendor")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "VendorProduct", list = "vendorProductList", conditions = {@ConditionExpr(fieldName = "productId", fromField = "parameters.productId")})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddVendorProduct}", includeForms = {
                    @IncludeForm(name = "EditVendorProduct", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditVendorProduct}", includeForms = {
                    @IncludeForm(name = "ListVendorProducts", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditVendorProduct {}

    @Screen(name = "BestSellingProducts", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "1", valueType = "Integer")
    @Action(type = ActionType.SET, field = "chartDatasets", value = "1")
    @Action(type = ActionType.SET, field = "xlabel")
    @Action(type = ActionType.SET, field = "ylabel")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.ProductOne}")
    @Action(type = ActionType.SET, field = "label2", value = "${uiLabelMap.ProductTwo}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/BestProducts.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"bestSellingProducts"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/ScipioBestProductChart.ftl")}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductNoProductFound}.", style = "common-msg-result-norecord")}))})}))
    public interface BestSellingProducts {}

    @Screen(name = "ViewProductOrder", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewProductOrders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewProductOrder")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "OrderOrders")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/order/FindOrders.groovy")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/ViewProductOrders.ftl"
            )})
        }
    )
    public interface ViewProductOrder {}

    @Screen(name = "EditProductCommunicationEvents", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductCommunicationEvents")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleCommEvents")
    @Action(type = ActionType.ENTITY_AND, entityName = "CommunicationEventAndProduct", list = "communicationEvents", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "parameters.productId")})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew}", style = "${styles.link_nav} ${styles.action_add}", target = "AddCommEventForProduct"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleCommEvents}", includeForms = {
                    @IncludeForm(name = "ListCommEvents", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductCommunicationEvents {}

    @Screen(name = "EditCommunicationEvent", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCommEvents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductCommunicationEvents")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductAddCommunicationEvent")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.SET, field = "communicationEventId", fromField = "parameters.communicationEventId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CommunicationEvent", valueField = "communicationEvent")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddCommunicationEvent}", includeForms = {
                    @IncludeForm(name = "EditCommEvent", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditCommunicationEvent {}

    @Screen(name = "ProductPriceHistory", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleProductPricesHistory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductPrices")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductPricesHistory")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductPriceChange", list = "productPricesChanges", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "productId"), @FieldMap(fieldName = "productPriceTypeId", fromField = "parameters.productPriceTypeId"), @FieldMap(fieldName = "fromDate", fromField = "parameters.fromDate")}, orderBy = {"fromDate", "changedDate DESC"})
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListProductPriceHistory", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface ProductPriceHistory {}

    @Screen(name = "ViewProductGroupOrder", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductGroupOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewProductGroupOrder")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductGroupOrder")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductGroupOrder", list = "productGroupOrders", fieldMaps = {@FieldMap(fieldName = "productId", fromField = "productId")})
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddGroupOrder}", includeForms = {
                    @IncludeForm(name = "CreateProductGroupOrder", location = "component://product/widget/catalog/ProductForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductGroupOrder}", includeForms = {
                    @IncludeForm(name = "ListProductGroupOrder", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface ViewProductGroupOrder {}

    @Screen(name = "EditProductGroupOrder", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductGroupOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductGroupOrder")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductGroupOrder")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductGroupOrder", valueField = "productGroupOrder")
    @DecoratorScreen(
        name = "CommonProductDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductEditGroupOrder}", includeForms = {
                    @IncludeForm(name = "EditProductGroupOrder", location = "component://product/widget/catalog/ProductForms.xml"
                )})})
        }
    )
    public interface EditProductGroupOrder {}

    @Screen(name = "ScipioEditProduct", location = "component://product/widget/catalog/ProductScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/ScipioEditProduct.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioEditProduct.ftl")})}))
    public interface ScipioEditProduct {}

    @Screen(name = "ScipioProductInfo", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.productName", "product.internalName", "product.brandName", "product.productTypeId", "product.manufacturerPartyId", "product.comments"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonOverview}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductInfo.ftl")})}))
    public interface ScipioProductInfo {}

    @Screen(name = "ScipioProductCategory", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.primaryProductCategoryId"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductPrimaryCategory}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductCategory.ftl")})}))
    public interface ScipioProductCategory {}

    @Screen(name = "ScipioProductDates", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.introductionDate", "product.releaseDate", "product.salesDiscontinuationDate", "product.supportDiscontinuationDate"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonDates}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductDates.ftl")})}))
    public interface ScipioProductDates {}

    @Screen(name = "ScipioProductInventory", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.salesDiscWhenNotAvail", "product.requirementMethodEnumId", "product.lotIdFilledIn", "product.inventoryMessage"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonInventory}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductInventory.ftl")})}))
    public interface ScipioProductInventory {}

    @Screen(name = "ScipioProductRates", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.productRating"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonRate}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductRates.ftl")})}))
    public interface ScipioProductRates {}

    @Screen(name = "ScipioProductAmount", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.requireAmount", "product.amountUomTypeId"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonAmount}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductAmount.ftl")})}))
    public interface ScipioProductAmount {}

    @Screen(name = "ScipioProductMeasures", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.productHeight", "product.productWidth", "product.productDepth", "product.productDiameter", "product.productWeight"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonMeasures}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductMeasures.ftl")})}))
    public interface ScipioProductMeasures {}

    @Screen(name = "ScipioProductShipping", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.quantityIncluded", "product.piecesIncluded", "product.inShippingBox", "product.defaultShipmentBoxTypeId", "product.chargeShipping"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonShipping}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductShipping.ftl")})}))
    public interface ScipioProductShipping {}

    @Screen(name = "ScipioProductShoppingCart", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.orderDecimalQuantity"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonShoppingCart}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductShoppingCart.ftl")})}))
    public interface ScipioProductShoppingCart {}

    @Screen(name = "ScipioProductMisc", location = "component://product/widget/catalog/ProductScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"product.returnable", "product.includeInPromotions", "product.taxable"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.CommonMiscellaneous}", htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/catalog/product/ScipioProductMisc.ftl")})}))
    public interface ScipioProductMisc {}

    @Screen(name = "EditProductStcLocFields", location = "component://product/widget/catalog/ProductScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"product"})}))
    @Action(type = ActionType.SET, field = "productId", fromField = "product.productId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/product/GetProductStcLocFields.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/product/EditProductStcLocFields.ftl")}))
    public interface EditProductStcLocFields {}

}
