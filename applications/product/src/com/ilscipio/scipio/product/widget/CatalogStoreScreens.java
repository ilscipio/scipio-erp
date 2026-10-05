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
public class CatalogStoreScreens {

    @Screen(name = "FindProductStore", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreList")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreList")
    @Action(type = ActionType.SET, field = "isSpecificProductStore", value = "false", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListProductStore", location = "component://product/widget/catalog/StoreForms.xml"
                )})})
        }
    )
    public interface FindProductStore {}

    @Screen(name = "EditProductStore", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: parameters.productStoreId ? 'PageTitleEditProductStore' : 'ProductCreateNewProductStore'}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStore")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.SET, field = "isCreateProductStore", value = "${groovy: !(context.productStore || (parameters.productStoreId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "${groovy: isCreateProductStore ? 'ProductNewProductStore' : 'ProductStore'}")
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProductStore", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})})
        }
    )
    public interface EditProductStore {}

    @Screen(name = "FindProductStoreRoles", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindProductStoreRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindProductStoreRoles")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreRoles")
    @Action(type = ActionType.SET, field = "parameters.fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreRole", valueField = "productStoreRole")
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreRoles}", includeForms = {
                    @IncludeForm(name = "EditProductStoreRole", location = "component://product/widget/catalog/StoreForms.xml"
                )})}, decorators = {
                    @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindProductStoreRole", location = "component://product/widget/catalog/StoreForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListProductStoreRole", location = "component://product/widget/catalog/StoreForms.xml"
                        )}))}, position = 0)})
        }
    )
    public interface FindProductStoreRoles {}

    @Screen(name = "EditProductStorePromos", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStorePromos")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStorePromos")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStorePromotions")
    @Action(type = ActionType.SET, field = "userEntered", fromField = "parameters.userEntered")
    @Action(type = ActionType.SET, field = "activeOnly", fromField = "parameters.activeOnly", defaultValue = "true")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStorePromoAndAppl", list = "productStorePromoAndAppls", conditions = {@ConditionExpr(fieldName = "userEntered", fromField = "userEntered", ignoreIfEmpty = true), @ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")}, orderBy = {"sequenceNum", "productPromoId"})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductAddStorePromo}", includeForms = {
                    @IncludeForm(name = "CreateProductStorePromo", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"productStorePromoAndAppls"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(includeForms = {
                            @IncludeForm(name = "ListProductStorePromos", location = "component://product/widget/catalog/ProductStoreForms.xml"
                        )})}), position = 0)})
        }
    )
    public interface EditProductStorePromos {}

    @Screen(name = "EditProductStoreCatalogs", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreCatalogs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreCatalogs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreCatalogs")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreCatalog", list = "productStoreCatalogs", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")}, orderBy = {"sequenceNum", "productStoreId"})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreCatalogs}", includeForms = {
                    @IncludeForm(name = "UpdateProductStoreCatalog", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductStoreCatalogs}", includeForms = {
                    @IncludeForm(name = "CreateProductStoreCatalog", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})})
        }
    )
    public interface EditProductStoreCatalogs {}

    @Screen(name = "EditProductStoreWebSites", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreWebSites")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreWebSites")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreWebSites")
    @Action(type = ActionType.SET, field = "labelTitlePropertyFull", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.viewProductStoreId")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId", defaultValue = "${productStoreId}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WebSite", list = "storeWebSites", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")}, orderBy = {"siteName"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WebSite", list = "webSites", orderBy = {"siteName"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/store/EditProductStoreWebSites.ftl"
            )})
        }
    )
    public interface EditProductStoreWebSites {}

    @Screen(name = "EditProductStoreShipSetup", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreShipSetup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreShipSetup")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreShipmentSettings")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreShipmentMethView", list = "storeShipMethods", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")}, orderBy = {"sequenceNumber"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreShipmentMeth", valueField = "productStoreShipmentMeth")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CarrierShipmentMethod", list = "carrierShipmentMethods", orderBy = {"sequenceNumber"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustomMethod", list = "shipmentCustomMethods", conditions = {@ConditionExpr(fieldName = "customMethodTypeId", value = "SHIP_EST")}, orderBy = {"description"})
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/SetStoreLastProductStore.groovy")
    @DecoratorScreen(
        name = "CommonShippingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreShipSetup}", includeForms = {
                    @IncludeForm(name = "ListProductStoreShipmentMeths", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Empty.class, params = {"productStoreShipmentMeth"
                    }),
                    @Condition(type = Empty.class, params = {"parameters.addCarrierShipMeth"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleAddProductStoreShipSetup}", htmlTemplates = {
                        @HtmlTemplate(location = "component://product/webapp/catalog/store/EditProductStoreShipSetup.ftl"
                    )})}), failWidgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleAddProductStoreShipSetup}", includeForms = {
                            @IncludeForm(name = "EditProductStoreShipmentMeth", location = "component://product/widget/catalog/ProductStoreForms.xml"
                        )})}))})
        }
    )
    public interface EditProductStoreShipSetup {}

    @Screen(name = "EditProductStoreShipmentCostEstimates", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreShipmentCostEstimates")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreShipmentCostEstimates")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreShipmentSettings")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ShipmentCostEstimate", list = "estimates", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")}, orderBy = {"geoIdFrom", "shipmentMethodTypeId", "geoIdTo"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ShipmentCostEstimate", valueField = "estimate")
    @DecoratorScreen(
        name = "CommonShippingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListShipmentCostEstimates", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Empty.class, params = {"estimate"})}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.ProductNewShipmentEstimate}", includeForms = {
                                @IncludeForm(name = "AddShipmentCostEstimate", location = "component://product/widget/catalog/ProductStoreForms.xml"
                            )})}), failWidgets = @InlineWidgets(screenlets = {
                                @Screenlet(title = "${uiLabelMap.ProductViewEstimates}", includeForms = {
                                    @IncludeForm(name = "ViewShipmentCostEstimate", location = "component://product/widget/catalog/ProductStoreForms.xml"
                                )})}))})
        }
    )
    public interface EditProductStoreShipmentCostEstimates {}

    @Screen(name = "EditProductStorePaySetup", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStorePaySetup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStorePaySetup")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStorePaymentSettings")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.SET, field = "paymentMethodTypeId", fromField = "parameters.paymentMethodTypeId")
    @Action(type = ActionType.SET, field = "paymentServiceTypeEnumId", fromField = "parameters.paymentServiceTypeEnumId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStorePaymentSetting", valueField = "productStorePaymentSetting")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/EditProductStorePaySetup.groovy")
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListProductStorePaySetup}", actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "productStorePaymentSettings", valueType = "NewList"
                )}), includeForms = {
                    @IncludeForm(name = "ListProductStorePaymentSettings", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStorePaySetup}", sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = HasPermission.class, params = {"CATALOG", "_CREATE"
                    })}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "EditProductStorePaymentSetting", location = "component://product/widget/catalog/ProductStoreForms.xml"
                    )}))})})
        }
    )
    public interface EditProductStorePaySetup {}

    @Screen(name = "EditProductStoreEmails", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreEmailSetup")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreEmails")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreEmailSettings")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreEmailSetting", list = "productStoreEmailSettings", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")}, orderBy = {"emailType"})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreEmailSetup}", includeForms = {
                    @IncludeForm(name = "updateProductStoreEmail", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductStoreEmailSetup}", includeForms = {
                    @IncludeForm(name = "createProductStoreEmail", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})})
        }
    )
    public interface EditProductStoreEmails {}

    @Screen(name = "EditProductStoreSurveys", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreSurveys")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreSurveys")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductStoreSurveySettings")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/store/EditProductStoreSurveys.groovy")
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/store/EditProductStoreSurveys.ftl"
            )})
        }
    )
    public interface EditProductStoreSurveys {}

    @Screen(name = "EditProductStoreKeywordOvrd", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreKeywordOvrd")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreKeywordOvrd")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductStoreKeywordOverrides")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreKeywordOvrd", list = "productStorekeywordOvrdList", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreKeywordOvrd}", includeForms = {
                    @IncludeForm(name = "UpdateproductStorekeywordOvrdForm", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddProductStoreKeywordOvrd}", includeForms = {
                    @IncludeForm(name = "CreateproductStorekeywordOvrdForm", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})})
        }
    )
    public interface EditProductStoreKeywordOvrd {}

    @Screen(name = "ViewProductStoreSegments", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewProductStoreSegments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewProductStoreSegments")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreSegments")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore", fieldMaps = {@FieldMap(fieldName = "productStoreId")})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewSegment}", style = "${styles.link_nav} ${styles.action_add}", target = "/marketing/control/viewSegmentGroup"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "ViewProductStoreSegments", location = "component://product/widget/catalog/ProductStoreForms.xml"
                    )}, position = 1)})
        }
    )
    public interface ViewProductStoreSegments {}

    @Screen(name = "EditProductStoreFinAccountSettings", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreFinAccountSettings")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreFinAccountSettings")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductStoreFinAccountSettings")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreFinActSetting", valueField = "finAccountSetting")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreFinActSetting", list = "productStoreFinActSettings", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListProductStoreFinAccountSettings}", includeForms = {
                    @IncludeForm(name = "ListProductStoreFinAccountSettings", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreFinAccountSettings}", includeForms = {
                    @IncludeForm(name = "EditProductStoreFinAccountSettings", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})})
        }
    )
    public interface EditProductStoreFinAccountSettings {}

    @Screen(name = "EditProductStoreVendorPayments", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreVendorPayments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreVendorPayments")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductStoreVendorPayments")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreVendorPayment", list = "productStoreVendorPaymentList", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListProductStoreVendorPayments}", includeForms = {
                    @IncludeForm(name = "ListProductStoreVendorPayments", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreVendorPayments}", includeForms = {
                    @IncludeForm(name = "EditProductStoreVendorPayment", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})})
        }
    )
    public interface EditProductStoreVendorPayments {}

    @Screen(name = "EditProductStoreVendorShipments", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreVendorShipments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreVendorShipments")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductStoreVendorShipments")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStoreVendorShipment", list = "productStoreVendorShipmentList", conditions = {@ConditionExpr(fieldName = "productStoreId", fromField = "productStoreId")})
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListProductStoreVendorShipments}", includeForms = {
                    @IncludeForm(name = "ListProductStoreVendorShipments", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductStoreVendorShipments}", includeForms = {
                    @IncludeForm(name = "EditProductStoreVendorShipment", location = "component://product/widget/catalog/ProductStoreForms.xml"
                )})})
        }
    )
    public interface EditProductStoreVendorShipments {}

    @Screen(name = "ProductStoreFacilities", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductStoreFacilities")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/catalog/CatalogMenus.xml#ProductStore")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStore", valueField = "productStore")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreFacilities")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreFacilities")
    @DecoratorScreen(
        name = "CommonProductStoreDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"productStore"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.ProductStoreFacilityAssocList}", includeForms = {
                            @IncludeForm(name = "ListProductStoreFacility", location = "component://product/widget/catalog/StoreForms.xml", position = 1
                        )}, includeMenus = {
                            @IncludeMenu(name = "ProductStoreFacility", location = "component://product/widget/catalog/CatalogMenus.xml", position = 0
                        )}, position = 0)}, sections = {
                            @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                @Condition(type = HasPermission.class, params = {"CATALOG", "_UPDATE"
                            })}), actions = @Actions(value = {
                                @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId"
                            ),
                            @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate"
                        ),
                        @Action(type = ActionType.SET, field = "isSuccessfulUpdate", value = "${groovy: (context.requestMethod=='POST' && !context.isError)}", valueType = "Boolean"
                    ),
                    @Action(type = ActionType.SET, field = "facilityId", value = "${groovy: (isSuccessfulUpdate ? '' : context.facilityId)}"
                ),
                @Action(type = ActionType.SET, field = "fromDate", value = "${groovy: (isSuccessfulUpdate ? '' : context.fromDate)}"
            ),
            @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreFacility", valueField = "productStoreFacility"
            ),
            @Action(type = ActionType.SET, field = "sectionTitleProp", value = "${groovy: context.productStoreFacility ? 'ProductEditFacility' : 'ProductAddFacility'}"
            )}), widgets = @WidgetsForContainer(screenlets = {
                @ScreenletNested(title = "${uiLabelMap[sectionTitleProp]}", includeForms = {
                    @IncludeForm(name = "EditProductStoreFacility", location = "component://product/widget/catalog/StoreForms.xml"
                
            )})}), position = 1)}))})
        }
    )
    public interface ProductStoreFacilities {}

    @Screen(name = "ListProductStoreFacility", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductStoreFacilityAssocList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductStoreFacilities")
    @Action(type = ActionType.SET, field = "productStoreId", fromField = "parameters.productStoreId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "portalPageId", value = "ProductStoreFacility")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.CONTAINER, id = "ProductStoreFacilityEditArea")}, screenlets = {@Screenlet(title = "${uiLabelMap.ProductStoreFacilityAssocList}", includeForms = {@IncludeForm(name = "ListProductStoreFacility", location = "component://product/widget/catalog/StoreForms.xml", position = 1)}, includeMenus = {@IncludeMenu(name = "ProductStoreFacility", location = "component://product/widget/catalog/CatalogMenus.xml", position = 0)})}))
    public interface ListProductStoreFacility {}

    @Screen(name = "EditProductStoreFacility", location = "component://product/widget/catalog/StoreScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CATALOG", "_UPDATE"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductCatalogUpdatePermissionError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreFacility", valueField = "productStoreFacility")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "EditProductStoreFacility", location = "component://product/widget/catalog/StoreForms.xml")}))
    public interface EditProductStoreFacility {}

    @Screen(name = "ListParentProductStoreGroup", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductProductStoreGroup")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreGroup")
    @DecoratorScreen(
        name = "CommonProductStoreGroupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ProductStoreGroupButtonBar", location = "component://product/widget/catalog/CatalogMenus.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductParentProductStoreGroupList}", includeForms = {
                    @IncludeForm(name = "ListParentProductStoreGroup", location = "component://product/widget/catalog/StoreForms.xml"
                )})})
        }
    )
    public interface ListParentProductStoreGroup {}

    @Screen(name = "EditProductStoreGroup", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductProductStoreGroup")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductProductStoreGroup")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreGroup", valueField = "productStoreGroup")
    @DecoratorScreen(
        name = "CommonProductStoreGroupDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProductStoreGroup", location = "component://product/widget/catalog/StoreForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"parameters.productStoreGroupId"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "ProductStoreGroupButtonBar", location = "component://product/widget/catalog/CatalogMenus.xml"
                    )}), position = 0)})
        }
    )
    public interface EditProductStoreGroup {}

    @Screen(name = "EditProductStoreGroupAndAssoc", location = "component://product/widget/catalog/StoreScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductStoreGroup", valueField = "productStoreGroup")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.ProductProductStoreGroup} ${productStoreGroup.productStoreGroupName} [${productStoreGroup.productStoreGroupId}]", name = "editProductStoreGroup", collapsible = true, initiallyCollapsed = true, includeForms = {@IncludeForm(name = "EditProductStoreGroup", location = "component://product/widget/catalog/StoreForms.xml")}), @Screenlet(title = "${uiLabelMap.ProductProductStoreGroupRollup}", name = "editProductStoreGroupRollup", collapsible = true, initiallyCollapsed = true, includeForms = {@IncludeForm(name = "ListProductStoreGroupAssoc", location = "component://product/widget/catalog/StoreForms.xml")}), @Screenlet(title = "${uiLabelMap.ProductProductStoreMember}", includeForms = {@IncludeForm(name = "ListProductStoreAssoc", location = "component://product/widget/catalog/StoreForms.xml")}), @Screenlet(title = "${uiLabelMap.ProductAddToProductStoreGroup}", includeForms = {@IncludeForm(name = "AddProductStoreAssoc", location = "component://product/widget/catalog/StoreForms.xml")})}))
    public interface EditProductStoreGroupAndAssoc {}

}
