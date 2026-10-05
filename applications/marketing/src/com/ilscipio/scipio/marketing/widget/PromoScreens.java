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
public class PromoScreens {

    @Screen(name = "FindProductPromo", location = "component://marketing/widget/PromoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindProductPromos")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindProductPromo")
    @Action(type = ActionType.SET, field = "userEntered", fromField = "parameters.userEntered")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPromo", list = "productPromos", conditions = {@ConditionExpr(fieldName = "userEntered", fromField = "userEntered", ignoreIfEmpty = true)}, orderBy = {"-createdDate"})
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}8 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.ProductProductPromotionsList}", includeForms = {
                    @IncludeForm(name = "ListProductPromos", location = "component://product/widget/catalog/PromoForms.xml"
                
                    )})}),
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.PageTitleEditProductPromotionCode}", includeForms = {
                    @IncludeForm(name = "GoToProductPromoCode", location = "component://product/widget/catalog/PromoForms.xml"
                
                    )})})})})
        }
    )
    public interface FindProductPromo {}

    @Screen(name = "EditProductPromo", location = "component://marketing/widget/PromoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductPromos")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductPromotion")
    @Action(type = ActionType.SET, field = "productPromoId", fromField = "parameters.productPromoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductPromo", valueField = "productPromo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.productPromoId ? 'EditProductPromo' : 'FindProductPromo'}")
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditProductPromo", location = "component://product/widget/catalog/PromoForms.xml"
                )})})
        }
    )
    public interface EditProductPromo {}

    @Screen(name = "EditProductPromoRules", location = "component://marketing/widget/PromoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductPromoRules")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductPromoRules")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductRules")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "productPromoId", fromField = "parameters.productPromoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductPromo", valueField = "productPromo")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPromoRule", list = "productPromoRules", conditions = {@ConditionExpr(fieldName = "productPromoId", fromField = "productPromoId")}, orderBy = {"ruleName"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPromoCategory", list = "promoProductPromoCategories", conditions = {@ConditionExpr(fieldName = "productPromoId", fromField = "productPromoId"), @ConditionExpr(fieldName = "productPromoRuleId", value = "_NA_"), @ConditionExpr(fieldName = "productPromoActionSeqId", value = "_NA_"), @ConditionExpr(fieldName = "productPromoCondSeqId", value = "_NA_")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPromoProduct", list = "promoProductPromoProducts", conditions = {@ConditionExpr(fieldName = "productPromoId", fromField = "productPromoId"), @ConditionExpr(fieldName = "productPromoRuleId", value = "_NA_"), @ConditionExpr(fieldName = "productPromoActionSeqId", value = "_NA_"), @ConditionExpr(fieldName = "productPromoCondSeqId", value = "_NA_")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Enumeration", list = "inputParamEnums", useCache = true, conditions = {@ConditionExpr(fieldName = "enumTypeId", value = "PROD_PROMO_IN_PARAM")}, orderBy = {"sequenceId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CarrierShipmentMethod", list = "carrierShipmentMethods", useCache = true, orderBy = {"shipmentMethodTypeId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Enumeration", list = "condOperEnums", useCache = true, conditions = {@ConditionExpr(fieldName = "enumTypeId", value = "PROD_PROMO_COND")}, orderBy = {"sequenceId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Enumeration", list = "productPromoActionEnums", useCache = true, conditions = {@ConditionExpr(fieldName = "enumTypeId", value = "PROD_PROMO_ACTION")}, orderBy = {"sequenceId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Enumeration", list = "productPromoApplEnums", useCache = true, conditions = {@ConditionExpr(fieldName = "enumTypeId", value = "PROD_PROMO_PCAPPL")}, orderBy = {"sequenceId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "OrderAdjustmentType", list = "orderAdjustmentTypes", useCache = true, orderBy = {"description"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductCategory", list = "productCategories", conditions = {@ConditionExpr(fieldName = "showInSelect", operator = "not-equals", value = "N")}, orderBy = {"description"})
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/promo/EditProductPromoRules.ftl"
            )})
        }
    )
    public interface EditProductPromoRules {}

    @Screen(name = "EditProductPromoStores", location = "component://marketing/widget/PromoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductPromoStores")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductPromoStores")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductStores")
    @Action(type = ActionType.SET, field = "productPromoId", fromField = "parameters.productPromoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductPromo", valueField = "productPromo")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStorePromoAppl", list = "productStorePromoAppls", conditions = {@ConditionExpr(fieldName = "productPromoId", fromField = "productPromoId")}, orderBy = {"sequenceNum", "productPromoId"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductStore", list = "productStores", orderBy = {"storeName"})
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/promo/EditProductPromoStores.ftl"
            )})
        }
    )
    public interface EditProductPromoStores {}

    @Screen(name = "FindProductPromoCode", location = "component://marketing/widget/PromoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductPromotionCode")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindProductPromoCode")
    @Action(type = ActionType.SET, field = "productPromoId", fromField = "parameters.productPromoId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductPromo", valueField = "productPromo")
    @Action(type = ActionType.SET, field = "manualOnly", fromField = "parameters.manualOnly", defaultValue = "Y")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPromoCode", list = "productPromoCodes", conditions = {@ConditionExpr(fieldName = "productPromoId", fromField = "productPromoId")}, orderBy = {"-createdDate"})
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/promo/FindProductPromoCode.ftl"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"productPromoCodes"}
                )}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.ProductPromotionCodes}", includeForms = {
                        @IncludeForm(name = "ListProductPromoCodes", location = "component://product/widget/catalog/PromoForms.xml"
                    )})}), failWidgets = @InlineWidgets(sections = {
                        @SectionNested(actions = @Actions(value = {
                            @Action(type = ActionType.ENTITY_CONDITION, entityName = "ProductPromoCode", list = "productPromoCodes", orderBy = {"-createdDate"
                        })}), widgets = @WidgetsForContainer(screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.ProductPromotionCodes}", includeForms = {
                    @IncludeForm(name = "ListProductPromoCodes", location = "component://product/widget/catalog/PromoForms.xml"
                
                        )})}))}))})
        }
    )
    public interface FindProductPromoCode {}

    @Screen(name = "EditProductPromoCode", location = "component://marketing/widget/PromoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductPromotionCode")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindProductPromoCode")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductPromotionCode")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/catalog/WEB-INF/actions/promo/EditProductPromoCode.groovy")
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/catalog/promo/EditProductPromoCode.ftl"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewPromotionCode}", style = "${styles.link_nav} ${styles.action_add}", target = "EditProductPromoCode"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleEditProductPromotionCode}", includeForms = {
                        @IncludeForm(name = "EditProductPromoCode", location = "component://product/widget/catalog/PromoForms.xml"
                    )}, position = 1)})
        }
    )
    public interface EditProductPromoCode {}

    @Screen(name = "EditProductPromoContent", location = "component://marketing/widget/PromoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditProductPromoContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditProductPromoContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "ProductPromoContents")
    @Action(type = ActionType.SET, field = "productPromoId", fromField = "parameters.productPromoId")
    @Action(type = ActionType.SET, field = "parameters.fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "parameters.thruDate", fromField = "parameters.thruDate", valueType = "Timestamp")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductPromo", valueField = "productPromo")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ProductPromoContent", valueField = "productPromoContent")
    @Action(type = ActionType.ENTITY_AND, entityName = "ProductPromoContent", list = "productPromoContents", fieldMaps = {@FieldMap(fieldName = "productPromoId", fromField = "productPromoId")})
    @DecoratorScreen(
        name = "CommonPromoDecorator",
        location = "component://marketing/widget/sfa/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditProductPromoContent}", includeForms = {
                    @IncludeForm(name = "EditProductPromoContentImage", location = "component://product/widget/catalog/PromoForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ProductProductPromoContentList}", includeForms = {
                    @IncludeForm(name = "ListProductPromoContent", location = "component://product/widget/catalog/PromoForms.xml"
                )})})
        }
    )
    public interface EditProductPromoContent {}

}
