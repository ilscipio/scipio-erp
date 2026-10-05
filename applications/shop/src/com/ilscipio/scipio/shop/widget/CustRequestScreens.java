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
package com.ilscipio.scipio.shop.widget;

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
public class CustRequestScreens {

    @Screen(name = "CommonRequestDecorator", location = "component://shop/widget/CustRequestScreens.xml")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonRequestDecorator {}

    @Screen(name = "ListRequests", location = "component://shop/widget/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListRequests")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CustRequest", list = "requestList", conditions = {@ConditionExpr(fieldName = "fromPartyId", fromField = "userLogin.partyId")}, orderBy = {"-custRequestDate"})
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/request/RequestList.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ListRequests {}

    @Screen(name = "NewRequest", location = "component://shop/widget/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNewRequest")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/request/CustRequest.groovy")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "component://shop/widget/CustRequestScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/request/requestInfo.ftl"
                )}))})
        }
    )
    public interface NewRequest {}

    @Screen(name = "ViewRequest", location = "component://shop/widget/CustRequestScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewRequest")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "CustRequestType", toValueField = "custRequestType")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "StatusItem", toValueField = "statusItem")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "CurrencyUom", toValueField = "currency")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "custRequest", relationName = "ProductStore", toValueField = "store")
    @Action(type = ActionType.GET_RELATED, valueField = "custRequest", relationName = "CustRequestItem", list = "requestItems")
    @Action(type = ActionType.GET_RELATED, valueField = "custRequest", relationName = "CustRequestParty", list = "requestParties")
    @DecoratorScreen(
        name = "CommonRequestDecorator",
        location = "component://shop/widget/CustRequestScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = CompareField.class, params = {"custRequest.fromPartyId", "equals", "userLogin.partyId"
                        })}), widgets = @WidgetsForContainer(containers = {
                            @Container2(style = "${styles.grid_row}", containers = {
                                @Container3(style = "${styles.grid_large}4 ${styles.grid_cell}", htmlTemplates = {
                                    @HtmlTemplate(location = "component://shop/webapp/shop/request/requestInfo.ftl"
                                )}),
                                @Container3(style = "${styles.grid_large}4 ${styles.grid_cell}", htmlTemplates = {
                                    @HtmlTemplate(location = "component://order/webapp/ordermgr/request/requestDate.ftl"
                                )}),
                                @Container3(style = "${styles.grid_large}4 ${styles.grid_cell}", htmlTemplates = {
                                    @HtmlTemplate(location = "component://order/webapp/ordermgr/request/requestContactMech.ftl"
                                )})}),
                                @Container2(style = "${styles.grid_large}12", htmlTemplates = {
                                    @HtmlTemplate(location = "component://order/webapp/ordermgr/request/ViewRequestItemInfo.ftl"
                                ),
                                @HtmlTemplate(location = "component://shop/webapp/shop/request/requestRoles.ftl"
                            )})}), failWidgets = @WidgetsForContainer(value = {
                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderNoRequestFound}"
                            )}))}), failWidgets = @InlineWidgets(value = {
                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                            )}))})
        }
    )
    public interface ViewRequest {}

}
