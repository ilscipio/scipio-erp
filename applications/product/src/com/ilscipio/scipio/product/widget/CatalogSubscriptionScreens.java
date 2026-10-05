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
public class CatalogSubscriptionScreens {

    @Screen(name = "FindSubscription", location = "component://product/widget/catalog/SubscriptionScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindSubscription")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Subscription")
    @Action(type = ActionType.SET, field = "isSpecificSubscription", value = "false", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonSubscriptionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"subscriptionPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindSubscription", location = "component://product/widget/catalog/SubscriptionForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFindSubscription", location = "component://product/widget/catalog/SubscriptionForms.xml"
                    )}))})), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductSubscriptionViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface FindSubscription {}

    @Screen(name = "EditSubscription", location = "component://product/widget/catalog/SubscriptionScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSubscription")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSubscription")
    @Action(type = ActionType.SET, field = "subscriptionId", fromField = "parameters.subscriptionId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Subscription", valueField = "subscription")
    @Action(type = ActionType.SET, field = "isCreateSubscription", value = "${groovy: !(context.subscription || (parameters.subscriptionId && parameters.isCreate != 'true'))}", valueType = "Boolean")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"subscriptionId"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "ProductNewSubscription")}))
    @DecoratorScreen(
        name = "CommonSubscriptionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(name = "editSubscription", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditSubscription", location = "component://product/widget/catalog/SubscriptionForms.xml"
                )})})
        }
    )
    public interface EditSubscription {}

    @Screen(name = "EditSubscriptionAttributes", location = "component://product/widget/catalog/SubscriptionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSubscriptionAttributes")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSubscriptionAttributes")
    @Action(type = ActionType.SET, field = "subscriptionId", fromField = "parameters.subscriptionId")
    @Action(type = ActionType.ENTITY_AND, entityName = "SubscriptionAttribute", list = "subscriptionAttributes", fieldMaps = {@FieldMap(fieldName = "subscriptionId")}, orderBy = {"attrName"})
    @DecoratorScreen(
        name = "CommonSubscriptionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditSubscriptionAttributes", location = "component://product/widget/catalog/SubscriptionForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddSubscriptionAttributes}", name = "addSubscriptionAttribute", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSubscriptionAttribute", location = "component://product/widget/catalog/SubscriptionForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditSubscriptionAttributes {}

    @Screen(name = "FindSubscriptionResource", location = "component://product/widget/catalog/SubscriptionScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindSubscriptionResource")
    @DecoratorScreen(
        name = "CommonSubscriptionResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"subscriptionPermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSubscriptionResources", location = "component://product/widget/catalog/SubscriptionForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductNewSubscriptionResource}", style = "${styles.link_nav} ${styles.action_add}", target = "EditSubscriptionResource"
                    )}, position = 0)}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ProductSubscriptionViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface FindSubscriptionResource {}

    @Screen(name = "EditSubscriptionResource", location = "component://product/widget/catalog/SubscriptionScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSubscriptionResource")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSubscriptionResource")
    @Action(type = ActionType.SET, field = "subscriptionResourceId", fromField = "parameters.subscriptionResourceId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SubscriptionResource", valueField = "subscriptionResource")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"subscriptionResourceId"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "ProductNewSubscriptionResource")}))
    @DecoratorScreen(
        name = "CommonSubscriptionResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(name = "EditSubscriptionResource", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditSubscriptionResource", location = "component://product/widget/catalog/SubscriptionForms.xml"
                )})})
        }
    )
    public interface EditSubscriptionResource {}

    @Screen(name = "EditSubscriptionResourceProducts", location = "component://product/widget/catalog/SubscriptionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSubscriptionResourceProducts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSubscriptionResourceProducts")
    @Action(type = ActionType.SET, field = "subscriptionResourceId", fromField = "parameters.subscriptionResourceId")
    @DecoratorScreen(
        name = "CommonSubscriptionResourceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListSubscriptionResourceProducts", location = "component://product/widget/catalog/SubscriptionForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddSubscriptionResourceProducts}", name = "addSubscriptionResourceProduct", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddSubscriptionResourceProduct", location = "component://product/widget/catalog/SubscriptionForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditSubscriptionResourceProducts {}

    @Screen(name = "EditSubscriptionCommEvent", location = "component://product/widget/catalog/SubscriptionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditSubscriptionCommEvent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditSubscriptionCommEvent")
    @Action(type = ActionType.SET, field = "subscriptionId", fromField = "parameters.subscriptionId")
    @Action(type = ActionType.ENTITY_AND, entityName = "SubscriptionAndCommEvent", list = "subscriptionCommEvent", fieldMaps = {@FieldMap(fieldName = "subscriptionId")})
    @DecoratorScreen(
        name = "CommonSubscriptionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listSubscriptionCommEvent", location = "component://product/widget/catalog/SubscriptionForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddSubscriptionCommEvent}", name = "addSubscriptionCommEvent", collapsible = true, includeForms = {
                    @IncludeForm(name = "createSubscriptionCommEvent", location = "component://product/widget/catalog/SubscriptionForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditSubscriptionCommEvent {}

}
