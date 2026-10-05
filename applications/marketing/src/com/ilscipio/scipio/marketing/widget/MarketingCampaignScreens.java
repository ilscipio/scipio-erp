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
public class MarketingCampaignScreens {

    @Screen(name = "EditMarketingCampaign", location = "component://marketing/widget/MarketingCampaignScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MarketingCampaign")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "marketingCampaignId", fromField = "parameters.marketingCampaignId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingCampaign} ${marketingCampaignId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "MarketingCampaign", valueField = "marketingCampaign")
    @DecoratorScreen(
        name = "CommonMarketingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditMarketingCampaign", location = "component://marketing/widget/MarketingCampaignForms.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"marketingCampaign"}
                )}), position = 0)})
        }
    )
    public interface EditMarketingCampaign {}

    @Screen(name = "FindMarketingCampaign", location = "component://marketing/widget/MarketingCampaignScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MarketingCampaign")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "marketingCampaignId", fromField = "parameters.marketingCampaignId")
    @Action(type = ActionType.SET, field = "entityName", value = "MarketingCampaign")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingCampaign} ${marketingCampaignId}")
    @Action(type = ActionType.SET, field = "showActionButtons", value = "Y")
    @DecoratorScreen(
        name = "CommonMarketingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"MARKETING", "_CREATE"
                })}), widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.MarketingCampaignCreate}", style = "${styles.link_nav} ${styles.action_add}", target = "EditMarketingCampaign"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/campaign/FindMarketingCampaing.ftl"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "MarketingCampaignSearchResults"
                        )}))})))})
        }
    )
    public interface FindMarketingCampaign {}

    @Screen(name = "MarketingCampaignSearchResults", location = "component://marketing/widget/MarketingCampaignScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/campaign/FindMarketingCampaign.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/campaign/MarketingCampaignList.ftl")}))
    public interface MarketingCampaignSearchResults {}

}
