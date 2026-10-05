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
package com.ilscipio.scipio.content.widget;

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
public class WebAnalyticsScreens {

    @Screen(name = "FindWebAnalyticsConfigs", location = "component://content/widget/WebAnalyticsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CatalogWebSiteWebAnalytics")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "CatalogWebSiteWebAnalytics")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WebAnalytics")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "FindWebAnalyticsConfigs")
    @Action(type = ActionType.SET, field = "webAnalyticsConfigCtx", fromField = "parameters")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonWebAnalyticsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListWebAnalyticsConfig", location = "component://content/widget/website/WebAnalyticsForms.xml"
                )})})
        }
    )
    public interface FindWebAnalyticsConfigs {}

    @Screen(name = "EditWebAnalyticsConfig", location = "component://content/widget/WebAnalyticsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CatalogWebSiteWebAnalyticsConfigs")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "CatalogWebSiteWebAnalyticsConfigs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "WebAnalytics")
    @Action(type = ActionType.SET, field = "activeSubMenuItem2", value = "EditWebAnalyticsConfig")
    @Action(type = ActionType.SET, field = "webSiteId", fromField = "parameters.webSiteId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebSite", valueField = "webSite")
    @Action(type = ActionType.SET, field = "webAnalyticsTypeId", fromField = "parameters.webAnalyticsTypeId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WebAnalyticsConfig", valueField = "webAnalyticsConfig")
    @DecoratorScreen(
        name = "CommonWebAnalyticsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditWebAnalyticsConfig", location = "component://content/widget/website/WebAnalyticsForms.xml"
                )})})
        }
    )
    public interface EditWebAnalyticsConfig {}

}
