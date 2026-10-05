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
package com.ilscipio.scipio.webtools.widget;

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
public class LabelManagerScreens {

    @Screen(name = "SearchLabels", location = "component://webtools/widget/LabelManagerScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"LABEL_MANAGER_VIEW"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsLabelManagerSecurityError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsLabelManagerFindLabels")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "labels")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/labelmanager/LabelManager.groovy")
    @DecoratorScreen(
        name = "CommonLabelDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/labelmanager/SearchLabels.ftl"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/labelmanager/ViewLabels.ftl"
                    )}))})})
        }
    )
    public interface SearchLabels {}

    @Screen(name = "UpdateLabel", location = "component://webtools/widget/LabelManagerScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"LABEL_MANAGER_VIEW"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsLabelManagerSecurityError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsLabelManagerAddNew")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/labelmanager/UpdateManager.groovy")
    @DecoratorScreen(
        name = "CommonLabelDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/labelmanager/UpdateLabel.ftl"
            )})
        }
    )
    public interface UpdateLabel {}

    @Screen(name = "ViewReferences", location = "component://webtools/widget/LabelManagerScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"LABEL_MANAGER_VIEW"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsLabelManagerSecurityError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsLabelManagerViewReferences")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/labelmanager/ViewReferences.groovy")
    @DecoratorScreen(
        name = "CommonLabelDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/labelmanager/ViewReferences.ftl"
            )})
        }
    )
    public interface ViewReferences {}

    @Screen(name = "ViewFile", location = "component://webtools/widget/LabelManagerScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"LABEL_MANAGER_VIEW"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsLabelManagerSecurityError}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsLabelManagerViewFile")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/labelmanager/ViewFile.groovy")
    @DecoratorScreen(
        name = "CommonLabelDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(name = "ViewFilePanel", htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/labelmanager/ViewFile.ftl"
                )})})
        }
    )
    public interface ViewFile {}

    @Screen(name = "EntityLabels", location = "component://webtools/widget/LabelManagerScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsEntityLabels")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EntityLabels")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/labelmanager/EntityLabels.groovy")
    @DecoratorScreen(
        name = "CommonLabelDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = HasPermission.class, params = {"LABEL_MANAGER_VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/labelmanager/EntityLabels.ftl"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsLabelManagerSecurityError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface EntityLabels {}

}
