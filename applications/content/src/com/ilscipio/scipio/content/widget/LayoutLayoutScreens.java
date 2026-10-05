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
public class LayoutLayoutScreens {

    @Screen(name = "FindLayout", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindLayout")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditLayoutSubContent"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "findLayout", location = "component://content/widget/layout/LayoutForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "listFindLayout", location = "component://content/widget/layout/LayoutForms.xml"
                        )}))})})
        }
    )
    public interface FindLayout {}

    @Screen(name = "ListLayout", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListLayout")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentAssocDataResourceViewFrom", list = "layoutList", fieldMaps = {@FieldMap(fieldName = "caContentIdTo", value = "TEMPLATE_MASTER")})
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "listListLayout", location = "component://content/widget/layout/LayoutForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonCreateNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditLayoutSubContent"
                    )}, position = 0)})})
        }
    )
    public interface ListLayout {}

    @Screen(name = "EditLayout", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditLayout")
    @Action(type = ActionType.SET, field = "wrapTemplateId", value = "STDWRAP001")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContentDataResourceView", valueField = "currentValue")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditLayout", location = "component://content/widget/layout/LayoutForms.xml", position = 0
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://content/webapp/content/layout/renderSubContent.ftl", position = 2
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCloneLayout}", style = "${styles.link_run_sys} ${styles.action_copy}", target = "cloneLayout", position = 1
                )})})
        }
    )
    public interface EditLayout {}

    @Screen(name = "EditLayoutSubContent", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditLayoutSubContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditLayoutSubContent")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SubContentDataResourceView", valueField = "currentValue")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/layout/EditSubContent.groovy")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditLayoutSubContent", location = "component://content/widget/layout/LayoutForms.xml"
                )})})
        }
    )
    public interface EditLayoutSubContent {}

    @Screen(name = "EditLayoutText", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditLayoutText")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SubContentDataResourceView", valueField = "currentValue")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/layout/EditSubContent.groovy")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body")
        }
    )
    public interface EditLayoutText {}

    @Screen(name = "EditLayoutHtml", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditLayoutHtml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SubContentDataResourceView", valueField = "currentValue")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/layout/EditSubContent.groovy")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body")
        }
    )
    public interface EditLayoutHtml {}

    @Screen(name = "EditLayoutUrl", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditLayoutUrl")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SubContentDataResourceView", valueField = "currentValue")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/layout/EditSubContent.groovy")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body")
        }
    )
    public interface EditLayoutUrl {}

    @Screen(name = "EditLayoutImage", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditLayoutImage")
    @Action(type = ActionType.ENTITY_ONE, entityName = "SubContentDataResourceView", valueField = "currentValue")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/layout/EditSubContent.groovy")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body")
        }
    )
    public interface EditLayoutImage {}

    @Screen(name = "AddLayout", location = "component://content/widget/layout/LayoutScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditLayout")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditLayout")
    @DecoratorScreen(
        name = "CommonLayoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AddLayout", location = "component://content/widget/layout/LayoutForms.xml"
                )})})
        }
    )
    public interface AddLayout {}

}
