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
package com.ilscipio.scipio.common.widget;

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
public class HelpScreens {

    @Screen(name = "LookupDecorator", location = "component://common/widget/HelpScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SERVICE, serviceName = "getUserPreferenceGroup", resultMapName = "prefResult", fieldMaps = {@FieldMap(fieldName = "userPrefGroupTypeId", value = "GLOBAL_PREFERENCES")})
    @Action(type = ActionType.SET, field = "userPreferences", fromField = "prefResult.userPrefMap", global = true)
    @Action(type = ActionType.SET, field = "lookupType", value = "HELP")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLayoutSettingsVisualThemeResources.groovy")
    @Action(type = ActionType.SET, field = "messagesTemplateLocation", fromField = "layoutSettings.VT_MSG_TMPLT_LOC[0]", defaultValue = "component://common/webcommon/includes/messages.ftl")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/lookup.ftl", position = 0), @Widget(type = WidgetType.HTML_TEMPLATE, location = "${messagesTemplateLocation}", position = 1), @Widget(type = WidgetType.CONTAINER, style = "clear", position = 3), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/lookupFooter.ftl", position = 4)}, containers = {@Container(style = "${styles.grid_row}", containers = {@Container2(id = "${styles.grid_large}12 columns", includeScreens = {@IncludeScreen(name = "${leftbarScreenName}", location = "${leftbarScreenLocation}")}, containers = {@Container3(id = "content-main-section", style = "${MainColumnStyle}", decoratorSectionIncludes = {@DecoratorSectionInclude(name = "body")})})}, position = 2)}))
    public interface LookupDecorator {}

    @Screen(name = "ShowHelp", location = "component://common/widget/HelpScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.helpTopic", "equals", "navigateHelp"}), @Condition(type = Empty.class, params = {"parameters.portalPageId"})}), failWidgets = @WidgetsForContainer(sections = {@SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"parameters.portalPageId"})}), actions = @Actions(value = {@Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentAssoc", list = "contentAssocs", conditions = {@ConditionExpr(fieldName = "mapKey", fromField = "parameters.helpTopic"), @ConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, orderBy = {"sequenceNum"}), @Action(type = ActionType.SET, field = "contentId", fromField = "contentAssocs[0].contentIdTo"), @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")}), widgets = @WidgetsForContainer2(sections = {@SectionNested3(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"content"})}), widgets = @WidgetsForContainer3(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "navigateHelp")}), failWidgets = @WidgetsForContainer3(decorator = @DecoratorScreenNested(name = "LookupDecorator", location = "component://common/widget/HelpScreens.xml", sections = {@DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(screenlets = {@ScreenletNested(title = "${uiLabelMap.CommonExtHelpTitle}", navigationMenuName = "lookupMenu", includeMenus = {
                    @IncludeMenu(name = "lookupMenu", location = "component://content/widget/content/ContentMenus.xml", position = 0
                )}, widgets = {
                    @Widget(type = WidgetType.ITERATE_SECTION, list = "contentAssocs", entry = "contentAssoc", name = "ShowHelp-iterate1", location = "component://common/widget/HelpScreens.xml", position = 1
                )})}))})))}), failWidgets = @WidgetsForContainer2(sections = {@SectionNested3(actions = @Actions(value = {@Action(type = ActionType.ENTITY_ONE, entityName = "PortalPage", valueField = "portalPageTmp", useCache = true), @Action(type = ActionType.SET, field = "originalPortalPageId", fromField = "portalPageTmp.originalPortalPageId", defaultValue = "${parameters.portalPageId}"), @Action(type = ActionType.ENTITY_ONE, entityName = "PortalPage", valueField = "portalPage", useCache = true, fieldMaps = {@FieldMap(fieldName = "portalPageId", fromField = "originalPortalPageId")})}), widgets = @WidgetsForContainer3(decorator = @DecoratorScreenNested(name = "LookupDecorator", location = "component://common/widget/HelpScreens.xml", sections = {@DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(screenlets = {@ScreenletNested(title = "${uiLabelMap.CommonExtHelpTitle}", navigationMenuName = "lookupMenu", includeMenus = {
                    @IncludeMenu(name = "lookupMenu", location = "component://content/widget/content/ContentMenus.xml", position = 0
                )}, widgets = {
                    @Widget(type = WidgetType.CONTENT, contentId = "${portalPage.helpContentId}", position = 1
                )}), @ScreenletNested(title = "${uiLabelMap.CommonSelectPortletToHelp}", includeForms = {
                    @IncludeForm(name = "PortletList", location = "component://common/widget/PortalPageForms.xml"
                )})}))})))}))}))
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "navigateHelp")}))
    public interface ShowHelp {}

    @Screen(name = "ShowHelp-iterate1", location = "component://common/widget/HelpScreens.xml")
    @Action(type = ActionType.SET, field = "contentId", fromField = "contentAssoc.contentIdTo")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "showDocument")}))
    public interface ShowHelp_iterate1 {}

    @Screen(name = "showDocument", location = "component://common/widget/HelpScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonExtUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId", defaultValue = "${contentId}")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLayoutSettingsVisualThemeResources.groovy")
    @Section(widgets = @Widgets(containers = {@Container(id = "Document", widgets = {@Widget(type = WidgetType.CONTENT, contentId = "${contentId}")})}))
    public interface showDocument {}

    @Screen(name = "navigateHelp", location = "component://common/widget/HelpScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNavigateContent")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentAssoc", list = "contentAssoc", conditions = {@ConditionExpr(fieldName = "contentId", value = "HELP_ROOT"), @ConditionExpr(fieldName = "contentAssocTypeId", value = "TREE_CHILD"), @ConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, orderBy = {"sequenceNum"})
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId", defaultValue = "HELP_ROOT")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/HelpScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(containers = {
                    @Container(style = "left-border", htmlTemplates = {
                        @HtmlTemplate(location = "component://content/webapp/content/content/DisplayContentNav.ftl", position = 1
                    )}, containers = {
                        @Container2(id = "EditDocumentTree", position = 0)}),
                        @Container(style = "leftonly", includeScreens = {
                            @IncludeScreen(name = "showDocument", location = "component://common/widget/HelpScreens.xml"
                        )})})})
        }
    )
    public interface navigateHelp {}

}
