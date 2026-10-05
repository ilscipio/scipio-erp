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
public class ContentContentScreens {

    @Screen(name = "FindContent", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindContent")
    @Action(type = ActionType.SET, field = "entityName", value = "Content")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findContent")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "30")
    @DecoratorScreen(
        name = "ContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", includeMenus = {
                            @IncludeMenu(name = "contentMenu", location = "component://content/widget/content/ContentMenus.xml"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindContent", location = "component://content/widget/content/ContentForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContent", location = "component://content/widget/content/ContentForms.xml"
                        )}))})})
        }
    )
    public interface FindContent {}

    @Screen(name = "findContentSearchResults", location = "component://content/widget/content/ContentScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListContent", location = "component://content/widget/content/ContentForms.xml")}))
    public interface findContentSearchResults {}

    @Screen(name = "navigateContent", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleNavigateContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "navigateContent")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleNavigateContent")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentAssoc", list = "contentAssoc", fieldMaps = {@FieldMap(fieldName = "contentId", value = "TREE_ROOT"), @FieldMap(fieldName = "contentAssocTypeId", value = "TREE_CHILD")})
    @DecoratorScreen(
        name = "ContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "left", screenlets = {
                    @ScreenletNested(containers = {
                    @ContainerInScreenlet(id = "EditDocumentTree"
                )}, includeScreens = {
                        @IncludeScreen(name = "navigateMenu", location = "component://content/widget/content/ContentScreens.xml"
                    
            )})}),
            @Container(style = "leftonly", screenlets = {
                @ScreenletNested(title = "${uiLabelMap.ContentContent}", containers = {
                    @ContainerInScreenlet(id = "Document", includeScreens = {
                        @IncludeScreen(name = "ListDocument", location = "component://content/widget/content/ContentScreens.xml"
                    
            )})})})})
        }
    )
    public interface navigateContent {}

    @Screen(name = "navigateMenu", location = "component://content/widget/content/ContentScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/content/ContentNav.ftl")}))
    public interface navigateMenu {}

    @Screen(name = "EditDocument", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(containers = {@Container(includeForms = {@IncludeForm(name = "AddDocument", location = "component://content/widget/content/ContentForms.xml")})}))
    public interface EditDocument {}

    @Screen(name = "ListDocument", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListDocument")
    @Action(type = ActionType.SET, field = "contentIdTo", fromField = "parameters.contentIdTo")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "viewSize", value = "${parameters.VIEW_SIZE}", valueType = "Integer", defaultValue = "30")
    @Action(type = ActionType.SET, field = "viewIndex", value = "${parameters.VIEW_INDEX}", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/content/GetContentLookupList.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/lookup/ContentTreeLookupList.ftl")}))
    public interface ListDocument {}

    @Screen(name = "ShowContent", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonExtUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId", defaultValue = "${contentId}")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/GetLayoutSettingsVisualThemeResources.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.CONTENT, contentId = "${contentId}")}))
    public interface ShowContent {}

    @Screen(name = "ShowContentPortlet", location = "component://content/widget/content/ContentScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.CONTENT, contentId = "${contentId}")}))
    public interface ShowContentPortlet {}

    @Screen(name = "EditDocumentTree", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/content/EditContentTree.ftl")}))
    public interface EditDocumentTree {}

    @Screen(name = "EditContent", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "content")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue")
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditContent", location = "component://content/widget/content/ContentForms.xml"
                )})})
        }
    )
    public interface EditContent {}

    @Screen(name = "EditContentAssoc", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentAssoc")
    @Action(type = ActionType.SET, field = "currentContentMenuItemName", value = "contentassoc")
    @Action(type = ActionType.SET, field = "extraFunctionName", value = "'${uiLabelMap.CommonFrom}'")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "association")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "contentIdTo", fromField = "parameters.contentIdTo")
    @Action(type = ActionType.SET, field = "contentAssocTypeId", fromField = "parameters.contentAssocTypeId", defaultValue = "${defaultContentAssocTypeId}")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue")
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleEditContentAssoc}", includeForms = {
                    @IncludeForm(name = "ListContentAssocFrom", location = "component://content/widget/content/ContentForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleListAssociations} '${uiLabelMap.CommonTo}'", includeForms = {
                    @IncludeForm(name = "ListContentAssocTo", location = "component://content/widget/content/ContentForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"contentId"}),
                        @Condition(type = NotEmpty.class, params = {"contentIdTo"}),
                        @Condition(type = NotEmpty.class, params = {"contentAssocTypeId"
                    }),
                    @Condition(type = NotEmpty.class, params = {"fromDate"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.PageTitleEditAssociation}'", includeForms = {
                            @IncludeForm(name = "EditContentAssoc", location = "component://content/widget/content/ContentForms.xml"
                        )})})),
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                            @OrCondition(ifEmpty = {"contentId", "contentIdTo", "contentAssocTypeId", "fromDate"
                        })}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.PageTitleAddAssociation}'", includeForms = {
                                @IncludeForm(name = "AddContentAssoc", location = "component://content/widget/content/ContentForms.xml"
                            )})}))})
        }
    )
    public interface EditContentAssoc {}

    @Screen(name = "EditContentRole", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentRole")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "role")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "contentRoleTarget")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContentRole", location = "component://content/widget/content/ContentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddRole}", name = "AddContentRolePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentRole", location = "component://content/widget/content/ContentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentRole {}

    @Screen(name = "EditContentPurpose", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentPurpose")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "purpose")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContentPurpose", location = "component://content/widget/content/ContentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddPurpose}", name = "AddContentPurposePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentPurpose", location = "component://content/widget/content/ContentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentPurpose {}

    @Screen(name = "EditContentAttribute", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentAttribute")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "attribute")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContentAttribute", location = "component://content/widget/content/ContentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddAttribute}", name = "AddContentAttributePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentAttribute", location = "component://content/widget/content/ContentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentAttribute {}

    @Screen(name = "ListWebSite", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindWebSite")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindWebSite")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "websites")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "WebSiteAndContent", list = "webSites", conditions = {@ConditionExpr(fieldName = "contentId", fromField = "parameters.contentId")}, orderBy = {"webSiteId"})
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleListWebSite}", includeForms = {
                    @IncludeForm(name = "ListWebSites", location = "component://content/widget/content/ContentForms.xml"
                )})})
        }
    )
    public interface ListWebSite {}

    @Screen(name = "EditContentMetaData", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentMetadata")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "metaData")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContentMetaData", location = "component://content/widget/content/ContentForms.xml"
            )}, screenlets = {
                @Screenlet(name = "AddContentMetaDataPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentMetaData", location = "component://content/widget/content/ContentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentMetaData {}

    @Screen(name = "EditContentWorkEfforts", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentWorkEffort")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "workEffort")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListWorkEffortContents", location = "component://content/widget/content/ContentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddWorkEffort}", name = "AddWorkEffortContentPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddWorkEffortContent", location = "component://content/widget/content/ContentForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditContentWorkEfforts {}

    @Screen(name = "LookupContent", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupContent}")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "20")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "LookupContent", location = "component://content/widget/content/ContentForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListLookupContent", location = "component://content/widget/content/ContentForms.xml"
            )})
        }
    )
    public interface LookupContent {}

    @Screen(name = "ListContentTree", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListContentTree")
    @Action(type = ActionType.SET, field = "contentIdTo", fromField = "parameters.contentIdTo")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "viewSize", value = "${parameters.VIEW_SIZE}", valueType = "Integer", defaultValue = "30")
    @Action(type = ActionType.SET, field = "viewIndex", value = "${parameters.VIEW_INDEX}", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/content/GetContentLookupList.groovy")
    @Section(widgets = @Widgets(containers = {@Container(id = "Document", htmlTemplates = {@HtmlTemplate(location = "component://content/webapp/content/lookup/ContentTreeLookupList.ftl")})}))
    public interface ListContentTree {}

    @Screen(name = "LookupContentTree", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupContent}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "LookupContentTree")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleNavigateContent")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentAssoc", list = "contentAssoc", fieldMaps = {@FieldMap(fieldName = "contentId", value = "TREE_ROOT"), @FieldMap(fieldName = "contentAssocTypeId", value = "TREE_CHILD")})
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(containers = {
                    @Container(style = "left-border", htmlTemplates = {
                        @HtmlTemplate(location = "component://content/webapp/content/content/ContentNav.ftl"
                    )}),
                    @Container(style = "leftonly", includeScreens = {
                        @IncludeScreen(name = "ListContentTree", location = "component://content/widget/content/ContentScreens.xml"
                    )})})})
        }
    )
    public interface LookupContentTree {}

    @Screen(name = "LookupDetailContentTree", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupContent}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "LookupDetailContentTree")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleNavigateContent")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentAssoc", list = "contentAssoc", fieldMaps = {@FieldMap(fieldName = "contentId", value = "TREE_ROOT"), @FieldMap(fieldName = "contentAssocTypeId", value = "TREE_CHILD")})
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(containers = {
                    @Container(style = "left-border", htmlTemplates = {
                        @HtmlTemplate(location = "component://content/webapp/content/content/ContentNav.ftl"
                    )}),
                    @Container(style = "leftonly", containers = {
                        @Container2(style = "contentarea", includeScreens = {
                            @IncludeScreen(name = "ViewContentDetail", location = "component://content/widget/content/ContentScreens.xml"
                        )})})})})
        }
    )
    public interface LookupDetailContentTree {}

    @Screen(name = "ViewContentDetail", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "contentIdTo", fromField = "parameters.contentIdTo")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "lookupContentDetail", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentId")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"lookupContentDetail"})}), widgets = @Widgets(containers = {@Container(id = "Document", labels = {@Label(text = "${uiLabelMap.PageTitlePleaseSelectData}")})}), failWidgets = @Widgets(containers = {@Container(id = "Document", includeForms = {@IncludeForm(name = "ViewContentDetail", location = "component://content/widget/content/ContentForms.xml")})}))
    public interface ViewContentDetail {}

    @Screen(name = "ContentSearchOptions", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchResults")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/content/ContentSearchOptions.groovy")
    @DecoratorScreen(
        name = "ContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/content/ContentSearchOptions.ftl"
            )})
        }
    )
    public interface ContentSearchOptions {}

    @Screen(name = "ContentSearchResults", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSearchResults")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/content/ContentSearchResults.groovy")
    @DecoratorScreen(
        name = "ContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://content/webapp/content/content/ContentSearchResults.ftl"
            )})
        }
    )
    public interface ContentSearchResults {}

    @Screen(name = "EditContentKeywords", location = "component://content/widget/content/ContentScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/cms/GetMenuContext.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditContentKeywords")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "keywords")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "currentValue", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId")})
    @DecoratorScreen(
        name = "CommonContentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddContentKeyword}", name = "AddContentKeywordsPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddContentKeyword", location = "component://content/widget/content/ContentForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleEditContentKeywords}", includeForms = {
                    @IncludeForm(name = "ListContentKeywords", location = "component://content/widget/content/ContentForms.xml"
                )})})
        }
    )
    public interface EditContentKeywords {}

}
