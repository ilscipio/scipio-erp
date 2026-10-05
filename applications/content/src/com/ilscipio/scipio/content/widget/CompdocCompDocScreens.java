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
public class CompdocCompDocScreens {

    @Screen(name = "ListContentApproval", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentId}")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${parameters.contentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentId}")
    @Action(type = ActionType.SET, field = "contentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${parameters.contentRevisionSeqId}")
    @Action(type = ActionType.SERVICE, serviceName = "getMostRecentRevision", resultMapName = "revisionResult", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rootContentId")})
    @Action(type = ActionType.SET, field = "mostRecentRevisionSeqId", fromField = "revisionResult.mostRecentRevisionSeqId")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "approval")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true)
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.SERVICE, serviceName = "getApprovalsWithPermissions", resultMapName = "result", fieldMaps = {@FieldMap(fieldName = "rootContentId", fromField = "contentId"), @FieldMap(fieldName = "contentRevisionSeqId", fromField = "contentRevisionSeqId"), @FieldMap(fieldName = "checkPermission", value = "false")})
    @Action(type = ActionType.SET, field = "contentApprovalList", fromField = "result.contentApprovalList")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentCompDocApprovalsFor} ${contentId}, ${uiLabelMap.ContentCompDocRev} ${contentRevisionSeqId}")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocViewWaitingApprovals}", style = "${styles.link_nav} ${styles.action_view}", target = "ListWaitingContentApproval"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleListContentApproval}", includeForms = {
                        @IncludeForm(name = "ListContentApproval", location = "component://content/widget/compdoc/CompDocForms.xml"
                    )}, position = 2)}, sections = {
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"content.contentTypeId", "equals", "COMPDOC_INSTANCE"
                        })}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "rootInstanceApprovalStatus"
                        )}), position = 1),
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"content.contentTypeId", "equals", "COMPDOC_TEMPLATE"
                        })}), actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditCompDocInstance} [${rootContentId}]"
                        ),
                        @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.contentRevisionSeqId", defaultValue = "${parameters.rootContentRevisionSeqId}"
                    )}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.CommonAdd}", includeForms = {
                            @IncludeForm(name = "AddContentApproval", location = "component://content/widget/compdoc/CompDocForms.xml"
                        )})}), position = 3)})
        }
    )
    public interface ListContentApproval {}

    @Screen(name = "ListWaitingContentApproval", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentId}")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${parameters.contentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentId}")
    @Action(type = ActionType.SET, field = "contentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${parameters.contentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true)
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.SET, field = "menuName", value = "empty")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "approval")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentCompDocViewWaitingApprovals")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "approval")
    @Action(type = ActionType.SERVICE, serviceName = "checkForWaitingApprovals", resultMapName = "result")
    @Action(type = ActionType.SET, field = "contentApprovalList", fromField = "result.contentApprovalList")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListWaitingContentApproval", location = "component://content/widget/compdoc/CompDocForms.xml"
                )})})
        }
    )
    public interface ListWaitingContentApproval {}

    @Screen(name = "EditContentApproval", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "approval")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContentApproval", valueField = "contentApproval", useCache = true, fieldMaps = {@FieldMap(fieldName = "contentApprovalId", fromField = "parameters.contentApprovalId")})
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "contentApproval.contentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "contentApproval.contentRevisionSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true, fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentApproval.contentId")})
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditContentApprovalEditPage} ${contentApproval.contentApprovalId}")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditContentApproval", location = "component://content/widget/compdoc/CompDocForms.xml"
                )})})
        }
    )
    public interface EditContentApproval {}

    @Screen(name = "ListContentRevisions", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "rootContentId")
    @Action(type = ActionType.SET, field = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true)
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "revision")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true, fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentId")})
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentRevision", list = "contentRevisionList", useCache = true, fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentId")})
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentCompDocRevisionListPageForContent} ${rootContentId} ${uiLabelMap.ContentCompDocRev} ${rootContentRevisionSeqId}")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListContentRevisions", location = "component://content/widget/compdoc/CompDocForms.xml"
                )})})
        }
    )
    public interface ListContentRevisions {}

    @Screen(name = "EditContentRevision", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentId}")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${parameters.contentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "contentRevisionSeqId", fromField = "parameters.contentRevisionSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true)
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "revision")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentCompDocRevisionListPageForContent} ${rootContentId} ${uiLabelMap.ContentCompDocRevisions} ${rootContentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContentRevision", valueField = "contentRevision", useCache = true)
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditContentRevision", location = "component://content/widget/compdoc/CompDocForms.xml"
                )})})
        }
    )
    public interface EditContentRevision {}

    @Screen(name = "ListContentRevisionItem", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "rootContentId")
    @Action(type = ActionType.SET, field = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true)
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "revision")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentCompDocRevisionListPageForContent} ${rootContentId} ${uiLabelMap.ContentCompDocRevisions} ${rootContentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_AND, entityName = "ContentRevisionItem", list = "contentRevisionItemList", useCache = true, fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.contentId"), @FieldMap(fieldName = "contentRevisionSeqId", fromField = "parameters.contentRevisionSeqId")})
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListContentRevisionItem", location = "component://content/widget/compdoc/CompDocForms.xml"
                )})})
        }
    )
    public interface ListContentRevisionItem {}

    @Screen(name = "EditContentRevisionItem", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId", defaultValue = "${rootContentId}")
    @Action(type = ActionType.SET, field = "contentRevisionSeqId", fromField = "parameters.contentRevisionSeqId", defaultValue = "${rootContentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true)
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "revision")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentCompDocRevisionItemEditPage} ${contentId} ${uiLabelMap.ContentCompDocRevisions} ${contentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContentRevisionItem", valueField = "contentRevisionItem", useCache = true)
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditContentRevisionItem", location = "component://content/widget/compdoc/CompDocForms.xml"
                )})})
        }
    )
    public interface EditContentRevisionItem {}

    @Screen(name = "FindCompDoc", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Action(type = ActionType.SET, field = "menuName", value = "empty")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindCompDoc")
    @Action(type = ActionType.SET, field = "entityName", value = "ContentAssocViewFrom")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "currentContentMenuItemName")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "20")
    @Action(type = ActionType.SET, field = "dataResourceId")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(widgets = @InlineWidgets(decorator = @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleCreateNewRootCompDocTemplate}", style = "${styles.link_nav} ${styles.action_add}", target = "AddRootCompDocTemplate"
                        ),
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocViewWaitingApprovals}", style = "${styles.link_nav} ${styles.action_view}", target = "ListWaitingContentApproval"
                    )})})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindCompDoc", location = "component://content/widget/compdoc/CompDocForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCompDoc", location = "component://content/widget/compdoc/CompDocForms.xml"
                    )}))})))})
        }
    )
    public interface FindCompDoc {}

    @Screen(name = "ViewInstances", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "entityName", value = "ContentAssocViewFrom")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "currentContentMenuItemName")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "requestParameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "requestParameters.VIEW_SIZE", valueType = "Integer", defaultValue = "20")
    @Action(type = ActionType.SET, field = "contentTypeId", value = "COMPDOC_TEMPLATE")
    @Action(type = ActionType.ENTITY_AND, entityName = "Content", list = "compDocFindList", fieldMaps = {@FieldMap(fieldName = "instanceOfContentId", fromField = "parameters.rootContentId"), @FieldMap(fieldName = "contentTypeId", value = "COMPDOC_INSTANCE")})
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentCompDocViewInstances} ${parameters.rootContentId}")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListCompDocInstances", location = "component://content/widget/compdoc/CompDocForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleCreateNewRootCompDocTemplate}", style = "${styles.link_nav} ${styles.action_add}", target = "AddRootCompDocTemplate"
                    )}, position = 0)})})
        }
    )
    public interface ViewInstances {}

    @Screen(name = "EditRootCompDoc", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "edit")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentId}")
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "contentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "dataResource", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "content.dataResourceId")})
    @Action(type = ActionType.SET, field = "mimeTypeId", fromField = "dataResource.mimeTypeId")
    @Action(type = ActionType.SET, field = "contentTypeId", fromField = "content.contentTypeId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentRevision", list = "contentRevisions", useCache = true, conditions = {@ConditionExpr(fieldName = "contentId", operator = "equals", fromField = "rootContentId")}, orderBy = {"-contentRevisionSeqId"})
    @Action(type = ActionType.SET, field = "contentRevisionSeqId", fromField = "parameters.contentRevisionSeqId", defaultValue = "${contentRevisions[0].contentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "contentRevisionSeqId")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"contentTypeId", "equals", "COMPDOC_TEMPLATE"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditCompDocTemplate} ${rootContentId}"), @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.contentRevisionSeqId", defaultValue = "${contentRevisions[0].contentRevisionSeqId}")}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditRootCompDocTemplate", location = "component://content/widget/compdoc/CompDocForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleCreateInstanceOfThisTemplate}", style = "${styles.link_nav} ${styles.action_add}", target = "AddRootCompDocInstance"
                    ),
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocViewInstances}", style = "${styles.link_nav} ${styles.action_view}", target = "ViewInstances"
                )}, position = 0)})})
        }
    )))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"content.contentTypeId", "equals", "COMPDOC_INSTANCE"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditCompDocInstance} [${rootContentId}]"), @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.contentRevisionSeqId", defaultValue = "${parameters.rootContentRevisionSeqId}")}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditRootCompDocInstance", location = "component://content/widget/compdoc/CompDocForms.xml"
                )})})
        }
    )))
    public interface EditRootCompDoc {}

    @Screen(name = "EditChildCompDoc", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_CREATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "menuName", value = "subtree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "edit")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.caContentIdTo}")
    @Action(type = ActionType.SERVICE, serviceName = "getMostRecentRevision", resultMapName = "revisionResult", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rootContentId")})
    @Action(type = ActionType.SET, field = "mostRecentRevisionSeqId", fromField = "revisionResult.mostRecentRevisionSeqId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${parameters.contentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId", defaultValue = "${mostRecentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "itemContent", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "contentId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContentAssoc", valueField = "contentAssoc", fieldMaps = {@FieldMap(fieldName = "contentIdTo", fromField = "rootContentId"), @FieldMap(fieldName = "contentId", fromField = "contentId"), @FieldMap(fieldName = "fromDate", fromField = "parameters.caFromDate"), @FieldMap(fieldName = "contentAssocTypeId", value = "COMPDOC_PART")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentRevisionItem", list = "contentRevisionItems", useCache = true, conditions = {@ConditionExpr(fieldName = "contentId", fromField = "rootContentId"), @ConditionExpr(fieldName = "itemContentId", fromField = "contentId"), @ConditionExpr(fieldName = "contentRevisionSeqId", operator = "less-equals", fromField = "parameters.contentRevisionSeqId", ignoreIfEmpty = true)}, orderBy = {"-contentRevisionSeqId"})
    @Action(type = ActionType.SET, field = "itemContentRevisionSeqId", fromField = "parameters.itemContentRevisionSeqId", defaultValue = "${contentRevisionItems[0].contentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "dataResourceId", fromField = "contentRevisionItems[0].newDataResourceId", defaultValue = "${itemContent.dataResourceId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "dataResource", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "dataResourceId")})
    @Action(type = ActionType.SET, field = "mimeTypeId", fromField = "dataResource.mimeTypeId")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"itemContent.contentTypeId", "equals", "TEMPLATE"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditCompDocTemplate} [${contentId}, part of ${rootContentId}]"), @Action(type = ActionType.SET, field = "childCompDocTarget", value = "updateChildCompDocTemplate"), @Action(type = ActionType.SET, field = "contentTypeId", value = "TEMPLATE")}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_FORM, name = "EditChildCompDoc", location = "component://content/widget/compdoc/CompDocForms.xml", position = 1
                ),
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ContentViewLink", position = 2
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "UploadCompDocContent", position = 3
            )}, containers = {
                @Container(labels = {
                    @Label(text = "${uiLabelMap.ContentEditingLatestRevision} [${mostRecentRevisionSeqId}]", style = "tableheadtext"
                )}, position = 0)})),
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"mostRecentRevisionSeqId"
                }),
                @Condition(type = NotEmpty.class, params = {"rootContentRevisionSeqId"
            }),
            @Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "greater", "rootContentRevisionSeqId"
            })}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ViewChildCompDoc", location = "component://content/widget/compdoc/CompDocForms.xml", position = 1
            )}, containers = {
                @Container(includeScreens = {
                    @IncludeScreen(name = "ContentViewLink", location = "component://content/widget/compdoc/CompDocScreens.xml", position = 2
                )}, labels = {
                    @Label(text = "${uiLabelMap.ContentCompDocRevisions} [${rootContentRevisionSeqId}], Latest is [${mostRecentRevisionSeqId}]", style = "tableheadtext", position = 0
                )}, widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentLatest}", style = "${styles.link_run_sys} ${styles.action_find}", target = "EditChildCompDoc", position = 1
                )}, position = 0)}))})
        }
    )))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"itemContent.contentTypeId", "equals", "DOCUMENT"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditCompDocInstance} [${contentId} of ${contentId}]"), @Action(type = ActionType.SET, field = "childCompDocTarget", value = "updateChildCompDocInstance"), @Action(type = ActionType.SET, field = "contentTypeId", value = "DOCUMENT"), @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "templateContent", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "itemContent.instanceOfContentId")}), @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "templateDataResource", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "templateContent.dataResourceId")})}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "ContentViewLink", location = "component://content/widget/compdoc/CompDocScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "EditChildCompDoc", location = "component://content/widget/compdoc/CompDocForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "UploadCompDocContent", location = "component://content/widget/compdoc/CompDocScreens.xml"
            )})
        }
    )))
    public interface EditChildCompDoc {}

    @Screen(name = "ContentViewLink", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/msword"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/pdf"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/vnd.oasis.opendocument.text"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/jpeg"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/gif"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/tiff"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/png")})}), widgets = @Widgets(containers = {@Container(widgets = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleViewCompDocContentBinary}", style = "${styles.link_run_sys} ${styles.action_view}", target = "ViewCompDocContentBinary")})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "text/html"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "text/plain")})}), widgets = @Widgets(containers = {@Container(widgets = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleViewCompDocContentHtml}", style = "${styles.link_run_sys} ${styles.action_view}", target = "ViewCompDocContentHtml")})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/vnd.ofbiz.survey"})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/vnd.ofbiz.survey.response"})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/msword"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/pdf"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/vnd.oasis.opendocument.text"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/jpeg"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/gif"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/tiff"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/png"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "text/html"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "text/plain"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.dataResourceTypeId", "equals", "SURVEY"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.dataResourceTypeId", "equals", "SURVEY_RESPONSE"})})}))
    public interface ContentViewLink {}

    @Screen(name = "UploadCompDocContent", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/msword"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/pdf"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/vnd.oasis.opendocument.text"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/jpeg"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/gif"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/tiff"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "image/png")})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "UploadCompDocContent", location = "component://content/widget/compdoc/CompDocForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "text/html"), @IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "text/plain")})}), actions = @Actions(value = {@Action(type = ActionType.ENTITY_ONE, entityName = "ElectronicText", valueField = "electronicText", fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "dataResource.dataResourceId")}), @Action(type = ActionType.SET, field = "textData", fromField = "electronicText.textData")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "EditCompDocTextContent", location = "component://content/widget/compdoc/CompDocForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/vnd.ofbiz.survey")})}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "UploadCompDocPdf2Survey", location = "component://content/widget/compdoc/CompDocForms.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.mimeTypeId", operator = "equals", value = "application/vnd.ofbiz.survey.response")})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/msword"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/pdf"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/vnd.oasis.opendocument.text"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/jpeg"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/gif"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/tiff"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "image/png"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "text/html"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "text/plain"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/vnd.ofbiz.survey.response"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.mimeTypeId", "equals", "application/vnd.ofbiz.survey"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.dataResourceTypeId", "equals", "SURVEY"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.dataResourceTypeId", "equals", "SURVEY_RESPONSE"})})}))
    public interface UploadCompDocContent {}

    @Screen(name = "ViewCompDocContentHtml", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.CONTENT, contentId = "${parameters.contentId}")}))
    public interface ViewCompDocContentHtml {}

    @Screen(name = "AddRootCompDocInstance", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_CREATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "contentTypeId", value = "COMPDOC_INSTANCE")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentRevision", list = "contentRevisions", useCache = true, conditions = {@ConditionExpr(fieldName = "contentId", operator = "equals", fromField = "rootContentId")}, orderBy = {"-contentRevisionSeqId"})
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${contentRevisions[0].contentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "templateContent", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rootContentId")})
    @Action(type = ActionType.SET, field = "contentName", fromField = "templateContent.contentName")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentCompDocAddDocumentInstancePageForTemplate}: ${rootContentId}")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddRootCompDocInstance", location = "component://content/widget/compdoc/CompDocForms.xml"
            )})
        }
    )
    public interface AddRootCompDocInstance {}

    @Screen(name = "AddRootCompDocTemplate", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_CREATE"})}))
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddCompositeDocumentTemplate")
    @Action(type = ActionType.SET, field = "contentTypeId", value = "COMPDOC_TEMPLATE")
    @Action(type = ActionType.SET, field = "createChildCompDoc", value = "createChildCompDocTemplate")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddRootCompDocTemplate", location = "component://content/widget/compdoc/CompDocForms.xml"
            )})
        }
    )
    public interface AddRootCompDocTemplate {}

    @Screen(name = "AddChildCompDocInstance", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_CREATE"})}))
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddCompositeDocumentInstance")
    @Action(type = ActionType.SET, field = "contentIdTo", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "templateContentId", fromField = "parameters.instanceOfContentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "templateContent", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "templateContentId")})
    @Action(type = ActionType.SET, field = "contentName", fromField = "templateContent.contentName")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "templateContent", relationName = "DataResource", toValueField = "templateDataResource")
    @Action(type = ActionType.SET, field = "templateDataResourceTypeId", fromField = "templateDataResource.dataResourceTypeId")
    @Action(type = ActionType.SET, field = "mimeTypeId", fromField = "templateDataResource.mimeTypeId")
    @Action(type = ActionType.SET, field = "contentAssocTypeId", value = "COMPDOC_PART")
    @Action(type = ActionType.SET, field = "contentTypeId", value = "DOCUMENT")
    @Action(type = ActionType.SET, field = "sequenceNum", fromField = "parameters.caSequenceNum", defaultValue = "${parameters.sequenceNum}")
    @Action(type = ActionType.SET, field = "childCompDocTarget", value = "createChildCompDocInstance")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId")
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentIdTo}")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddChildCompDoc", location = "component://content/widget/compdoc/CompDocForms.xml"
            )})
        }
    )
    public interface AddChildCompDocInstance {}

    @Screen(name = "AddChildCompDocTemplate", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_CREATE"})}))
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleAddCompositeDocumentTemplate")
    @Action(type = ActionType.SET, field = "contentIdTo", fromField = "parameters.rootContentId", defaultValue = "${parameters.contentIdTo}")
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "contentIdTo")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "contentAssocTypeId", value = "COMPDOC_PART")
    @Action(type = ActionType.SET, field = "sequenceNum", fromField = "parameters.caSequenceNum", defaultValue = "${parameters.sequenceNum}")
    @Action(type = ActionType.SET, field = "contentTypeId", value = "TEMPLATE")
    @Action(type = ActionType.SET, field = "contentId")
    @Action(type = ActionType.SET, field = "instanceOfDataResourceTypeId")
    @Action(type = ActionType.SET, field = "childCompDocTarget", value = "createChildCompDocTemplate")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddChildCompDoc", location = "component://content/widget/compdoc/CompDocForms.xml"
            )})
        }
    )
    public interface AddChildCompDocTemplate {}

    @Screen(name = "ViewCompDocTemplateTree", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_CREATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "contentTypeId", value = "COMPDOC_TEMPLATE")
    @Action(type = ActionType.SERVICE, serviceName = "getMostRecentRevision", resultMapName = "revisionResult", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rootContentId")})
    @Action(type = ActionType.SET, field = "mostRecentRevisionSeqId", fromField = "revisionResult.mostRecentRevisionSeqId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${mostRecentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentTemplateRoot} ${rootContentId}, ${uiLabelMap.ContentCompDocRev} ${rootContentRevisionSeqId}")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "viewtree")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_TREE, name = "CompDocTemplateTree", location = "component://content/widget/compdoc/CompDocTemplateTree.xml"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocViewInstances}", style = "${styles.link_nav} ${styles.action_view}", target = "ViewInstances"
                ),
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleCreateInstanceOfThisTemplate}", style = "${styles.link_nav} ${styles.action_add}", target = "AddRootCompDocInstance"
            )}, position = 0)})
        }
    )
    public interface ViewCompDocTemplateTree {}

    @Screen(name = "ViewCompDocInstanceTree", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_CREATE"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "viewtree2")
    @Action(type = ActionType.SET, field = "rootContentId", fromField = "parameters.rootContentId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId")
    @Action(type = ActionType.SET, field = "contentTypeId", value = "COMPDOC_INSTANCE")
    @Action(type = ActionType.SERVICE, serviceName = "getMostRecentRevision", resultMapName = "revisionResult", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "parameters.rootContentId")})
    @Action(type = ActionType.SET, field = "mostRecentRevisionSeqId", fromField = "revisionResult.mostRecentRevisionSeqId")
    @Action(type = ActionType.SET, field = "rootContentRevisionSeqId", fromField = "parameters.rootContentRevisionSeqId", defaultValue = "${mostRecentRevisionSeqId}")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "instanceContent", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rootContentId")})
    @Action(type = ActionType.SET, field = "templateContentId", fromField = "instanceContent.instanceOfContentId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentRevision", list = "contentTemplateRevisions", useCache = true, conditions = {@ConditionExpr(fieldName = "contentId", operator = "equals", fromField = "templateContentId")}, orderBy = {"-contentRevisionSeqId"})
    @Action(type = ActionType.SET, field = "templateContentRevisionSeqId", fromField = "contentTemplateRevisions[0].contentRevisionSeqId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ContentRoot} ${rootContentId}, ${uiLabelMap.ContentCompDocRev} ${rootContentRevisionSeqId}  ${uiLabelMap.FormFieldTitle_instanceOfContentId} ${instanceContent.instanceOfContentId}, ${uiLabelMap.ContentCompDocRev} ${templateContentRevisionSeqId}")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_TREE, name = "CompDocInstanceTree", location = "component://content/widget/compdoc/CompDocTemplateTree.xml"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocGeneratePDF}", style = "${styles.link_run_sys} ${styles.action_export}", target = "GenCompDocPdf"
                )}, position = 0)})
        }
    )
    public interface ViewCompDocInstanceTree {}

    @Screen(name = "rootTemplateLine", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "rootTemplateLine", location = "component://content/widget/compdoc/CompDocMenus.xml")}))
    public interface rootTemplateLine {}

    @Screen(name = "rootInstanceLine", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "rootInstanceLine", location = "component://content/widget/compdoc/CompDocMenus.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "rootInstanceApprovalStatus")}))
    public interface rootInstanceLine {}

    @Screen(name = "rootInstanceApprovalStatus", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "getFinalApprovalStatus", resultMapName = "approvalStatusResult", fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "rootContentId"), @FieldMap(fieldName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "StatusItem", valueField = "statusItem", fieldMaps = {@FieldMap(fieldName = "statusId", fromField = "approvalStatusResult.approvalStatusId")})
    @Section(widgets = @Widgets(containers = {@Container(labels = {@Label(text = "${uiLabelMap.ContentCompDocApprovalStatus}: ${statusItem.description}", style = "heading")}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"}), @Condition(type = Compare.class, params = {"approvalStatusResult.approvalStatusId", "equals", "CNTAP_NOT_READY"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocStartApprovalProcess}", style = "${styles.link_run_sys} ${styles.action_begin}", target = "prepForApproval")}))})}))
    public interface rootInstanceApprovalStatus {}

    @Screen(name = "childTemplateLine", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/content/PrepSeqNo.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "childTemplateLine", location = "component://content/widget/compdoc/CompDocMenus.xml")}))
    public interface childTemplateLine {}

    @Screen(name = "childInstanceLine", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://content/webapp/content/WEB-INF/actions/content/PrepSeqNo.groovy")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "ContentAssocRevisionItemView", list = "assocRevisionItemViewList", conditions = {@ConditionExpr(fieldName = "contentIdTo", operator = "equals", fromField = "rootContentId"), @ConditionExpr(fieldName = "rootRevisionContentId", operator = "equals", fromField = "rootContentId"), @ConditionExpr(fieldName = "instanceOfContentId", operator = "equals", fromField = "contentId"), @ConditionExpr(fieldName = "contentAssocTypeId", operator = "equals", value = "COMPDOC_PART"), @ConditionExpr(fieldName = "contentRevisionSeqId", operator = "less-equals", fromField = "rootContentRevisionSeqId"), @ConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")}, orderBy = {"-maxRevisionSeqId"}, selectFields = {"rootRevisionContentId", "itemContentId", "maxRevisionSeqId", "contentId", "contentIdTo", "contentAssocTypeId", "fromDate", "sequenceNum"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.ITERATE_SECTION, list = "assocRevisionItemViewList", entry = "assocRevisionItemView", name = "childInstanceLine-iterate1", location = "component://content/widget/compdoc/CompDocScreens.xml", position = 1)}, containers = {@Container(labels = {@Label(text = "${contentName}", style = "tableheadtext")}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleAddCompDocInstance}", style = "${styles.link_nav} ${styles.action_add}", target = "AddChildCompDocInstance")}))}, position = 0)}))
    public interface childInstanceLine {}

    @Screen(name = "childInstanceLine-iterate1", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "instanceContent", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "contentId", fromField = "assocRevisionItemView.itemContentId")})
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "instanceDataResource", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "instanceContent.dataResourceId")})
    @Section(widgets = @Widgets(containers = {@Container(labels = {@Label(text = "- ${instanceContent.contentName} [${instanceContent.contentId}] - ${instanceDataResource.objectInfo} ${instanceDataResource.relatedDetailId}", style = "tableheadtext", position = 0)}, widgets = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleEditCompDocInstance}", style = "${styles.link_nav} ${styles.action_update}", target = "EditChildCompDoc", position = 1), @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ContentCompDocGeneratePDF}", style = "${styles.link_run_sys} ${styles.action_export}", target = "GenContentPdf", position = 2)})}))
    public interface childInstanceLine_iterate1 {}

    @Screen(name = "EditContentRevisionAndItem", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "EditContentRevisionAndItem", location = "component://content/widget/compdoc/CompDocForms.xml"
            )})
        }
    )
    public interface EditContentRevisionAndItem {}

    @Screen(name = "EditCompDocContentRole", location = "component://content/widget/compdoc/CompDocScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"CONTENTMGR", "_UPDATE"})}))
    @Action(type = ActionType.SET, field = "menuName", value = "tree")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ContentCompDocContentRoleEditPage")
    @Action(type = ActionType.SET, field = "currentMenuItemName", value = "role")
    @Action(type = ActionType.SET, field = "defaultContentId", fromField = "contentId", fromScope = "user")
    @Action(type = ActionType.SET, field = "contentId", fromField = "parameters.contentId", defaultValue = "${defaultContentId}")
    @Action(type = ActionType.SET, field = "contentId", fromField = "contentId")
    @Action(type = ActionType.SET, field = "contentRoleTarget", value = "CompDoc")
    @DecoratorScreen(
        name = "CommonCompDocDecorator",
        location = "component://content/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListContentRole", location = "component://content/widget/content/ContentForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "AddContentRole", location = "component://content/widget/content/ContentForms.xml"
            )})
        }
    )
    public interface EditCompDocContentRole {}

    @Screen(name = "ViewCompDocContent", location = "component://content/widget/compdoc/CompDocScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Content", valueField = "content", useCache = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "DataResource", valueField = "dataResource", useCache = true, fieldMaps = {@FieldMap(fieldName = "dataResourceId", fromField = "content.dataResourceId")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "dataResource.dataResourceTypeId", operator = "equals", value = "SURVEY"), @IfCompare(field = "dataResource.dataResourceTypeId", operator = "equals", value = "SURVEY_RESPONSE")})}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = Compare.class, params = {"dataResource.dataResourceTypeId", "equals", "SURVEY"}), @ConditionNode(not = true, type = Compare.class, params = {"dataResource.dataResourceTypeId", "equals", "SURVEY_RESPONSE"})})}))
    public interface ViewCompDocContent {}

}
