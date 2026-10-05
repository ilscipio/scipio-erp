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

import com.ilscipio.scipio.widget.def.menu.*;
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
public class CompdocCompDocMenus {

    @Menu(
        name = "empty",
        location = "component://content/widget/compdoc/CompDocMenus.xml",
        menuWidth = "100%",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentMenuItemName"
    )
    public interface empty {}

    @Menu(
        name = "tree",
        location = "component://content/widget/compdoc/CompDocMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        items = {
            @MenuItem(name = "viewtree", title = "${uiLabelMap.ContentCompDocViewTree}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "COMPDOC_TEMPLATE"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "TEMPLATE")})}), link = @MenuLink(target = "ViewCompDocTemplateTree", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "viewtree2", title = "${uiLabelMap.ContentCompDocViewTree}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "COMPDOC_INSTANCE"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "DOCUMENT")})}), link = @MenuLink(target = "ViewCompDocInstanceTree", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "edit", title = "${uiLabelMap.CommonEdit}", link = @MenuLink(target = "EditRootCompDoc", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "approval", title = "${uiLabelMap.ContentCompDocApprovals}", link = @MenuLink(target = "ListContentApproval", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "revision", title = "${uiLabelMap.ContentCompDocRevisions}", link = @MenuLink(target = "ListContentRevisions", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")}))
        }
    )
    public interface tree {}

    @Menu(
        name = "subtree",
        location = "component://content/widget/compdoc/CompDocMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        items = {
            @MenuItem(name = "viewtree", title = "${uiLabelMap.PageTitleViewCompDocTree}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "COMPDOC_TEMPLATE"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "TEMPLATE")})}), link = @MenuLink(target = "ViewCompDocTemplateTree", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "viewtree2", title = "${uiLabelMap.PageTitleViewCompDocTree}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "COMPDOC_INSTANCE"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "contentTypeId", operator = "equals", value = "DOCUMENT")})}), link = @MenuLink(target = "ViewCompDocInstanceTree", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "edit", title = "${uiLabelMap.CommonEdit}", link = @MenuLink(target = "EditChildCompDoc", parameters = {@MenuParameter(paramName = "contentId", fromField = "contentId"), @MenuParameter(paramName = "itemContentRevisionSeqId", fromField = "itemContentRevisionSeqId"), @MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId"), @MenuParameter(paramName = "caFromDate", fromField = "parameters.caFromDate")}))
        }
    )
    public interface subtree {}

    @Menu(
        name = "rootTemplateLine",
        location = "component://content/widget/compdoc/CompDocMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt1}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        items = {
            @MenuItem(name = "edit-inplace", title = "${contentName}[${contentId}]", widgetStyle = "+h2", link = @MenuLink(target = "EditRootCompDoc", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "edit-link", title = "${uiLabelMap.CommonEdit}", link = @MenuLink(target = "EditRootCompDoc", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "child", title = "${uiLabelMap.ContentCompDocChild}", condition = @MenuItemCondition(conditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"})}), link = @MenuLink(target = "AddChildCompDocTemplate", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "contentId"), @MenuParameter(paramName = "sequenceNum", value = "9999")})),
            @MenuItem(name = "latest", title = "${uiLabelMap.ContentCompDocCurrentTemplate}", condition = @MenuItemCondition(conditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "greater", "rootContentRevisionSeqId"})}), link = @MenuLink(target = "ViewCompDocTemplateTree", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "contentId")}))
        }
    )
    public interface rootTemplateLine {}

    @Menu(
        name = "childTemplateLine",
        location = "component://content/widget/compdoc/CompDocMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt1}",
        extraIndex = "${contentId}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        items = {
            @MenuItem(name = "edit-inplace", title = "${contentName} [${contentId}]", link = @MenuLink(target = "EditChildCompDoc", parameters = {@MenuParameter(paramName = "contentId", fromField = "contentId"), @MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "caFromDate", fromField = "fromDate"), @MenuParameter(paramName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "edit-link", title = "${uiLabelMap.CommonEdit}", link = @MenuLink(target = "EditChildCompDoc", parameters = {@MenuParameter(paramName = "contentId", fromField = "contentId"), @MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "caFromDate", fromField = "fromDate"), @MenuParameter(paramName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "bef", title = "${uiLabelMap.ContentCompDocBefore}", condition = @MenuItemCondition(conditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"})}), link = @MenuLink(target = "AddChildCompDocTemplate", parameters = {@MenuParameter(paramName = "contentId", fromField = "contentId"), @MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "caContentAssocTypeId", fromField = "contentAssocTypeId"), @MenuParameter(paramName = "caSequenceNum", fromField = "seqNumBefore"), @MenuParameter(paramName = "caFromDate", fromField = "fromDate")})),
            @MenuItem(name = "aft", title = "${uiLabelMap.ContentCompDocAfter}", condition = @MenuItemCondition(conditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"})}), link = @MenuLink(target = "AddChildCompDocTemplate", parameters = {@MenuParameter(paramName = "contentId", fromField = "contentId"), @MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "caContentAssocTypeId", fromField = "contentAssocTypeId"), @MenuParameter(paramName = "caSequenceNum", fromField = "seqNumAfter"), @MenuParameter(paramName = "caFromDate", fromField = "fromDate")})),
            @MenuItem(name = "up", title = "${uiLabelMap.ContentCompDocUp}", condition = @MenuItemCondition(conditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"})}), link = @MenuLink(target = "resequenceCompDocPart", parameters = {@MenuParameter(paramName = "contentId", fromField = "contentId"), @MenuParameter(paramName = "dir", value = "up"), @MenuParameter(paramName = "contentAssocTypeId", value = "COMPDOC_PART"), @MenuParameter(paramName = "contentIdTo", fromField = "contentIdTo"), @MenuParameter(paramName = "caContentAssocTypeId", fromField = "contentAssocTypeId"), @MenuParameter(paramName = "caSequenceNum", fromField = "seqNumBefore"), @MenuParameter(paramName = "caFromDate", fromField = "fromDate"), @MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "down", title = "${uiLabelMap.ContentCompDocDown}", condition = @MenuItemCondition(conditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "equals", "rootContentRevisionSeqId"})}), link = @MenuLink(target = "resequenceCompDocPart", parameters = {@MenuParameter(paramName = "contentId", fromField = "contentId"), @MenuParameter(paramName = "dir", value = "down"), @MenuParameter(paramName = "contentAssocTypeId", value = "COMPDOC_PART"), @MenuParameter(paramName = "contentIdTo", fromField = "contentIdTo"), @MenuParameter(paramName = "caContentAssocTypeId", fromField = "contentAssocTypeId"), @MenuParameter(paramName = "caSequenceNum", fromField = "seqNumBefore"), @MenuParameter(paramName = "caFromDate", fromField = "fromDate"), @MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "rootContentRevisionSeqId")}))
        }
    )
    public interface childTemplateLine {}

    @Menu(
        name = "rootInstanceLine",
        location = "component://content/widget/compdoc/CompDocMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt1}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        items = {
            @MenuItem(name = "edit-inplace", title = "${instanceContent.contentName}[${instanceContent.contentId}]", widgetStyle = "+h2", link = @MenuLink(target = "EditRootCompDoc", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "edit", title = "${uiLabelMap.CommonEdit}", link = @MenuLink(target = "EditRootCompDoc", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId"), @MenuParameter(paramName = "contentRevisionSeqId", fromField = "rootContentRevisionSeqId")})),
            @MenuItem(name = "viewtree", title = "${uiLabelMap.PageTitleViewCompDocTemplateTree}", link = @MenuLink(target = "ViewCompDocTemplateTree", parameters = {@MenuParameter(paramName = "rootContentRevisionSeqId", fromField = "templateContentRevisionSeqId"), @MenuParameter(paramName = "rootContentId", fromField = "templateContentId")})),
            @MenuItem(name = "latest", title = "${uiLabelMap.ContentCompDocCurrentInstance}", condition = @MenuItemCondition(conditions = {@Condition(type = CompareField.class, params = {"mostRecentRevisionSeqId", "greater", "rootContentRevisionSeqId"})}), link = @MenuLink(target = "ViewCompDocInstanceTree", parameters = {@MenuParameter(paramName = "rootContentId", fromField = "rootContentId")}))
        }
    )
    public interface rootInstanceLine {}

}
