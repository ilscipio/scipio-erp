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
public class ContentDataResourceMenus {

    @Menu(
        name = "contentsetup",
        location = "component://content/widget/content/DataResourceMenus.xml",
        selectedMenuItemContextFieldName = "activeSubMenuItem",
        items = {
            @MenuItem(name = "adddataresource", title = "${uiLabelMap.CommonAdd}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "AddDataResource", targetWindow = "_top", name = "AddDataResource")),
            @MenuItem(name = "editdataresource", title = "${uiLabelMap.ContentDataResource}", condition = @MenuItemCondition(disabledStyle = "+disabled", not = true, conditions = {@Condition(type = Empty.class, params = {"dataResourceId"})}), link = @MenuLink(target = "EditDataResource", targetWindow = "_top", name = "EditDataResource")),
            @MenuItem(name = "editelectronictext", title = "${uiLabelMap.ContentDataResourceText}", condition = @MenuItemCondition(disabledStyle = "+disabled", not = true, conditions = {@Condition(type = Empty.class, params = {"dataResourceId"})}), link = @MenuLink(target = "EditElectronicText", targetWindow = "_top", name = "EditElectronicText")),
            @MenuItem(name = "edithtmltext", title = "${uiLabelMap.ContentHtml}", condition = @MenuItemCondition(disabledStyle = "+disabled", not = true, conditions = {@Condition(type = Empty.class, params = {"dataResourceId"})}), link = @MenuLink(target = "EditHtmlText", targetWindow = "_top", name = "EditHtmlText")),
            @MenuItem(name = "uploadimage", title = "${uiLabelMap.ContentImage}", condition = @MenuItemCondition(disabledStyle = "+disabled", not = true, conditions = {@Condition(type = Empty.class, params = {"dataResourceId"})}), link = @MenuLink(target = "UploadImage", targetWindow = "_top", name = "UploadImage")),
            @MenuItem(name = "editdataresourceattribute", title = "${uiLabelMap.ContentAttribute}", condition = @MenuItemCondition(disabledStyle = "+disabled", not = true, conditions = {@Condition(type = Empty.class, params = {"dataResourceId"})}), link = @MenuLink(target = "EditDataResourceAttribute", targetWindow = "_top", name = "EditDataResourceAttribute")),
            @MenuItem(name = "editdataresourcerole", title = "${uiLabelMap.ContentDataResourceRole}", condition = @MenuItemCondition(disabledStyle = "+disabled", not = true, conditions = {@Condition(type = Empty.class, params = {"dataResourceId"})}), link = @MenuLink(target = "EditDataResourceRole", targetWindow = "_top", name = "EditDataResourceRole")),
            @MenuItem(name = "editdataresourceproductfeatures", title = "${uiLabelMap.ContentDataResourceProductFeatures}", condition = @MenuItemCondition(disabledStyle = "+disabled", not = true, conditions = {@Condition(type = Empty.class, params = {"dataResourceId"})}), link = @MenuLink(target = "EditDataResourceProductFeatures", targetWindow = "_top", name = "EditDataResourceProductFeatures"))
        }
    )
    public interface contentsetup {}

    @Menu(
        name = "DataResourceButtonBar",
        location = "component://content/widget/content/DataResourceMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "findDataResource", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "findDataResource")),
            @MenuItem(name = "navigateDataResource", title = "${uiLabelMap.ContentNavigate}", link = @MenuLink(target = "navigateDataResource"))
        }
    )
    public interface DataResourceButtonBar {}

    @Menu(
        name = "DataResourceSideBar",
        location = "component://content/widget/content/DataResourceMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "DataResourceButtonBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface DataResourceSideBar {}

    @Menu(
        name = "dataresource",
        location = "component://content/widget/content/DataResourceMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "editDataResource", title = "${uiLabelMap.CommonEdit}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.dataResourceId"})}), link = @MenuLink(target = "EditDataResource", parameters = {@MenuParameter(paramName = "dataResourceId", fromField = "parameters.dataResourceId")})),
            @MenuItem(name = "editElectronicText", title = "${uiLabelMap.ContentDataResourceText}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"currentValue.dataResourceId"}), @Condition(type = Compare.class, params = {"currentValue.dataResourceTypeId", "equals", "ELECTRONIC_TEXT"})}), link = @MenuLink(target = "EditElectronicText", parameters = {@MenuParameter(paramName = "dataResourceId", fromField = "parameters.dataResourceId")})),
            @MenuItem(name = "editHtmlText", title = "${uiLabelMap.ContentDataResourceHtml}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"currentValue.dataResourceId"}), @Condition(type = Compare.class, params = {"currentValue.dataResourceTypeId", "equals", "ELECTRONIC_TEXT"})}), link = @MenuLink(target = "EditHtmlText", parameters = {@MenuParameter(paramName = "dataResourceId", fromField = "parameters.dataResourceId")})),
            @MenuItem(name = "uploadImage", title = "${uiLabelMap.ContentDataResourceUpload}", condition = @MenuItemCondition(conditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = Compare.class, params = {"currentValue.dataResourceTypeId", "equals", "IMAGE_OBJECT"}), @ConditionNode(type = And.class), @ConditionNode(parent = 1, type = Compare.class, params = {"currentValue.dataResourceTypeId", "contains", "FILE"})})}), link = @MenuLink(target = "UploadImage", parameters = {@MenuParameter(paramName = "dataResourceId", fromField = "parameters.dataResourceId")})),
            @MenuItem(name = "EditDataResourceAttribute", title = "${uiLabelMap.ContentDataResourceAttribute}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.dataResourceId"})}), link = @MenuLink(target = "EditDataResourceAttribute", parameters = {@MenuParameter(paramName = "dataResourceId", fromField = "parameters.dataResourceId")})),
            @MenuItem(name = "editDataResourceRole", title = "${uiLabelMap.ContentDataResourceRole}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.dataResourceId"})}), link = @MenuLink(target = "EditDataResourceRole", parameters = {@MenuParameter(paramName = "dataResourceId", fromField = "parameters.dataResourceId")})),
            @MenuItem(name = "editDataResourceProductFeatures", title = "${uiLabelMap.ContentDataResourceProductFeatures}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"currentValue.dataResourceId"})}), link = @MenuLink(target = "EditDataResourceProductFeatures", parameters = {@MenuParameter(paramName = "dataResourceId", fromField = "parameters.dataResourceId")}))
        }
    )
    public interface dataresource {}

}
