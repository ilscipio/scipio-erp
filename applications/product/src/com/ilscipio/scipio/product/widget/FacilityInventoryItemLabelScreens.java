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
public class FacilityInventoryItemLabelScreens {

    @Screen(name = "FindInventoryItemLabels", location = "component://product/widget/facility/InventoryItemLabelScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindInventoryItemLabels")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "inventoryItemLabel")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindInventoryItemLabels")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InventoryItemLabel", list = "inventoryItemLabels")
    @DecoratorScreen(
        name = "CommonInventoryItemLabelsDecorator",
        location = "${parameters.commonFacilityDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "button-bar", widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditInventoryItemLabel"
                )}, position = 0)}, screenlets = {
                    @Screenlet(title = "${uiLabelMap.PageTitleFindInventoryItemLabels}", includeForms = {
                        @IncludeForm(name = "ListInventoryItemLabels", location = "component://product/widget/facility/InventoryItemLabelForms.xml"
                    )}, position = 1)})
        }
    )
    public interface FindInventoryItemLabels {}

    @Screen(name = "EditInventoryItemLabelTypes", location = "component://product/widget/facility/InventoryItemLabelScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditInventoryItemLabelTypes")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "inventoryItemLabel")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditInventoryItemLabelTypes")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InventoryItemLabelType", list = "inventoryItemLabelTypes")
    @DecoratorScreen(
        name = "CommonInventoryItemLabelsDecorator",
        location = "${parameters.commonFacilityDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateInventoryItemLabelTypes", location = "component://product/widget/facility/InventoryItemLabelForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddInventoryItemLabelTypes}", name = "AddInventoryItemLabelTypePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddInventoryItemLabelType", location = "component://product/widget/facility/InventoryItemLabelForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditInventoryItemLabelTypes {}

    @Screen(name = "EditInventoryItemLabel", location = "component://product/widget/facility/InventoryItemLabelScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductInventoryItemLabel")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "inventoryItemLabel")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindInventoryItemLabels")
    @Action(type = ActionType.SET, field = "subTabButtonItem", value = "EditInventoryItemLabel")
    @Action(type = ActionType.ENTITY_ONE, entityName = "InventoryItemLabel", valueField = "inventoryItemLabel")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"inventoryItemLabel"})}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonInventoryItemLabelDecorator",
        location = "${parameters.commonFacilityDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditInventoryItemLabel", location = "component://product/widget/facility/InventoryItemLabelForms.xml"
                )})})
        }
    )), failWidgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonInventoryItemLabelsDecorator",
        location = "${parameters.commonFacilityDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.CommonNew}", style = "${styles.link_nav} ${styles.action_add}", target = "EditInventoryItemLabel"
                )}, position = 0)}, screenlets = {
                    @Screenlet(includeForms = {
                        @IncludeForm(name = "EditInventoryItemLabel", location = "component://product/widget/facility/InventoryItemLabelForms.xml"
                    )}, position = 1)})
        }
    )))
    public interface EditInventoryItemLabel {}

    @Screen(name = "EditInventoryItemLabelAppls", location = "component://product/widget/facility/InventoryItemLabelScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditInventoryItemLabelAppls")
    @Action(type = ActionType.SET, field = "activeMainMenuItem", value = "inventoryItemLabel")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindInventoryItemLabels")
    @Action(type = ActionType.SET, field = "subTabButtonItem", value = "EditInventoryItemLabelAppls")
    @Action(type = ActionType.ENTITY_ONE, entityName = "InventoryItemLabel", valueField = "inventoryItemLabel")
    @Action(type = ActionType.GET_RELATED, valueField = "inventoryItemLabel", relationName = "InventoryItemLabelAppl", list = "inventoryItemLabelAppls")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "inventoryItemLabel", relationName = "InventoryItemLabelType", toValueField = "inventoryItemLabelType")
    @DecoratorScreen(
        name = "CommonInventoryItemLabelDecorator",
        location = "${parameters.commonFacilityDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonType}: ${inventoryItemLabelType.description} [${inventoryItemLabelType.inventoryItemLabelTypeId}]", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "UpdateInventoryItemLabelAppls", location = "component://product/widget/facility/InventoryItemLabelForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.PageTitleAddInventoryItemLabelAppls}", name = "AddInventoryItemLabelApplPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddInventoryItemLabelAppl", location = "component://product/widget/facility/InventoryItemLabelForms.xml"
                )}, position = 1)})
        }
    )
    public interface EditInventoryItemLabelAppls {}

}
