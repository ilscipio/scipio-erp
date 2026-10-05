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
package com.ilscipio.scipio.manufacturing.widget;

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Fabrication order screens: FindFabricationOrders (find + list) and EditFabricationOrder (a combined
 * create-or-update header form, a totals panel, the runs table, add/create-run forms, and the bulk
 * status-change buttons).
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ManufacturingFabricationScreens {

    @Screen(name = "FindFabricationOrders", location = "component://manufacturing/widget/manufacturing/FabricationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingFindFabricationOrders")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "fabricationOrders")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ManufacturingFabricationOrderHelp}"),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "findFabricationOrder", location = "component://manufacturing/widget/manufacturing/FabricationForms.xml"),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListFabricationOrders", location = "component://manufacturing/widget/manufacturing/FabricationForms.xml"
            )})
        }
    )
    public interface FindFabricationOrders {}

    @Screen(name = "EditFabricationOrder", location = "component://manufacturing/widget/manufacturing/FabricationScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingEditFabricationOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "fabricationOrders")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/fabrication/FabricationOrder.groovy")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingFabricationOrder}", labels = {
                    @Label(text = "${uiLabelMap.ManufacturingFabricationOrderHelp}")
                }, includeForms = {
                    @IncludeForm(name = "UpdateFabricationOrder", location = "component://manufacturing/widget/manufacturing/FabricationForms.xml"
                )})
            }, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"fabOrder"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.ManufacturingFabricationOrderTotals}", htmlTemplates = {
                        @HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/fabrication/FabricationOrder.ftl"
                    )}, containers = {
                        @Container(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingScheduleAll}", style = "${styles.link_run_sys} ${styles.action_update}", target = "changeFabricationOrderStatus?fabricationOrderId=${fabricationOrderId}&statusId=PRUN_SCHEDULED"
                        ), @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingPrintAll}", style = "${styles.link_run_sys} ${styles.action_export}", target = "changeFabricationOrderStatus?fabricationOrderId=${fabricationOrderId}&statusId=PRUN_DOC_PRINTED"
                        ), @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingStartAll}", style = "${styles.link_run_sys} ${styles.action_begin}", target = "changeFabricationOrderStatus?fabricationOrderId=${fabricationOrderId}&statusId=PRUN_RUNNING"
                        ), @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingCompleteAll}", style = "${styles.link_run_sys} ${styles.action_complete}", target = "changeFabricationOrderStatus?fabricationOrderId=${fabricationOrderId}&statusId=PRUN_COMPLETED"
                        )})}),
                    @Screenlet(title = "${uiLabelMap.ManufacturingFabricationOrderRuns}", includeForms = {
                        @IncludeForm(name = "ListFabricationOrderRuns", location = "component://manufacturing/widget/manufacturing/FabricationForms.xml"
                    )}),
                    @Screenlet(title = "${uiLabelMap.ManufacturingAddExistingRun}", includeForms = {
                        @IncludeForm(name = "AddProductionRunToFabricationOrder", location = "component://manufacturing/widget/manufacturing/FabricationForms.xml"
                    )}),
                    @Screenlet(title = "${uiLabelMap.ManufacturingCreateRunInOrder}", includeForms = {
                        @IncludeForm(name = "CreateProductionRunInFabricationOrder", location = "component://manufacturing/widget/manufacturing/FabricationForms.xml"
                    )})
                }))
            })
        }
    )
    public interface EditFabricationOrder {}

}
