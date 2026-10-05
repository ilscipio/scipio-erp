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
 * Screen definitions for the manufacturing barcode/QR scan feature: the shop floor scan screen
 * and the production run label sheet (FOP PDF).
 *
 * <p>SCIPIO: 4.0.0: Added for the barcode capture feature.</p>
 */
public class ManufacturingBarcodeScreens {

    @Screen(name = "ScanTask", location = "component://manufacturing/widget/manufacturing/BarcodeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingScan")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ScanTask")
    @Action(type = ActionType.SET, field = "scanAction", fromField = "parameters.scanAction", defaultValue = "INFO")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/barcode/ScanTask.groovy")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ScanTaskForm", location = "component://manufacturing/widget/manufacturing/BarcodeForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Always.class
                    )}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/barcode/ScanTask.ftl"
                    )}))})
        }
    )
    public interface ScanTask {}

    @Screen(name = "ProductionRunLabels", location = "component://manufacturing/widget/manufacturing/BarcodeScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRun")
    @Action(type = ActionType.SET, field = "bodyFontSize", value = "10pt")
    @Action(type = ActionType.SET, field = "productionRunId", fromField = "parameters.productionRunId")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/barcode/ProductionRunLabels.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/barcode/ProductionRunLabels.fo.ftl")}))
    public interface ProductionRunLabels {}

}
