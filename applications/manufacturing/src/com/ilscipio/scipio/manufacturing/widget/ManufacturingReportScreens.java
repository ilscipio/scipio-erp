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
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ManufacturingReportScreens {

    @Screen(name = "ManufacturingReports", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingReports")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ManufacturingReports")
    @Action(type = ActionType.SET, field = "mrpName", fromField = "parameters.mrpName")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "SelectMrpName", location = "component://manufacturing/widget/manufacturing/ProductionRunForms.xml"
                )}, htmlTemplates = {
                    @HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/jobshopmgt/MrpReports.ftl"
                )})})
        }
    )
    public interface ManufacturingReports {}

    @Screen(name = "CuttingListReport", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titlePropery", value = "ManufacturingCuttingListReport")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/widget/manufacturing/CuttingListReport.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/jobshopmgt/CuttingListReport.fo.ftl", platform = "xsl-fo")}))
    public interface CuttingListReport {}

    @Screen(name = "MRPPRunsProductsByFeature", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunComponentsByFeature")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "mrpName", fromField = "parameters.mrpName")
    @Action(type = ActionType.SET, field = "planName", value = "MRP_${mrpName}")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PRunsProductsByFeature.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/PRunsProductsByFeature.fo.ftl", platform = "xsl-fo")}))
    public interface MRPPRunsProductsByFeature {}

    @Screen(name = "SPPRunsProductsByFeature", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunComponentsByFeature")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "planName", value = "SP_${shipmentId}")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Shipment", valueField = "shipment")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PRunsProductsByFeature.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/PRunsProductsByFeature.fo.ftl", platform = "xsl-fo")}))
    public interface SPPRunsProductsByFeature {}

    @Screen(name = "MRPPRunsComponentsByFeature", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunComponentsByFeature")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "mrpName", fromField = "parameters.mrpName")
    @Action(type = ActionType.SET, field = "planName", value = "MRP_${mrpName}")
    @Action(type = ActionType.SET, field = "showLocation", fromField = "parameters.showLocation")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PRunsComponentsByFeature.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/PRunsComponentsByFeature.fo.ftl", platform = "xsl-fo")}))
    public interface MRPPRunsComponentsByFeature {}

    @Screen(name = "SPPRunsComponentsByFeature", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunComponentsByFeature")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "planName", value = "SP_${shipmentId}")
    @Action(type = ActionType.SET, field = "showLocation", fromField = "parameters.showLocation")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "Shipment", valueField = "shipment")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PRunsComponentsByFeature.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/reports/PRunsComponentsByFeature.fo.ftl")}))
    public interface SPPRunsComponentsByFeature {}

    @Screen(name = "PRunsProductsStacks", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductsStacks")
    @Action(type = ActionType.SET, field = "mrpName", fromField = "parameters.mrpName")
    @Action(type = ActionType.SET, field = "planName", value = "MRP_${mrpName}")
    @Action(type = ActionType.SET, field = "stackQty", value = "50", valueType = "Integer")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PRunsProductsStacks.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/PRunsProductsStacks.fo.ftl", platform = "xsl-fo")}))
    public interface PRunsProductsStacks {}

    @Screen(name = "PackageContentsAndOrder", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingPackageContentsAndOrder")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "planName", value = "SP_${shipmentId}")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PackageContentsAndOrder.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/reports/PackageContentsAndOrder.fo.ftl")}))
    public interface PackageContentsAndOrder {}

    @Screen(name = "PRunsProductsAndOrder", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingPackageContentsAndOrder")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "planName", value = "SP_${shipmentId}")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PRunsProductsAndOrder.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/PRunsProductsAndOrder.fo.ftl", platform = "xsl-fo")}))
    public interface PRunsProductsAndOrder {}

    @Screen(name = "PRunsInfoAndOrder", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingProductionRunInfoAndOrder")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "planName", value = "SP_${shipmentId}")
    @Action(type = ActionType.SET, field = "taskNamePar", fromField = "parameters.taskNamePar")
    @Action(type = ActionType.SET, field = "productCategoryIdPar", fromField = "parameters.productCategoryIdPar")
    @Action(type = ActionType.SET, field = "productFeatureTypeIdPar", fromField = "parameters.productFeatureTypeIdPar")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/PRunsInfoAndOrder.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/PRunsInfoAndOrder.fo.ftl", platform = "xsl-fo")}))
    public interface PRunsInfoAndOrder {}

    @Screen(name = "ShipmentPlanStockReport", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingShipmentPlanStockReport")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/ShipmentPlanStockReport.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/ShipmentPlanStockReport.fo.ftl", platform = "xsl-fo")}))
    public interface ShipmentPlanStockReport {}

    @Screen(name = "ShipmentLabel", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductShippingLabel")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/ShipmentLabel.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/ShipmentLabel.fo.ftl", platform = "xsl-fo")}))
    public interface ShipmentLabel {}

    @Screen(name = "ShipmentWorkEffortTasks", location = "component://manufacturing/widget/manufacturing/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingShipmentWorkEffortTasks")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingReportsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/reports/ShipmentWorkEffortTasks.groovy")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://manufacturing/webapp/manufacturing/reports/ShipmentWorkEffortTasks.fo.ftl", platform = "xsl-fo")}))
    public interface ShipmentWorkEffortTasks {}

}
