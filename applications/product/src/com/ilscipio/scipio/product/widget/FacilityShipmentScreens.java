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
public class FacilityShipmentScreens {

    @Screen(name = "FindShipment", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductFindShipment")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindShipment")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/FindShipment.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "ShipmentTabBar", location = "component://product/widget/facility/FacilityMenus.xml"
            )}, decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindShipment", location = "component://product/widget/facility/ShipmentForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListShipment", location = "component://product/widget/facility/ShipmentForms.xml"
                    )}))})})
        }
    )
    public interface FindShipment {}

    @Screen(name = "CommonShipmentDecorator", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://product/widget/facility/FacilityMenus.xml#Shipment")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Shipment", valueField = "shipment")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "shipment.shipmentId", global = true)
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "shipment", relationName = "DestinationFacility", toValueField = "facility")
    @DecoratorScreen(
        name = "CommonFacilityAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = NotEmpty.class, params = {"shipment"}),
                    @Condition(type = NotEmpty.class, params = {"facility"}),
                    @Condition(type = Compare.class, params = {"shipment.shipmentTypeId", "equals", "PURCHASE_SHIPMENT"
                })}), widgets = @InlineWidgets(containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductReceiveInventory}", style = "${styles.link_nav} ${styles.action_add}", target = "ReceiveInventory"
                    )})}), position = 0),
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"shipment"}),
                        @Condition(type = NotEmpty.class, params = {"facility"}),
                        @Condition(type = Compare.class, params = {"shipment.shipmentTypeId", "equals", "PURCHASE_SHIPMENT"
                    }),
                    @Condition(type = NotEmpty.class, params = {"shipment.primaryOrderId"
                })}), widgets = @InlineWidgets(containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductReceiveInventoryAgainstPO}", style = "${styles.link_nav} ${styles.action_add}", target = "ReceiveInventoryAgainstPurchaseOrder"
                    )})}), position = 1)})
        }
    )
    public interface CommonShipmentDecorator {}

    @Screen(name = "EditShipment", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "shipmentTypeId", fromField = "parameters.shipmentTypeId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.FacilityShipment} ${shipmentId}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipment.groovy")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.shipment ? 'EditShipment' : 'NewShipment'}")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/EditShipment.ftl"
            )})
        }
    )
    public interface EditShipment {}

    @Screen(name = "EditShipmentItems", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditShipmentItems")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.FacilityShipmentItems} ${shipmentId}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipmentItems.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/EditShipmentItems.ftl"
            )})
        }
    )
    public interface EditShipmentItems {}

    @Screen(name = "EditShipmentPlan", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditShipmentPlan")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleEditShipmentPlan} ${shipmentId}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipmentPlan.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ProductShipmentPlan}", includeForms = {
                    @IncludeForm(name = "findOrderItems", location = "component://product/widget/facility/ShipmentForms.xml"
                )}, position = 0),
                @Screenlet(title = "${uiLabelMap.ProductShipmentPlanList}", includeForms = {
                    @IncludeForm(name = "listShipmentPlan", location = "component://product/widget/facility/ShipmentForms.xml"
                )}, labels = {
                    @Label(text = "${uiLabelMap.ProductShipmentTotalWeight}: ${totWeight} ${uiLabelMap.ProductShipmentTotalVolume}: ${totVolume}", style = "heading"
                )}, position = 2)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"addToShipmentPlanRows"
                    })}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.ProductShipmentPlanAdd}", includeForms = {
                            @IncludeForm(name = "addToShipmentPlan", location = "component://product/widget/facility/ShipmentForms.xml"
                        )})}), position = 1),
                        @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                            @Condition(type = Compare.class, params = {"shipment.shipmentTypeId", "equals", "SALES_SHIPMENT"
                        })}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ProductShipmentPlanToOrderItems}", style = "${styles.link_nav} ${styles.action_add}", target = "AddItemsFromOrder"
                        )}), position = 3)})
        }
    )
    public interface EditShipmentPlan {}

    @Screen(name = "EditShipmentPackages", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditShipmentPackages")
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.FacilityShipmentPackages} ${shipmentId}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipmentPackages.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/EditShipmentPackages.ftl"
            )})
        }
    )
    public interface EditShipmentPackages {}

    @Screen(name = "EditShipmentRouteSegments", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditShipmentRouteSegments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditShipmentRouteSegments")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipmentRouteSegments.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/EditShipmentRouteSegments.ftl"
            )})
        }
    )
    public interface EditShipmentRouteSegments {}

    @Screen(name = "AddItemsFromOrder", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AddItemsFromOrder")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductAddItemsShipment} ${shipmentId}")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/AddItemsFromOrder.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/AddItemsFromOrder.ftl"
            )})
        }
    )
    public interface AddItemsFromOrder {}

    @Screen(name = "ViewShipmentReceipts", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewShipmentReceipts")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewShipmentReceipts")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ShipmentReceipts", location = "component://product/widget/facility/ShipmentForms.xml"
            )})
        }
    )
    public interface ViewShipmentReceipts {}

    @Screen(name = "QuickShipOrder", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductQuickShipOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "shipment")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/QuickShipOrder.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipmentItems.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipmentPackages.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/EditShipmentRouteSegments.groovy")
    @DecoratorScreen(
        name = "CommonFacilityAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/QuickShipOrder.ftl"
            )})
        }
    )
    public interface QuickShipOrder {}

    @Screen(name = "VerifyPick", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "facilityId", value = "${parameters.facilityId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "VerifyPick")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/VerifyPick.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductVerifyPick} ${facilityId}")
    @DecoratorScreen(
        name = "CommonFacilityPickingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/VerifyPick.ftl"
            )})
        }
    )
    public interface VerifyPick {}

    @Screen(name = "PackOrder", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ProductPackOrder} ${facilityId}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PackOrder")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/PackOrder.groovy")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/PackOrder.ftl"
            )})
        }
    )
    public interface PackOrder {}

    @Screen(name = "WeightPackageOnly", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductWeighPackageOnly")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PackOrder")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/WeightPackage.groovy")
    @DecoratorScreen(
        name = "CommonFacilityDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/WeightPackage.ftl"
            )})
        }
    )
    public interface WeightPackageOnly {}

    @Screen(name = "PackingSlip.fo", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/ViewShipment.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/PackingSlip.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            )}),
            @DecoratorSection(name = "topRight", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/PackingSlipShipmentBarCode.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/PackingSlip.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface PackingSlip_fo {}

    @Screen(name = "ShipmentBarCode.fo", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://product/webapp/facility/shipment/ShipmentBarCode.fo.ftl", platform = "xsl-fo")}))
    public interface ShipmentBarCode_fo {}

    @Screen(name = "ShipmentManifest.fo", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "shipmentId", fromField = "parameters.shipmentId")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/ViewShipment.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/ShipmentManifest.groovy")
    @DecoratorScreen(
        name = "FoReportDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "topLeft", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CompanyLogo", location = "component://order/widget/ordermgr/OrderPrintScreens.xml"
            )}),
            @DecoratorSection(name = "topRight", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/PackingSlipShipmentBarCode.fo.ftl", platform = "xsl-fo"
            )}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/ShipmentManifest.fo.ftl", platform = "xsl-fo"
            )})
        }
    )
    public interface ShipmentManifest_fo {}

    @Screen(name = "ReceiveInventoryAgainstPurchaseOrder", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductEntityLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductReceiveInventoryAgainstPurchaseOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ProductReceiveInventoryAgainstPO")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/ReceiveInventoryAgainstPurchaseOrder.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/ReceiveInventoryAgainstPurchaseOrder.ftl"
            )})
        }
    )
    public interface ReceiveInventoryAgainstPurchaseOrder {}

    @Screen(name = "AddItemsFromInventory", location = "component://product/widget/facility/ShipmentScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "ProductAddItemsFromInventory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AddItemsFromInventory")
    @Action(type = ActionType.SCRIPT, location = "component://product/webapp/facility/WEB-INF/actions/shipment/AddItemsFromInventory.groovy")
    @DecoratorScreen(
        name = "CommonShipmentDecorator",
        location = "component://product/widget/facility/ShipmentScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://product/webapp/facility/shipment/AddItemsFromInventory.ftl"
            )})
        }
    )
    public interface AddItemsFromInventory {}

}
