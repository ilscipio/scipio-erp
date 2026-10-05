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
public class FacilityFacilityMenus {

    @Menu(
        name = "FacilityAppBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        title = "${uiLabelMap.ProductFacilityManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "facility", title = "${uiLabelMap.ProductFacilities}", link = @MenuLink(target = "FindFacility", parameters = {@MenuParameter(paramName = "noConditionFind", value = "Y")})),
            @MenuItem(name = "ShipmentGatewayConfig", title = "${uiLabelMap.FacilityShipmentGatewayConfig}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"FACILITY", "_ADMIN"})}), link = @MenuLink(target = "FindShipmentGatewayConfig")),
            @MenuItem(name = "shipment", title = "${uiLabelMap.ProductShipments}", link = @MenuLink(target = "FindShipment")),
            @MenuItem(name = "reports", title = "${uiLabelMap.CommonReports}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"facilityId"})}), link = @MenuLink(target = "InventoryReports", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface FacilityAppBar {}

    @Menu(
        name = "FacilityAppSideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        title = "${uiLabelMap.ProductFacilityManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "FacilityAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "facility", subMenus = {@SubMenu(name = "Facility", include = "component://product/widget/facility/FacilityMenus.xml#FacilitySideBar")}),
            @MenuItem(name = "ShipmentGatewayConfig", subMenus = {@SubMenu(name = "ShipmentGatewayConfig", include = "component://product/widget/facility/FacilityMenus.xml#ShipmentGatewayConfigSideBar")}),
            @MenuItem(name = "shipment", subMenus = {@SubMenu(name = "Shipment", include = "component://product/widget/facility/FacilityMenus.xml#ShipmentSideBar")})
        }
    )
    public interface FacilityAppSideBar {}

    @Menu(
        name = "FacilityTabBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditFacility",
        items = {
            @MenuItem(name = "EditFacility", title = "${uiLabelMap.ProductFacility}", widgetStyle = "+${styles.menu_subappitem}", sortMode = "off", link = @MenuLink(target = "EditFacility", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "settings", title = "${uiLabelMap.CommonSettings}", link = @MenuLink(target = "settings", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "inventory", title = "${uiLabelMap.ProductInventory}", link = @MenuLink(target = "EditFacilityInventoryItems", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "Scheduling", title = "${uiLabelMap.ProductScheduling}", link = @MenuLink(target = "Scheduling", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "PicklistOptions", title = "${uiLabelMap.ProductPicking}", link = @MenuLink(target = "PicklistOptions", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "PackOrder", title = "${uiLabelMap.ProductPacking}", link = @MenuLink(target = "PackOrder", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface FacilityTabBar {}

    @Menu(
        name = "FacilitySideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "FacilityTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EditFacility",
        items = {
            @MenuItem(name = "settings", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"facilityId"})}), subMenus = {@SubMenu(name = "FacilitySettings", include = "component://product/widget/facility/FacilityMenus.xml#FacilitySettingsSideBar")}),
            @MenuItem(name = "inventory", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"facilityId"})}), subMenus = {@SubMenu(name = "FacilityInventory", include = "component://product/widget/facility/FacilityMenus.xml#FacilityInventorySideBar")}),
            @MenuItem(name = "PicklistOptions", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"facilityId"})}), subMenus = {@SubMenu(name = "FacilityPicking", include = "component://product/widget/facility/FacilityMenus.xml#FacilityPickingSideBar")}),
            @MenuItem(name = "PackOrder", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"facilityId"})}), subMenus = {@SubMenu(name = "FacilityPackOrder", include = "component://product/widget/facility/FacilityMenus.xml#FacilityPackOrderSideBar")})
        }
    )
    public interface FacilitySideBar {}

    @Menu(
        name = "InventoryItemTabBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditInventoryItem",
        selectedMenuItemContextFieldName = "activeSubMenu2Item",
        items = {
            @MenuItem(name = "EditInventoryItem", title = "${uiLabelMap.ProductInventoryItem}", link = @MenuLink(target = "EditInventoryItem", parameters = {@MenuParameter(paramName = "inventoryItemId", fromField = "inventoryItemId"), @MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "ViewInventoryItemDetail", title = "${uiLabelMap.ProductInventoryDetails}", link = @MenuLink(target = "ViewInventoryItemDetail", parameters = {@MenuParameter(paramName = "inventoryItemId", fromField = "inventoryItemId"), @MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "EditInventoryItemLabels", title = "${uiLabelMap.ProductInventoryItemLabelAppl}", link = @MenuLink(target = "EditInventoryItemLabels", parameters = {@MenuParameter(paramName = "inventoryItemId", fromField = "inventoryItemId"), @MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface InventoryItemTabBar {}

    @Menu(
        name = "ShipmentTabBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditShipment",
        selectedMenuItemContextFieldName = "activeSubMenu2Item",
        items = {
            @MenuItem(name = "EditShipment", title = "${uiLabelMap.ProductNewShipment}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditShipment"))
        }
    )
    public interface ShipmentTabBar {}

    @Menu(
        name = "ShipmentSideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "facility", title = "${uiLabelMap.ProductFacilities}", condition = @MenuItemCondition(conditions = {@Condition(type = Empty.class, params = {"shipment"})}), link = @MenuLink(target = "FindFacility", parameters = {@MenuParameter(paramName = "noConditionFind", value = "Y")})),
            @MenuItem(name = "EditShipment", title = "${uiLabelMap.FacilityShipment}", sortMode = "off", link = @MenuLink(target = "EditShipment", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")})),
            @MenuItem(name = "AddItemsFromInventory", title = "${uiLabelMap.ProductOrderItems}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"shipment"}), @Condition(type = Compare.class, params = {"shipment.shipmentTypeId", "equals", "PURCHASE_RETURN"})}), link = @MenuLink(target = "AddItemsFromInventory", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")})),
            @MenuItem(name = "AddItemsFromOrder", title = "${uiLabelMap.ProductOrderItems}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"shipment"}), @Condition(type = Compare.class, params = {"shipment.shipmentTypeId", "equals", "SALES_SHIPMENT"})}), link = @MenuLink(target = "AddItemsFromOrder", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")})),
            @MenuItem(name = "EditShipmentPlan", title = "${uiLabelMap.ProductShipmentPlan}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "shipment.shipmentTypeId", operator = "equals", value = "PURCHASE_SHIPMENT"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "shipment.shipmentTypeId", operator = "equals", value = "SALES_SHIPMENT")})}, conditions = {@Condition(type = NotEmpty.class, params = {"shipment"})}), link = @MenuLink(target = "EditShipmentPlan", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")})),
            @MenuItem(name = "EditShipmentItems", title = "${uiLabelMap.ProductItems}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "shipment.shipmentTypeId", operator = "equals", value = "SALES_SHIPMENT")})}, conditions = {@Condition(type = NotEmpty.class, params = {"shipment"})}), link = @MenuLink(target = "EditShipmentItems", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")})),
            @MenuItem(name = "EditShipmentPackages", title = "${uiLabelMap.ProductPackages}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "shipment.shipmentTypeId", operator = "equals", value = "SALES_SHIPMENT")})}, conditions = {@Condition(type = NotEmpty.class, params = {"shipment"})}), link = @MenuLink(target = "EditShipmentPackages", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")})),
            @MenuItem(name = "EditShipmentRouteSegments", title = "${uiLabelMap.ProductRouteSegments}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "shipment.shipmentTypeId", operator = "equals", value = "SALES_SHIPMENT")})}, conditions = {@Condition(type = NotEmpty.class, params = {"shipment"})}), link = @MenuLink(target = "EditShipmentRouteSegments", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")})),
            @MenuItem(name = "ViewShipmentReceipts", title = "${uiLabelMap.ProductRouteSegments}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "shipment.shipmentTypeId", operator = "equals", value = "PURCHASE_SHIPMENT")})}, conditions = {@Condition(type = NotEmpty.class, params = {"shipment"})}), link = @MenuLink(target = "ViewShipmentReceipts", parameters = {@MenuParameter(paramName = "shipmentId", fromField = "shipmentId")}))
        }
    )
    public interface ShipmentSideBar {}

    @Menu(
        name = "ShipmentGatewayConfigTabBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "shipmentGatewayConfigTypesTab", title = "${uiLabelMap.FacilityShipmentGatewayConfigTypes}", link = @MenuLink(target = "FindShipmentGatewayConfigTypes"))
        }
    )
    public interface ShipmentGatewayConfigTabBar {}

    @Menu(
        name = "ShipmentGatewayConfigSideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ShipmentGatewayConfigTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ShipmentGatewayConfigSideBar {}

    @Menu(
        name = "ViewFacilityInventoryByProductTabBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "activeSubMenuItem2",
        items = {
            @MenuItem(name = "ViewFacilityInventoryByProductReportTab", title = "${uiLabelMap.CommonPrint}", link = @MenuLink(target = "ViewFacilityInventoryByProductReport?${searchParameterString}")),
            @MenuItem(name = "ViewFacilityInventoryByProductExportTab", title = "${uiLabelMap.CommonExport}", link = @MenuLink(target = "ViewFacilityInventoryByProductExport?${searchParameterString}")),
            @MenuItem(name = "InventoryItemTotalsTab", title = "${uiLabelMap.ProductInventoryItemTotals}", link = @MenuLink(target = "InventoryItemTotals", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId"), @MenuParameter(paramName = "action", value = "Y")})),
            @MenuItem(name = "InventoryItemGrandTotalsTab", title = "${uiLabelMap.ProductInventoryItemGrandTotals}", link = @MenuLink(target = "InventoryItemGrandTotals", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId"), @MenuParameter(paramName = "action", value = "Y")})),
            @MenuItem(name = "InventoryItemTotalsExportTab", title = "${uiLabelMap.ProductInventoryItemTotalsExport}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", link = @MenuLink(target = "InventoryItemTotalsExport.csv", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId"), @MenuParameter(paramName = "action", value = "Y")})),
            @MenuItem(name = "InventoryAverageCostsTab", title = "${uiLabelMap.ProductInventoryAverageCosts}", link = @MenuLink(target = "InventoryAverageCosts", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface ViewFacilityInventoryByProductTabBar {}

    @Menu(
        name = "FacilitySettingsSideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ViewContactMechs", title = "${uiLabelMap.PartyContactMechs}", link = @MenuLink(target = "ViewContactMechs", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "FindFacilityLocation", title = "${uiLabelMap.ProductLocations}", link = @MenuLink(target = "FindFacilityLocation", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "EditFacilityGeoPoint", title = "${uiLabelMap.CommonGeoLocation}", link = @MenuLink(target = "EditFacilityGeoPoint", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface FacilitySettingsSideBar {}

    @Menu(
        name = "FacilityInventorySideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditFacilityInventoryItems", title = "${uiLabelMap.ProductInventoryItems}", link = @MenuLink(target = "EditFacilityInventoryItems", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "ReceiveInventory", title = "${uiLabelMap.ProductInventoryReceive}", link = @MenuLink(target = "ReceiveInventory", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "PhysicalInventory", title = "${uiLabelMap.ProductPhysicalInventory}", link = @MenuLink(target = "FindFacilityPhysicalInventory", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "FindFacilityTransfers", title = "${uiLabelMap.ProductInventoryXfers}", link = @MenuLink(target = "FindFacilityTransfers", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface FacilityInventorySideBar {}

    @Menu(
        name = "FacilityPackOrderSideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.CommonDashboard}", widgetStyle = "+${styles.menu_sidebar_itemdashboard}", overrideMode = "remove-replace", sortMode = "off", link = @MenuLink(target = "EditFacility", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "PackOrder", title = "${uiLabelMap.ProductPacking}", link = @MenuLink(target = "PackOrder", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface FacilityPackOrderSideBar {}

    @Menu(
        name = "FacilityPickingSideBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "PickMoveStock", title = "${uiLabelMap.ProductStockMoves}", link = @MenuLink(target = "PickMoveStock", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "PicklistManage", title = "${uiLabelMap.ProductPicklistManage}", link = @MenuLink(target = "PicklistManage", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")})),
            @MenuItem(name = "VerifyPick", title = "${uiLabelMap.ProductVerifyPick}", link = @MenuLink(target = "VerifyPick", parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface FacilityPickingSideBar {}

    @Menu(
        name = "FacilitySubTabBar",
        location = "component://product/widget/facility/FacilityMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditFacility", title = "${uiLabelMap.ProductNewFacility}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditFacility")),
            @MenuItem(name = "ViewCalendar", title = "${uiLabelMap.CommonViewCalendar}", widgetStyle = "+${styles.action_nav} ${styles.action_view}", link = @MenuLink(target = "/workeffort/control/calendar", urlMode = UrlMode.INTER_APP, parameters = {@MenuParameter(paramName = "facilityId", fromField = "facilityId")}))
        }
    )
    public interface FacilitySubTabBar {}

}
