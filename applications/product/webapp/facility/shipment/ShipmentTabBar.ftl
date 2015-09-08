<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#if requestAttributes.uiLabelMap??>
<#assign uiLabelMap = requestAttributes.uiLabelMap>
</#if>
<#assign selected = tabButtonItem?default("void")>
<#if shipmentId?has_content>
    <@menu type="tab">
            <@menuitem type="link" ofbizHref="ViewShipment?shipmentId=${shipmentId}" text="${uiLabelMap.CommonView}" selected=(selected=="ViewShipment") />
            <@menuitem type="link" ofbizHref="EditShipment?shipmentId=${shipmentId}" text="${uiLabelMap.CommonEdit}" selected=(selected=="EditShipment") />
        <#if (shipment.shipmentTypeId)?? && shipment.shipmentTypeId = "PURCHASE_RETURN">
            <@menuitem type="link" ofbizHref="AddItemsFromInventory?shipmentId=${shipmentId}" text="${uiLabelMap.ProductOrderItems}" selected=(selected=="AddItemsFromInventory") />
        </#if>
        <#if (shipment.shipmentTypeId)?? && shipment.shipmentTypeId = "SALES_SHIPMENT">
            <@menuitem type="link" ofbizHref="EditShipmentPlan?shipmentId=${shipmentId}" text="${uiLabelMap.ProductShipmentPlan}" selected=(selected=="EditShipmentPlan") />
            <@menuitem type="link" ofbizHref="AddItemsFromOrder?shipmentId=${shipmentId}" text="${uiLabelMap.ProductOrderItems}" selected=(selected=="AddItemsFromOrder") />
            <@menuitem type="link" ofbizHref="EditShipmentItems?shipmentId=${shipmentId}" text="${uiLabelMap.ProductItems}" selected=(selected=="EditShipmentItems") />
            <@menuitem type="link" ofbizHref="EditShipmentPackages?shipmentId=${shipmentId}" text="${uiLabelMap.ProductPackages}" selected=(selected=="EditShipmentPackages") />
            <@menuitem type="link" ofbizHref="EditShipmentRouteSegments?shipmentId=${shipmentId}" text="${uiLabelMap.ProductRouteSegments}" selected=(selected=="EditShipmentRouteSegments") />
        </#if>
        <#if (shipment.shipmentTypeId)?? && shipment.shipmentTypeId='PURCHASE_SHIPMENT'>
            <@menuitem type="link" ofbizHref="EditShipmentPlan?shipmentId=${shipmentId}" text="${uiLabelMap.ProductOrderItems}" selected=(selected=="EditShipmentPlan") />
            <@menuitem type="link" ofbizHref="ViewShipmentReceipts?shipmentId=${shipmentId}" text="${uiLabelMap.ProductShipmentReceipts}" selected=(selected=="ViewShipmentReceipts") />
        </#if>
    </@menu>
</#if>
