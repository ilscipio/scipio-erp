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
            <@menuitem type="link" href=makeOfbizUrl("ViewShipment?shipmentId=${shipmentId}") text="${uiLabelMap.CommonView}" selected=(selected=="ViewShipment") class="+${styles.action_nav!} ${styles.action_view!}"/>
            <@menuitem type="link" href=makeOfbizUrl("EditShipment?shipmentId=${shipmentId}") text="${uiLabelMap.CommonEdit}" selected=(selected=="EditShipment") class="+${styles.action_nav!} ${styles.action_update!}" />
        <#if (shipment.shipmentTypeId)?? && shipment.shipmentTypeId == "PURCHASE_RETURN">
            <@menuitem type="link" href=makeOfbizUrl("AddItemsFromInventory?shipmentId=${shipmentId}") text="${uiLabelMap.ProductOrderItems}" selected=(selected=="AddItemsFromInventory") class="+${styles.action_nav!} ${styles.action_update!}" />
        </#if>
        <#if (shipment.shipmentTypeId)?? && shipment.shipmentTypeId == "SALES_SHIPMENT">
            <@menuitem type="link" href=makeOfbizUrl("EditShipmentPlan?shipmentId=${shipmentId}") text="${uiLabelMap.ProductShipmentPlan}" selected=(selected=="EditShipmentPlan") class="+${styles.action_nav!} ${styles.action_update!}" />
            <@menuitem type="link" href=makeOfbizUrl("AddItemsFromOrder?shipmentId=${shipmentId}") text="${uiLabelMap.ProductOrderItems}" selected=(selected=="AddItemsFromOrder") class="+${styles.action_nav!} ${styles.action_update!}" />
            <@menuitem type="link" href=makeOfbizUrl("EditShipmentItems?shipmentId=${shipmentId}") text="${uiLabelMap.ProductItems}" selected=(selected=="EditShipmentItems") class="+${styles.action_nav!} ${styles.action_update!}" />
            <@menuitem type="link" href=makeOfbizUrl("EditShipmentPackages?shipmentId=${shipmentId}") text="${uiLabelMap.ProductPackages}" selected=(selected=="EditShipmentPackages") class="+${styles.action_nav!} ${styles.action_update!}" />
            <@menuitem type="link" href=makeOfbizUrl("EditShipmentRouteSegments?shipmentId=${shipmentId}") text="${uiLabelMap.ProductRouteSegments}" selected=(selected=="EditShipmentRouteSegments") class="+${styles.action_nav!} ${styles.action_update!}" />
        </#if>
        <#if (shipment.shipmentTypeId)?? && shipment.shipmentTypeId == "PURCHASE_SHIPMENT">
            <@menuitem type="link" href=makeOfbizUrl("EditShipmentPlan?shipmentId=${shipmentId}") text="${uiLabelMap.ProductOrderItems}" selected=(selected=="EditShipmentPlan") class="+${styles.action_nav!} ${styles.action_update!}" />
            <@menuitem type="link" href=makeOfbizUrl("ViewShipmentReceipts?shipmentId=${shipmentId}") text="${uiLabelMap.ProductShipmentReceipts}" selected=(selected=="ViewShipmentReceipts") class="+${styles.action_nav!} ${styles.action_view!}" />
        </#if>
    </@menu>
</#if>
