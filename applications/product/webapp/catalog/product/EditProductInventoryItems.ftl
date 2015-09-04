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
<#assign externalKeyParam = "&amp;externalLoginKey=" + requestAttributes.externalLoginKey!>
<#assign sectionTitle>${uiLabelMap.ProductInventoryItems} ${uiLabelMap.CommonFor} <#if product??>${(product.internalName)!} </#if> [${uiLabelMap.CommonId}:${productId!}]</#assign>
<#assign menuHtml>
  <@menu type="section" inlineItems=true>
        <#if productId?has_content>
            <@menuitem type="link" href="/facility/control/EditInventoryItem?productId=${productId}${StringUtil.wrapString(externalKeyParam)}" text="${uiLabelMap.ProductCreateNewInventoryItemProduct}" />
            <#if showEmpty>
                <@menuitem type="link" ofbizHref="EditProductInventoryItems?productId=${productId}" text="${uiLabelMap.ProductHideEmptyItems}" />
            <#else>
                <@menuitem type="link" ofbizHref="EditProductInventoryItems?productId=${productId}&amp;showEmpty=true" text="${uiLabelMap.ProductShowEmptyItems}" />
            </#if>
        </#if>
  </@menu>
</#assign>
<@section title=sectionTitle menuHtml=menuHtml>
  <#if product??>
        <#if productId??>
            <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
            <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductItemId}</@th>
                <@th>${uiLabelMap.ProductItemType}</@th>
                <@th>${uiLabelMap.CommonStatus}</@th>
                <@th>${uiLabelMap.CommonReceived}</@th>
                <@th>${uiLabelMap.CommonExpire}</@th>
                <@th>${uiLabelMap.ProductFacilityContainerId}</@th>
                <@th>${uiLabelMap.ProductLocation}</@th>
                <@th>${uiLabelMap.ProductLotId}</@th>
                <@th>${uiLabelMap.ProductBinNum}</@th>
                <@th align="right">${uiLabelMap.ProductPerUnitPrice}</@th>
                <@th>&nbsp;</@th>
                <@th align="right">${uiLabelMap.ProductInventoryItemInitialQuantity}</@th>
                <@th align="right">${uiLabelMap.ProductAtpQohSerial}</@th>
            </@tr>
            </@thead>
            <#list productInventoryItems as inventoryItem>
               <#-- NOTE: Delivered for serialized inventory means shipped to customer so they should not be displayed here any more -->
               <#if showEmpty || (inventoryItem.inventoryItemTypeId! == "SERIALIZED_INV_ITEM" && inventoryItem.statusId! != "INV_DELIVERED")
                              || (inventoryItem.inventoryItemTypeId! == "NON_SERIAL_INV_ITEM" && ((inventoryItem.availableToPromiseTotal?? && inventoryItem.availableToPromiseTotal != 0) || (inventoryItem.quantityOnHandTotal?? && inventoryItem.quantityOnHandTotal != 0)))>
                    <#assign curInventoryItemType = inventoryItem.getRelatedOne("InventoryItemType", false)>
                    <#assign curStatusItem = inventoryItem.getRelatedOne("StatusItem", true)!>
                    <#assign facilityLocation = inventoryItem.getRelatedOne("FacilityLocation", false)!>
                    <#assign facilityLocationTypeEnum = (facilityLocation.getRelatedOne("TypeEnumeration", true))!>
                    <#assign inventoryItemDetailFirst = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(inventoryItem.getRelated("InventoryItemDetail", null, Static["org.ofbiz.base.util.UtilMisc"].toList("effectiveDate"), false))!>
                    <#if curInventoryItemType??>
                        <@tr valign="middle">
                            <@td><a href="/facility/control/EditInventoryItem?inventoryItemId=${(inventoryItem.inventoryItemId)!}${StringUtil.wrapString(externalKeyParam)}" class="${styles.button_default!}">${(inventoryItem.inventoryItemId)!}</a></@td>
                            <@td>&nbsp;${(curInventoryItemType.get("description",locale))!}</@td>
                            <@td>
                                    <#if curStatusItem?has_content>
                                        ${(curStatusItem.get("description",locale))!}
                                    <#elseif inventoryItem.statusId?has_content>
                                        [${inventoryItem.statusId}]
                                    <#else>
                                        ${uiLabelMap.CommonNotSet}&nbsp;
                                    </#if>
                            </@td>
                            <@td>&nbsp;${(inventoryItem.datetimeReceived)!}</@td>
                            <@td>&nbsp;${(inventoryItem.expireDate)!}</@td>
                            <#if inventoryItem.facilityId?? && inventoryItem.containerId??>
                                <@td style="color: red;">${uiLabelMap.ProductErrorFacility} (${inventoryItem.facilityId})
                                    ${uiLabelMap.ProductAndContainer} (${inventoryItem.containerId}) ${uiLabelMap.CommonSpecified}</@td>
                            <#elseif inventoryItem.facilityId??>
                                <@td>${uiLabelMap.ProductFacilityLetter}:&nbsp;<a href="/facility/control/EditFacility?facilityId=${inventoryItem.facilityId}${StringUtil.wrapString(externalKeyParam)}" class="${styles.button_default!}">${inventoryItem.facilityId}</a></@td>
                            <#elseif (inventoryItem.containerId)??>
                                <@td>${uiLabelMap.ProductContainerLetter}:&nbsp;<a href="<@ofbizUrl>EditContainer?containerId=${inventoryItem.containerId }</@ofbizUrl>" class="${styles.button_default!}">${inventoryItem.containerId}</a></@td>
                            <#else>
                                <@td>&nbsp;</@td>
                            </#if>
                            <@td><a href="/facility/control/EditFacilityLocation?facilityId=${(inventoryItem.facilityId)!}&amp;locationSeqId=${(inventoryItem.locationSeqId)!}${StringUtil.wrapString(externalKeyParam)}"><#if facilityLocation??>${facilityLocation.areaId!}:${facilityLocation.aisleId!}:${facilityLocation.sectionId!}:${facilityLocation.levelId!}:${facilityLocation.positionId!}</#if><#if facilityLocationTypeEnum?has_content> (${facilityLocationTypeEnum.get("description",locale)})</#if> [${(inventoryItem.locationSeqId)!}]</a></@td>
                            <@td>&nbsp;${(inventoryItem.lotId)!}</@td>
                            <@td>&nbsp;${(inventoryItem.binNumber)!}</@td>
                            <@td align="right"><@ofbizCurrency amount=inventoryItem.unitCost isoCode=inventoryItem.currencyUomId/></@td>
                            <@td>
                                <#if inventoryItemDetailFirst?? && inventoryItemDetailFirst.workEffortId??>
                                    <b>${uiLabelMap.ProductionRunId}</b> ${inventoryItemDetailFirst.workEffortId}
                                <#elseif inventoryItemDetailFirst?? && inventoryItemDetailFirst.orderId??>
                                    <b>${uiLabelMap.OrderId}</b> ${inventoryItemDetailFirst.orderId}
                                </#if>
                            </@td>
                            <@td align="right">${(inventoryItemDetailFirst.quantityOnHandDiff)!}</@td>
                            <#if inventoryItem.inventoryItemTypeId! == "NON_SERIAL_INV_ITEM">
                                <@td align="right">${(inventoryItem.availableToPromiseTotal)?default("NA")}
                                    / ${(inventoryItem.quantityOnHandTotal)?default("NA")}
                                </@td>
                            <#elseif inventoryItem.inventoryItemTypeId! == "SERIALIZED_INV_ITEM">
                                <@td align="right">&nbsp;${(inventoryItem.serialNumber)!}</@td>
                            <#else>
                                <@td align="right" style="color: red;">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSerialNumber} (${(inventoryItem.serialNumber)!})
                                    ${uiLabelMap.ProductAndQuantityOnHand} (${(inventoryItem.quantityOnHandTotal)!} ${uiLabelMap.CommonSpecified}</@td>
                            </#if>
                        </@tr>
                    </#if>
                </#if>
            </#list>
          </@table>
        </#if>
  <#else>
    <@alert type="error">${uiLabelMap.ProductProductNotFound} ${productId!}!</@alert>
  </#if>
</@section>
