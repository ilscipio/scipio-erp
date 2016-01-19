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

      <#if illegalInventoryItem??>
            <div class="errorMessage">${illegalInventoryItem}</div>
      </#if>
        <div class="button-bar">
          <a href="<@ofbizUrl>EditFacility</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.ProductNewFacility}</a>
          <a href="<@ofbizUrl>PickMoveStockSimple?facilityId=${facilityId!}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">${uiLabelMap.CommonPrint}</a>
        </div>
       <#if !(inventoryItem??)>
            <form method="post" action="<@ofbizUrl>TransferInventoryItem</@ofbizUrl>">
            <input type="hidden" name="facilityId" value="${facilityId}" />
        <@row>
          <@cell columns=9>
            <@field type="generic" label="${uiLabelMap.ProductInventoryItemId}">
                <input type="text" name="inventoryItemId" size="20" maxlength="20" />
            </@field>
          </@cell>
          <@cell columns=3>
            <@field type="submitarea">
                <input type="submit" value="${uiLabelMap.ProductGetItem}" />
            </@field>
          </@cell>
        </@row>
            </form>
        <#else>
           <#if !(inventoryTransfer??)>
                <form method="post" action="<@ofbizUrl>CreateInventoryTransfer</@ofbizUrl>" name="transferform">
            <#else>
                <form method="post" action="<@ofbizUrl>UpdateInventoryTransfer</@ofbizUrl>" name="transferform">
                <input type="hidden" name="inventoryTransferId" value="${inventoryTransferId!}" />
            </#if>

            <@script>
                function setNow(field) { eval('document.transferform.' + field + '.value="${nowTimestamp}"'); }
            </@script>

            <input type="hidden" name="inventoryItemId" value="${inventoryItemId!}" />
            <input type="hidden" name="facilityId" value="${facilityId!}" />
            <input type="hidden" name="locationSeqId" value="${(inventoryItem.locationSeqId)!}" />
            
            <@field type="generic" label="${uiLabelMap.ProductInventoryItemId}">
                ${inventoryItemId}
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductInventoryItemTypeId}">
                <#if inventoryItemType??>
                    ${(inventoryItemType.get("description",locale))!}
                </#if>
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductProductId}">
                <#if inventoryItem?? && (inventoryItem.productId)??>
                        <a href="/catalog/control/EditProduct?productId=${(inventoryItem.productId)!}" class="${styles.link_nav_record_id!}">${(inventoryItem.productId)!}</a>
                    </#if>
            </@field>
            <@field type="generic" label="${uiLabelMap.CommonStatus}">
                ${(inventoryStatus.get("description",locale))?default("--")}
            </@field>

            <@field type="generic" label="${uiLabelMap.ProductComments}">
                ${(inventoryItem.comments)?default("--")}
            </@field>

            <@field type="display" label="${uiLabelMap.ProductSerialAtpQoh}">
                <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                        ${(inventoryItem.availableToPromiseTotal)!}&nbsp;
                        /&nbsp;${(inventoryItem.quantityOnHandTotal)!}
                <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
                    ${(inventoryItem.serialNumber)!}
                <#elseif inventoryItem??>
                    <@alert type="error">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</@alert>
                </#if>
            </@field>

            <hr />
  
            <@field type="generic" label="${uiLabelMap.ProductTransferStatus}">
                <select name="statusId">
                    <#if (inventoryTransfer.statusId)??>
                        <#assign curStatusItem = inventoryTransfer.getRelatedOne("StatusItem", true)>
                        <option value="${(inventoryTransfer.statusId)!}">${(curStatusItem.get("description",locale))!}</option>
                    </#if>
                    <#list statusItems as statusItem>
                    <option value="${(statusItem.statusId)!}">${(statusItem.get("description",locale))!}</option>
                    </#list>
                </select>
            </@field>
        <@field type="generic" label="${uiLabelMap.ProductTransferSendDate}">
            <input type="text" name="sendDate" value="${(inventoryTransfer.sendDate)!}" size="22" />
            <a href="#" onclick="setNow('sendDate')" class="${styles.link_run_local!} ${styles.action_update!}">${uiLabelMap.CommonNow}</a>
        </@field>
        <#if !(inventoryTransfer??)>
            <@field type="generic" label="${uiLabelMap.ProductToFacilityContainer}">
                <select name="facilityIdTo">
                    <#list facilities as nextFacility>
                    <option value="${(nextFacility.facilityId)!}">${(nextFacility.facilityName)!} [${(nextFacility.facilityId)!}]</option>
                    </#list>
                </select>
                <span class="tooltip">${uiLabelMap.ProductSelectFacility}</span>
                <br />
                <input type="text" name="containerIdTo" value="${(inventoryTransfer.containerIdTo)!}" size="20" maxlength="20" />
                <span class="tooltip">${uiLabelMap.ProductOrEnterContainerId}</span>
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductToLocation}">
                <@htmlTemplate.lookupField value="${(inventoryTransfer.locationSeqIdTo)!}" formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation"/>
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductComments}">
                <input type="text" name="comments" size="60" maxlength="250" />
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductQuantityToTransfer}">
                <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                    <input type="text" size="5" name="xferQty" value="${(inventoryItem.availableToPromiseTotal)!}" />
                <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
                    <input type="hidden" name="xferQty" value="1" />
                    1
                <#elseif inventoryItem??>
                    <span class="alert">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</span>
                </#if>
            </@field>
        <#else>
            <@field type="generic" label="${uiLabelMap.ProductTransferReceiveDate}">
                <input type="text" name="receiveDate" value="${(inventoryTransfer.receiveDate)!}" size="22" />
                <a href="#" onclick="setNow('receiveDate')" class="${styles.link_run_local!} ${styles.action_update!}">${uiLabelMap.CommonNow}</a>
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductToFacilityContainer}">
                <#assign fac = delegator.findOne("Facility", Static["org.ofbiz.base.util.UtilMisc"].toMap("facilityId", inventoryTransfer.facilityIdTo), false)> ${(fac.facilityName)?default("&nbsp;")}
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductToLocation}">
                <@htmlTemplate.lookupField value="${(inventoryTransfer.locationSeqIdTo)!}" formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation?facilityId=${inventoryTransfer.facilityIdTo}"/>
            </@field>
            <@field type="generic" label="${uiLabelMap.ProductComments}">
                <input type="text" name="comments" value="${(inventoryTransfer.comments)!}" size="60" maxlength="250" />
            </@field>
        </#if>
        <@field type="submitarea">
            <#if !(inventoryTransfer??)>
                <input type="submit" value="${uiLabelMap.ProductTransfer}" />
            <#else>
                <input type="submit" value="${uiLabelMap.CommonUpdate}" />
            </#if>
        </@field>
        </form>
        </#if>
