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
            <@commonMsg type="error">${illegalInventoryItem}</@commonMsg>
      </#if>
        <@menu type="button">
          <@menuitem type="link" href=makeOfbizUrl("EditFacility") class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.ProductNewFacility />
          <@menuitem type="link" href=makeOfbizUrl("PickMoveStockSimple?facilityId=${facilityId!}") class="+${styles.action_run_sys!} ${styles.action_export!}" text=uiLabelMap.CommonPrint />
        </@menu>
       <#if !(inventoryItem??)>
            <form method="post" action="<@ofbizUrl>TransferInventoryItem</@ofbizUrl>">
            <input type="hidden" name="facilityId" value="${facilityId}" />
        <@row>
          <@cell columns=9>
            <@field type="input" label=uiLabelMap.ProductInventoryItemId name="inventoryItemId" size="20" maxlength="20" />
          </@cell>
          <@cell columns=3>
            <@field type="submit" text=uiLabelMap.ProductGetItem class="+${styles.link_run_sys!} ${styles.action_transfer!}" />
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
            
            <@field type="display" label=uiLabelMap.ProductInventoryItemId>
                ${inventoryItemId}
            </@field>
            <@field type="display" label=uiLabelMap.ProductInventoryItemTypeId>
                <#if inventoryItemType??>
                    ${(inventoryItemType.get("description",locale))!}
                </#if>
            </@field>
            <@field type="display" label=uiLabelMap.ProductProductId>
                <#if inventoryItem?? && (inventoryItem.productId)??>
                    <a href="<@ofbizInterWebappUrl>/catalog/control/EditProduct?productId=${(inventoryItem.productId)!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${(inventoryItem.productId)!}</a>
                </#if>
            </@field>
            <@field type="display" label=uiLabelMap.CommonStatus>
                ${(inventoryStatus.get("description",locale))!("--")}
            </@field>

            <@field type="display" label=uiLabelMap.ProductComments>
                ${(inventoryItem.comments)!("--")}
            </@field>

            <@field type="display" label=uiLabelMap.ProductSerialAtpQoh>
                <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                        ${(inventoryItem.availableToPromiseTotal)!}&nbsp;
                        /&nbsp;${(inventoryItem.quantityOnHandTotal)!}
                <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
                    ${(inventoryItem.serialNumber)!}
                <#elseif inventoryItem??>
                    <@commonMsg type="error">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</@commonMsg>
                </#if>
            </@field>

            <hr />
  
        <@field type="select" label=uiLabelMap.ProductTransferStatus name="statusId">
            <#if (inventoryTransfer.statusId)??>
                <#assign curStatusItem = inventoryTransfer.getRelatedOne("StatusItem", true)>
                <option value="${(inventoryTransfer.statusId)!}">${(curStatusItem.get("description",locale))!}</option>
            </#if>
            <#list statusItems as statusItem>
            <option value="${(statusItem.statusId)!}">${(statusItem.get("description",locale))!}</option>
            </#list>
        </@field>
        <@field type="generic" label=uiLabelMap.ProductTransferSendDate>
            <@field type="input" name="sendDate" value="${(inventoryTransfer.sendDate)!}" size="22" />
            <a href="#" onclick="javascript:setNow('sendDate')" class="${styles.link_run_local!} ${styles.action_update!}">${uiLabelMap.CommonNow}</a>
        </@field>
        <#if !(inventoryTransfer??)>
            <@field type="generic" label=uiLabelMap.ProductToFacilityContainer>
                <@field type="select" name="facilityIdTo" tooltip=uiLabelMap.ProductSelectFacility!>
                    <#list facilities as nextFacility>
                    <option value="${(nextFacility.facilityId)!}">${(nextFacility.facilityName)!} [${(nextFacility.facilityId)!}]</option>
                    </#list>
                </@field>
                <@field type="input" name="containerIdTo" value="${(inventoryTransfer.containerIdTo)!}" size="20" maxlength="20" tooltip=uiLabelMap.ProductOrEnterContainerId!/>
            </@field>
            <@field type="lookup" label=uiLabelMap.ProductToLocation value="${(inventoryTransfer.locationSeqIdTo)!}" formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation"/>
            <@field type="input" label=uiLabelMap.ProductComments name="comments" size="60" maxlength="250" />
            <@field type="generic" label=uiLabelMap.ProductQuantityToTransfer>
                <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                    <@field type="input" size="5" name="xferQty" value="${(inventoryItem.availableToPromiseTotal)!}" />
                <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
                    <input type="hidden" name="xferQty" value="1" />
                    1
                <#elseif inventoryItem??>
                    <@commonMsg type="warning">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</@commonMsg>
                </#if>
            </@field>
        <#else>
            <@field type="generic" label=uiLabelMap.ProductTransferReceiveDate>
                <@field type="input" name="receiveDate" value="${(inventoryTransfer.receiveDate)!}" size="22" />
                <a href="#" onclick="javascript:setNow('receiveDate')" class="${styles.link_run_local!} ${styles.action_update!}">${uiLabelMap.CommonNow}</a>
            </@field>
            <@field type="display" label=uiLabelMap.ProductToFacilityContainer>
                <#assign fac = delegator.findOne("Facility", {"facilityId":inventoryTransfer.facilityIdTo}, false)> 
                ${(fac.facilityName)!}
            </@field>
            <@field type="lookup" label=uiLabelMap.ProductToLocation value="${(inventoryTransfer.locationSeqIdTo)!}" formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation?facilityId=${inventoryTransfer.facilityIdTo}"/>
            <@field type="input" label=uiLabelMap.ProductComments name="comments" value="${(inventoryTransfer.comments)!}" size="60" maxlength="250" />
        </#if>
        <#if !(inventoryTransfer??)>
            <@field type="submit" text=uiLabelMap.ProductTransfer class="+${styles.link_run_sys!} ${styles.action_transfer!}" />
        <#else>
            <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
        </#if>
        </form>
        </#if>
