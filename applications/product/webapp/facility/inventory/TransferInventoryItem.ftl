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
    <@menuitem type="link" href=makeOfbizUrl("PickMoveStockSimple?facilityId=${facilityId!}") class="+${styles.action_run_sys!} ${styles.action_export!}" text=uiLabelMap.CommonPrint />
</@menu>

<#--<#if !(inventoryItem??)>
    <form method="post" action="<@ofbizUrl>TransferInventoryItem</@ofbizUrl>">
        <input type="hidden" name="facilityId" value="${facilityId}" />
        <@row>
            <@cell columns=9>
                
            </@cell>
            <@cell columns=3>
                <@field type="submit" text=uiLabelMap.ProductGetItem class="+${styles.link_run_sys!} ${styles.action_transfer!}" />
            </@cell>
        </@row>
    </form>
<#else>-->
    <@section id="inventoryItemDetail">
        <#if !(inventoryTransfer??)>
            <form method="post" action="<@ofbizUrl>CreateInventoryTransfer</@ofbizUrl>" name="transferform">
            <@field type="lookup" label=uiLabelMap.ProductInventoryItemId name="inventoryItemId"  size="20" maxlength="20" formName="transferform" id="inventoryItemId" fieldFormName="LookupInventoryItem"/>
             <@script>
                jQuery(document).ready(function() {
                    $("input[name=inventoryItemId]").focusout(function() {
                        console.log('show inventory item id for ' + $('input[name=inventoryItemId]').val());
                        if ($('input[name=inventoryItemId]').val().length > 0) {
                            $.ajax({
                                url : 'TransferInventoryItem',
                                method: 'POST',
                                data: { 'inventoryItemId' :  "$('input[name=inventoryItemId]').val())", 'facilityId': "${facilityId!}" }
                            }).done(function(data) {
                                $("#inventoryItemDetail").html(data);
                            });
                        }
                    });
            
                });
            </@script>
            
        <#else>
            <form method="post" action="<@ofbizUrl>UpdateInventoryTransfer</@ofbizUrl>" name="transferform">
            <#include "InventoryItems.ftl"/>
        </#if>
    </@section>
  
    <@field type="select" label=uiLabelMap.ProductTransferStatus name="statusId">
        <#if (inventoryTransfer.statusId)??>
            <#assign curStatusItem = inventoryTransfer.getRelatedOne("StatusItem", true)>
            <option value="${(inventoryTransfer.statusId)!}">${(curStatusItem.get("description",locale))!}</option>
        </#if>
        <#if statusItems?has_content>
            <#list statusItems as statusItem>
            <option value="${(statusItem.statusId)!}">${(statusItem.get("description",locale))!}</option>
            </#list>
        </#if>
    </@field>
    <@field type="generic" label=uiLabelMap.ProductTransferSendDate>
        <@field type="datetime" dateType="date-time" name="sendDate" value=((inventoryTransfer.sendDate)!) size="22" />        
    </@field>
    <#if !(inventoryTransfer??)>
        <@field type="generic" label=uiLabelMap.ProductToFacilityContainer>
            <@field type="select" name="facilityIdTo" tooltip=(uiLabelMap.ProductSelectFacility!)>
                <#if facilities?has_content>
                    <#list facilities as nextFacility>
                    <option value="${(nextFacility.facilityId)!}">${(nextFacility.facilityName)!} [${(nextFacility.facilityId)!}]</option>
                    </#list>
                </#if>
            </@field>
            <@field type="input" name="containerIdTo" value=((inventoryTransfer.containerIdTo)!) size="20" maxlength="20" tooltip=(uiLabelMap.ProductOrEnterContainerId!)/>
        </@field>
        <@field type="lookup" label=uiLabelMap.ProductToLocation value=((inventoryTransfer.locationSeqIdTo)!) formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation"/>
        <@field type="input" label=uiLabelMap.ProductComments name="comments" size="60" maxlength="250" />

        <@field type="generic" label=uiLabelMap.ProductQuantityToTransfer>
            <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                <@field type="input" size="5" name="xferQty" value=((inventoryItem.availableToPromiseTotal)!) />
            <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
                <input type="hidden" name="xferQty" value="1" />
                <@field type="display" label="1" />
            <#elseif inventoryItem??>
                <@commonMsg type="warning">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</@commonMsg>
            </#if>
        </@field>
    <#else>
        <@field type="generic" label=uiLabelMap.ProductTransferReceiveDate>
            <@field type="input" name="receiveDate" value=((inventoryTransfer.receiveDate)!) size="22" />          
        </@field>
        <@field type="display" label=uiLabelMap.ProductToFacilityContainer>
            <#assign fac = delegator.findOne("Facility", {"facilityId":inventoryTransfer.facilityIdTo}, false)> 
            ${(fac.facilityName)!}
        </@field>
        <@field type="lookup" label=uiLabelMap.ProductToLocation value=((inventoryTransfer.locationSeqIdTo)!) formName="transferform" name="locationSeqIdTo" id="locationSeqIdTo" fieldFormName="LookupFacilityLocation?facilityId=${inventoryTransfer.facilityIdTo}"/>
        <@field type="input" label=uiLabelMap.ProductComments name="comments" value=((inventoryTransfer.comments)!) size="60" maxlength="250" />
    </#if>
    <#if !(inventoryTransfer??)>
        <@field type="submit" text=uiLabelMap.ProductTransfer class="+${styles.link_run_sys!} ${styles.action_transfer!}" />
    <#else>
        <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
    </#if>
    </form>
<#--</#if>-->
