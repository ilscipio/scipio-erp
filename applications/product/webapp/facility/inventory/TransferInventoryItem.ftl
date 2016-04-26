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
<#if !(inventoryTransfer??)>
    <#assign formAction="CreateInventoryTransfer" />
<#else>
    <#assign formAction="UpdateInventoryTransfer" />
</#if>

<form method="post" action="<@ofbizUrl>${formAction}</@ofbizUrl>" name="transferform">
    <#if !(inventoryTransfer??)>
        <@section id="inventoryItemDetail">        
             <@field type="generic" label=uiLabelMap.ProductInventoryItemId>
                <@field type="lookup" inline=true label=uiLabelMap.ProductInventoryItemId name="inventoryItemId"  size="20" maxlength="20" formName="transferform" id="inventoryItemId" fieldFormName="LookupInventoryItem"/>
                <@field type="submit" inline=true submitType="button" text=uiLabelMap.CommonFind id="findInventoryItem" />
             </@field>
             <@script>
                jQuery(document).ready(function() {
                    $("#findInventoryItem").click(function() {
                        console.log('show inventory item id for ' + $('input[name=inventoryItemId]').val());
                        if ($('input[name=inventoryItemId]').val().length > 0) {
                            $.ajax({
                                url : 'TransferInventoryItemDetail',
                                method: 'POST',
                                data: { 'inventoryItemId' :  "$('input[name=inventoryItemId]').val())", 'facilityId': "${facilityId!}" }
                            }).done(function(data) {
                                $("#inventoryItemDetail").html(data);
                            });
                        }
                    });
                });
            </@script>
        </@section>
    <#else>
        <#include "TransferInventoryItemDetail.ftl"/>
    </#if>
</form>
   
<#--</#if>-->
