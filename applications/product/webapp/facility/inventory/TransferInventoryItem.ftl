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
<#if !(requestParameters.inventoryTransferId?has_content)>
    <#assign formAction="CreateInventoryTransfer" />
<#else>
    <#assign formAction="UpdateInventoryTransfer" />
</#if>

<form method="post" action="<@ofbizUrl>${formAction}</@ofbizUrl>" name="transferform">
    <#if !(requestParameters.inventoryTransferId?has_content)>
        <@section id="inventoryItemDetail">
            <@field type="lookup" label=uiLabelMap.ProductInventoryItemId name="inventoryItemId" size="20" maxlength="20" formName="transferform" id="inventoryItemId" fieldFormName="LookupInventoryItem" postfix=true/>             
        </@section>
        <@script>
            jQuery(document).ready(function() {
                $('body').on('click','#inventoryItemDetail input[type=submit].${styles.icon_button}', function(e) {
                    e.preventDefault();
                    submitInventoryItemId();
                });                    
                function submitInventoryItemId() {                    
                    if ($('input[name=inventoryItemId]').val().length > 0) {
                        $.ajax({
                            url : 'TransferInventoryItemDetail',
                            method: 'POST',
                            data: { 'inventoryItemId' : $('input[name=inventoryItemId]').val(), 'facilityId': "${facilityId!}" }
                        }).done(function(data) {
                            $("#inventoryItemDetail").html(data);
                        });
                    }
                }
            });
        </@script>
    <#else>
        <@render resource="component://product/widget/facility/FacilityScreens.xml#TransferInventoryItemDetail" />
    </#if>
</form>
   
<#--</#if>-->
