<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
