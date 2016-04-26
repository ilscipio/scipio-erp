<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("TransferInventoryItem?facilityId=${facilityId}") text=uiLabelMap.ProductNewInventoryTransfer class="+${styles.action_run_sys!} ${styles.action_update!}" />
        <@menuitem type="link" href=makeOfbizUrl("FindFacilityTransfers?facilityId=${facilityId}&completeRequested=true") text=uiLabelMap.ProductCompleteRequestedTransfers class="+${styles.action_nav!} ${styles.action_find!}" />         
  </@menu>
</#macro>

<@section menuContent=menuContent>
    <@paginate mode="content" url=makeOfbizUrl("FindFacilityTransfers") paramStr=paramStr viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=arraySize!0>
        <form action="<@ofbizUrl>FindFacilityTransfers</@ofbizUrl>" method="post" name="findFacilityTransfers" class="basic-form">
            <input type="hidden" name="facilityId" value="${facilityId}" />
            <@row>
                <@cell columns=12>
                    <@field type="input" label=uiLabelMap.ProductInventoryItemId name="inventoryId" value="" size="20" maxlength="50" />
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" label=uiLabelMap.ProductProductId name="productId" value="" size="20" maxlength="50" />
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="select" label=uiLabelMap.ProductActiveOnly name="activeOnly">                 
                        <option value="Y" <#if parameters.activeOnly?has_content && parameters.activeOnly == "Y">selected="selected"</#if>>${uiLabelMap.CommonYes}</option>
                        <option value="N" <#if parameters.activeOnly?has_content && parameters.activeOnly == "N">selected="selected"</#if>>${uiLabelMap.CommonNo}</option>            
                    </@field>        
                </@cell>
            </@row>       
            <@field type="submit" name="look_up" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
        </form>
    </@paginate>
</@section>