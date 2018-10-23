<#if physicalInventory?has_content>
    <@section menuContent=menuContent title=uiLabelMap.ProductPhysicalInventoryList>
        <form action="<@ofbizUrl>createPhysicalVariances?facilityId=${parameters.facilityId}&productId=${parameters.productId}&internalName=${parameters.internalName}</@ofbizUrl>" method="post" name="ListPhysicalInventory">          
            <input type="hidden" name="_useRowSubmit" value="Y" />                       
            <@table type="data-list" autoAltRows=true scrollable=true responsive=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductInventoryItemId}</@th>
                        <@th>${uiLabelMap.ProductId}</@th>
                        <@th>${uiLabelMap.ProductInternalName}</@th>
                        <@th>${uiLabelMap.ProductItemATP}</@th>
                        <@th>${uiLabelMap.ProductItemQOH}</@th>
                        <@th>${uiLabelMap.ProductProductATP}</@th>
                        <@th>${uiLabelMap.ProductProductQOH}</@th>
                        <@th>${uiLabelMap.FormFieldTitle_varianceReasonId}</@th>
                        <@th>${uiLabelMap.ProductProductATPVar}</@th>
                        <@th>${uiLabelMap.ProductProductQOHVar}</@th>                                
                        <@th>${uiLabelMap.CommonUpdate}</@th>                        
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <#list physicalInventory as productInventory>
                    <@tr>
                        <@td>                            
                            <input type="hidden" value="${productInventory.productId}" name="productId_o_${productInventory_index}"/>         
                            <input type="hidden" value="${productInventory.inventoryItemId}" name="inventoryItemId_o_${productInventory_index}">                            
                            <input type="hidden" value="${productInventory.internalName!}" name="internalName_o_${productInventory_index}">
                            <input type="hidden" value="${productInventory.availableToPromiseTotal}" name="availableToPromiseTotal_o_${productInventory_index}">
                            <input type="hidden" value="${productInventory.quantityOnHandTotal}" name="quantityOnHandTotal_o_${productInventory_index}">
                            <input type="hidden" value="${productInventory.productATP}" name="productATP_o_${productInventory_index}">
                            <input type="hidden" value="${productInventory.productQOH}" name="productQOH_o_${productInventory_index}">                                                     
                            <input type="hidden" name="_rowSubmit_o_${productInventory_index}" value="N"/>
                            <a href="<@ofbizUrl>EditFacilityInventoryItems?facilityId=${parameters.facilityId}&inventoryItemId=${productInventory.inventoryItemId}</@ofbizUrl>">${productInventory.inventoryItemId}</a>
                        </@td>
                        <@td><a href="<@ofbizInterWebappUrl>/catalog/control/EditProductInventoryItems?productId=${productInventory.productId}</@ofbizInterWebappUrl>">${productInventory.productId!}</a></@td>
                        <@td>${productInventory.internalName!}</@td>
                        <@td>${productInventory.availableToPromiseTotal!}</@td>
                        <@td>${productInventory.quantityOnHandTotal!}</@td>
                        <@td>${productInventory.productATP!}</@td>     
                        <@td>${productInventory.productQOH!}</@td>
                        <@td>
                            <@field type="select" name="varianceReasonId_o_${productInventory_index}">
                                <#list varianceReasonList as varianceReason>
                                    <option value="${varianceReason.varianceReasonId}">${varianceReason.description!}</option>
                                </#list>
                            </@field>
                        </@td>
                        <@td><@field type="input" name="availableToPromiseVar_o_${productInventory_index}" size=20 maxlength=50 /></@td>
                        <@td><@field type="input" name="quantityOnHandVar_o_${productInventory_index}" size=20 maxlength=50 /></@td>                        
                        <@td><@field type="submit" submitType="link" href="javascript:$('input[name=_rowSubmit_o_${productInventory_index}]').val('Y');console.log($('input[name=_rowSubmit_o_${productInventory_index}]').val());document.forms.ListPhysicalInventory.submit();" name="Update" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys} ${styles.action_update}"/></@td>
                    </@tr>
                </#list>
            </@table>    
        </form>
    </@section>         
</#if>