<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#assign title = titleProperty!>
<#if custRequest?has_content>
    <#assign title = rawLabel('OrderRequest') +  raw(custRequest.custRequestId) + rawLabel('CommonInformation')>
</#if>

<@section title=title>
    <#if custRequest?has_content>
        <@table type="fields" class="${styles.table_basic!}" cellspacing="0">
            <#-- request header information -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonType}</@td>
                <@td colspan="3">
                    ${(custRequestType.get("description",locale))!(custRequest.custRequestTypeId)!}
                </@td>
            </@tr>
            <#-- request status information -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonStatus}</@td>
                <@td colspan="3">
                    ${(statusItem.get("description", locale))!(custRequest.statusId)!}
                </@td>
            </@tr>
            <#-- party -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.PartyPartyId}</@td>
                <@td colspan="3">
                    ${custRequest.fromPartyId!}
                </@td>
            </@tr>
            <#-- request name -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonName}</@td>
                <@td colspan="3">
                    ${custRequest.custRequestName!}
                </@td>
            </@tr>
            <#-- request description -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonDescription}</@td>
                <@td colspan="3">
                    ${custRequest.description!}
                </@td>
            </@tr>
            <#-- request currency -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonCurrency}</@td>
                <@td colspan="3">
                    <#if currency??>${currency.get("description", locale)?default(custRequest.maximumAmountUomId!)}</#if>
                </@td>
            </@tr>
            <#-- request currency -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.ProductStore}</@td>
                <@td colspan="3">
                    <#if store??>${store.storeName!(custRequest.productStoreId!)}</#if>
                </@td>
            </@tr>
            <#-- request comment -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonInternalComment}</@td>
                <@td colspan="3">
                    ${custRequest.internalComment!}
                </@td>
            </@tr>
            <#-- request reason -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonReason}</@td>
                <@td colspan="3">
                    ${custRequest.reason!}
                </@td>
            </@tr>
        </@table>
    <#else>
        <form name="newRequestFrom" action="<@pageUrl>createCustRequest</@pageUrl>"> 
            <@field type="hidden" name="productStoreId" value=productStoreId! widgetOnly=true />
            <@field type="select" name="custRequestTypeId" label=uiLabelMap.CommonType value="">
                <option value="">--</option>
                <#list custRequestTypes as custRequestType>
                    <option value="${custRequestType.custRequestTypeId}">${custRequestType.description!}</option>
                </#list>
            </@field>
            <#-- 
            <@field type="select" name="statusId" label=uiLabelMap.CommonStatus value="">
                <option value="">--</option>
                <#list custRequestStatuses as custRequestStatus>
                    <option value="${custRequestType.custRequestStatusId}">${custRequestType.description!}</option>
                </#list>
            </@field>
             -->
            <#-- <@field type="text" name="fromPartyId" label=uiLabelMap.PartyPartyId value="" /> -->
            <@field type="text" name="custRequestName" label=uiLabelMap.CommonName  />
            <@field type="textarea" name="description" label=uiLabelMap.CommonDescription />
            <@field type="textarea" name="reason" label=uiLabelMap.CommonReason />
            <@field type="textarea" name="internalComment" label=uiLabelMap.CommonInternalComment />
            
            
            <@field type="submitarea">
              <@field type="submit" submitType="link" href=makePageUrl("ListRequests") text=uiLabelMap.CommonGoBack class="+${styles.link_nav_cancel!}" />
              <@field type="submit" submitType="link" href="javascript:document.newRequestFrom.submit()" text=uiLabelMap.CommonSave class="+${styles.link_run_sys!} ${styles.action_update!}" />
            </@field>
            
            <#-- <@field type="text" name="maximumAmountUomId" label=uiLabelMap.CommonCurrency value="" /> -->
        </form>
    </#if>
</@section>
