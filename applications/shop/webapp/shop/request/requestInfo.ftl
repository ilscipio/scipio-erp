<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title="${rawLabel('OrderRequest')} ${rawString(custRequest.custRequestId)} ${rawLabel('CommonInformation')}">
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
</@section>
