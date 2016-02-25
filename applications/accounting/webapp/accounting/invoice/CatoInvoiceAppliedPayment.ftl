<#-- CATO -->
<@section title="${uiLabelMap.AccountingAppliedPayments!}">
    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th >${uiLabelMap.CommonPayment}</@th>
                <#--<@th >${uiLabelMap.CommonSequenceNum}</@th>-->
                <@th>${uiLabelMap.CommonProduct}</@th>
                <@th >${uiLabelMap.CommonDescription}</@th>
                <@th class="${styles.text_right!}">${uiLabelMap.CommonTotal}</@th>
                <@th class="${styles.text_right!}">${uiLabelMap.CommonAmount}</@th>
            </@tr>
        </@thead>
        <#list invoiceApplications as iApp>
            <@tr>
                <@td><a href="<@ofbizUrl>/paymentOverview?paymentId=${iApp.paymentId!}</@ofbizUrl>">${iApp.paymentId!}</a></@td>
                <#--<@td>${iApp.invoiceItemSeqId!}</@td>-->
                <@td>${iApp.productId!}</@td>
                <@td>${iApp.description!}</@td>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=invoice.currencyUomId amount=iApp.total!/></@td>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=invoice.currencyUomId amount=iApp.amountApplied!/></@td>
            </@tr>
        </#list>        
    </@table>
</@section>