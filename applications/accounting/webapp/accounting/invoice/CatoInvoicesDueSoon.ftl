<#-- CATO -->

<@table type="fields">
    <@thead>
        <@tr valign="bottom" class="header-row">
            <@th>${uiLabelMap.AccountingInvoiceID!}</@th>
            <@th>${uiLabelMap.CommonType!}</@th>
            <#-- <@th>${uiLabelMap.CommonFrom!}</@th> --->
            <@th>${uiLabelMap.CommonDate!}</@th>
            <@th>${uiLabelMap.CommonTotal!}</@th>
            <@th>${uiLabelMap.FormFieldTitle_amountToApply!}</@th>
        </@tr>
    </@thead>
    <#list PastDueInvoices as item>
        <#assign total =  Static["java.text.NumberFormat"].getNumberInstance(locale).format(Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceTotal(delegator, item.invoiceId)) />
        <#assign outstandingAmount =  Static["java.text.NumberFormat"].getNumberInstance(locale).format(Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceNotApplied(delegator, item.invoiceId)) />
        <#assign itemType = item.getRelatedOne("InvoiceType", false)/>
        <@tr>
            <@td><a href="<@ofbizUrl>/invoiceOverview?invoiceId=${item.invoiceId}</@ofbizUrl>">${item.invoiceId!}</a></@td>
            <@td>${itemType.get("description",locale)!}</@td>
            <#-- <@td>${item.partyIdFrom}</@td> -->
            <@td>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(item.dueDate, "", locale, timeZone)!}</@td>
            <@td><@ofbizCurrency isoCode=item.currencyUomId amount=total!/></@td>              
            <@td><strong><@ofbizCurrency isoCode=item.currencyUomId amount=outstandingAmount!/></strong></@td>
        </@tr>
    </#list>
</@table>