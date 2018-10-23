<#-- SCIPIO -->
<#if invoices?has_content>
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
    <#list invoices as item>
        <#assign total = Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceTotal(delegator, item.invoiceId) />
        <#assign outstandingAmount = Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceNotApplied(delegator, item.invoiceId) />       
        <#assign itemType = item.getRelatedOne("InvoiceType", false)/>
        <@tr>
            <@td><a href="<@ofbizUrl>invoiceOverview?invoiceId=${item.invoiceId}</@ofbizUrl>">${item.invoiceId!}</a></@td>
            <@td>${itemType.get("description",locale)!}</@td>
            <#-- <@td>${item.partyIdFrom}</@td> -->
            <@td><#if item.dueDate?has_content><@formattedDateTime date=item.dueDate /></#if></@td>
            <@td><@ofbizCurrency isoCode=item.currencyUomId amount=total!0/></@td>              
            <@td><strong><@ofbizCurrency isoCode=item.currencyUomId amount=outstandingAmount!0/></strong></@td>
        </@tr>
    </#list>
</@table>
<#else>
  <@commonMsg type="result-norecord" />
</#if>
