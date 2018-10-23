<#-- SCIPIO -->
<@section title=uiLabelMap.AccountingInvoiceItems>
    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th>${uiLabelMap.FormFieldTitle_invoiceItemSeqId!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_invoiceItemTypeId!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_productId!}</@th>
                <@th width="10%">${uiLabelMap.FormFieldTitle_orderId!}</@th>
                <@th width="20%" class="${styles.text_right!}">${uiLabelMap.FormFieldTitle_quantity!}</@th>
                <@th width="20%" class="${styles.text_right!}">${uiLabelMap.FormFieldTitle_amount!}</@th>
                <@th width="20%" class="${styles.text_right!}">${uiLabelMap.FormFieldTitle_total!}</@th>
            </@tr>
        </@thead>
        <#list invItemAndOrdItems as item>
            <#assign iTotal = (item.quantity!1 * item.amount!0)/>
            <#assign itemType = delegator.findOne("InvoiceItemType", {"invoiceItemTypeId" : item.invoiceItemTypeId!}, true)>
            <@tr>
                <@td><a href="<@ofbizUrl>invoiceOverview?invoiceId=${item.invoiceId}</@ofbizUrl>">${item.invoiceId!}</a></@td>
                <@td>${itemType.get("description",locale)!}</@td>
                <@td>${item.productId!}</@td>
                <@td>${item.orderId!}</@td>
                <@td class="${styles.text_right!}">${item.quantity!1}</@td>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=invoice.currencyUomId amount=(item.amount!)/></@td>
                <@td class="${styles.text_right!}"><strong><@ofbizCurrency isoCode=invoice.currencyUomId amount=(iTotal!)/></strong></@td>
            </@tr>
        </#list>
    </@table>
</@section>