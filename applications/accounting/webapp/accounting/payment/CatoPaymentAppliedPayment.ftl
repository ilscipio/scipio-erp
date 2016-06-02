<#-- SCIPIO -->
<#if paymentList?has_content> <#-- FIXME: Ugly workaround, because variables set by entity-condition do not validate correctly on screen condition -->
<@section title=uiLabelMap.AccountingAppliedPayments>
    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th >${uiLabelMap.CommonInvoice}</@th>
                <#--<@th >${uiLabelMap.CommonSequenceNum}</@th>-->
                <@th >${uiLabelMap.CommonTo}</@th>
                <@th class="${styles.text_right!}">${uiLabelMap.AccountingAmountApplied}</@th>
            </@tr>
        </@thead>
        <#list paymentList as iApp>
            <#assign amountApplied = Static["org.ofbiz.accounting.payment.PaymentWorker"].getPaymentAppliedAmount(delegator, iApp.paymentApplicationId!0)/>
            <@tr>
                <@td><a href="<@ofbizUrl>invoiceOverview?invoiceId=${iApp.invoiceId!}</@ofbizUrl>">${iApp.invoiceId!}</a></@td>
                <#--<@td>${iApp.invoiceItemSeqId!}</@td>-->
                <#if iApp.billingAccountId?has_content>
                    <#assign billingAcct = iApp.getRelatedOne("BillingAccount", false)/>
                        <@td><a href="<@ofbizUrl>EditBillingAccount?billingAccountId=${invoice.billingAccountId!}</@ofbizUrl>">${billingAcct.get('description',locale)}</a></@td>
                    <#else>
                        <@td></@td>
                </#if>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=payment.currencyUomId amount=(iApp.amountApplied!)/></@td>
            </@tr>
        </#list>        
    </@table>
</@section>
</#if>