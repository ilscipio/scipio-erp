<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if quote?? && quote.statusId == "QUO_APPROVED">
            <@menuitem type="link" href=makePageUrl("loadCartFromQuote?quoteId=" + quote.quoteId + "&amp;finalizeMode=init") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.OrderCreateOrder />
        </#if>
    </@menu>
</#macro>

<@section title=title menuContent=menuContent>
    <#if quote?has_content>
        <@table type="fields" class="${styles.table_basic!}" cellspacing="0">

            <#-- quote id -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderQuote} ${uiLabelMap.CommonNbr}</@td>
                <@td colspan="3">
                    ${quote.quoteId!}
                </@td>
            </@tr>
            <#-- quote name -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonName}</@td>
                <@td colspan="3">
                    ${quote.quoteName!}
                </@td>
            </@tr>
            <#-- quote description -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonDescription}</@td>
                <@td colspan="3">
                    ${quote.description!}
                </@td>
            </@tr>
            <#-- issue date -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderOrderQuoteIssueDate}</@td>
                <@td colspan="3">
                    ${quote.issueDate!}
                </@td>
            </@tr>
            <#-- valid from date -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonValidFromDate}</@td>
                <@td colspan="3">
                    ${quote.validFromDate!}
                </@td>
            </@tr>
            <#-- valid thru date -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonValidThruDate}</@td>
                <@td colspan="3">
                    ${quote.validThruDate!}
                </@td>
            </@tr>
        </@table>
    </#if>
</@section>