<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<div>
    <#if quote??>
        <#if quote.statusId == "QUO_APPROVED">
            <a href="<@ofbizUrl>loadCartFromQuote?quoteId=${quote.quoteId}&amp;finalizeMode=init</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.OrderCreateOrder}</a>
        <#else>
            <span class="${styles.link_run_sys!} ${styles.action_add!} ${styles.disabled!}">${uiLabelMap.OrderCreateOrder}</span>
        </#if>
    </#if>
</div>

