<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if quote??>
<form action="<@ofbizUrl>copyQuote</@ofbizUrl>" method="post">
    <input type="hidden" name="quoteId" value="${quoteId}"/>
    <div>
        <span>${uiLabelMap.OrderCopyQuote}</span>
        ${uiLabelMap.OrderOrderQuoteItems}&nbsp;<input type="checkbox" name="copyQuoteItems" value="Y" checked="checked" />
        ${uiLabelMap.OrderOrderQuoteAdjustments}&nbsp;<input type="checkbox" name="copyQuoteAdjustments" value="Y" checked="checked" />
        ${uiLabelMap.OrderOrderQuoteRoles}&nbsp;<input type="checkbox" name="copyQuoteRoles" value="Y" checked="checked" />
        ${uiLabelMap.OrderOrderQuoteAttributes}&nbsp;<input type="checkbox" name="copyQuoteAttributes" value="Y" checked="checked" />
        ${uiLabelMap.OrderOrderQuoteCoefficients}&nbsp;<input type="checkbox" name="copyQuoteCoefficients" value="Y" checked="checked" />
        ${uiLabelMap.OrderOrderQuoteTerms}&nbsp;<input type="checkbox" name="copyQuoteTerms" value="Y" checked="checked" />
    </div>
    <input type="submit" class="${styles.link_run_sys!} ${styles.action_copy!}" value="${uiLabelMap.CommonCopy}"/>
</form>
</#if>