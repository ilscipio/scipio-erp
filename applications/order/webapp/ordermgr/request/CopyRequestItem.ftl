<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if custRequestItem??>
<form action="<@ofbizUrl>copyCustRequestItem</@ofbizUrl>" method="post">
    <input type="hidden" name="custRequestId" value="${custRequestItem.custRequestId}"/>
    <input type="hidden" name="custRequestItemSeqId" value="${custRequestItem.custRequestItemSeqId}"/>
    <div>
        <span>${uiLabelMap.OrderCopyCustRequestItem}</span>
        ${uiLabelMap.OrderOrderQuoteItems}&nbsp;<input type="checkbox" name="copyLinkedQuotes" value="Y"/>
    </div>
    <input type="submit" class="${styles.link_run_sys!} ${styles.action_copy!}" value="${uiLabelMap.CommonCopy}"/>
</form>
</#if>