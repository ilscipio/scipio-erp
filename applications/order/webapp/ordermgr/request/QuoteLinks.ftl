<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if custRequestItem??>
  <@menu type="button">
  <#if quoteId?has_content>
    <@menuitem type="link" href=makePageUrl("EditQuoteItemForRequest?quoteId=${quoteId}&custRequestId=${custRequestItem.custRequestId}&custRequestItemSeqId=${custRequestItem.custRequestItemSeqId}") text="${rawLabel('PageTitleEditQuoteItemForCustRequest')} [${raw(quoteId)}]" class="+${styles.action_nav!} ${styles.action_update!}" />
  <#else>
    <@menuitem type="link" href=makePageUrl("CreateQuoteAndQuoteItemForRequest?custRequestId=${custRequestItem.custRequestId}&custRequestItemSeqId=${custRequestItem.custRequestItemSeqId}") text=uiLabelMap.PageTitleCreateQuoteForCustRequest class="+${styles.action_nav!} ${styles.action_add!}" />
  </#if>
  </@menu>
</#if>
