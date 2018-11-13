<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if custRequestItem??>
  <@menu type="button">
  <#if quoteId?has_content>
    <@menuitem type="link" href=makeOfbizUrl("EditQuoteItemForRequest?quoteId=${quoteId}&custRequestId=${custRequestItem.custRequestId}&custRequestItemSeqId=${custRequestItem.custRequestItemSeqId}") text="${rawLabel('PageTitleEditQuoteItemForCustRequest')} [${rawString(quoteId)}]" class="+${styles.action_nav!} ${styles.action_update!}" />
  <#else>
    <@menuitem type="link" href=makeOfbizUrl("CreateQuoteAndQuoteItemForRequest?custRequestId=${custRequestItem.custRequestId}&custRequestItemSeqId=${custRequestItem.custRequestItemSeqId}") text=uiLabelMap.PageTitleCreateQuoteForCustRequest class="+${styles.action_nav!} ${styles.action_add!}" />
  </#if>
  </@menu>
</#if>
