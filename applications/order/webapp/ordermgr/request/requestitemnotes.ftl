<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@menu type="button">
<#if showAll == "false">
  <@menuitem type="link" href=makePageUrl("requestitemnotes?custRequestId=${custRequestId}&custRequestItemSeqId=${custRequestItemSeqId}&showAll=true") text=uiLabelMap.OrderShowAllNotes class="+${styles.action_run_sys!} ${styles.action_find!}" />
<#else>
  <@menuitem type="link" href=makePageUrl("requestitemnotes?custRequestId=${custRequestId}&custRequestItemSeqId=${custRequestItemSeqId}&showAll=false") text=uiLabelMap.OrderShowThisItemsNotes class="+${styles.action_run_sys!} ${styles.action_find!}" />
</#if>
</@menu>
