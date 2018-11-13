<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@menu type="button">
<#if showAll == "false">
  <@menuitem type="link" href=makeOfbizUrl("requestitemnotes?custRequestId=${custRequestId}&custRequestItemSeqId=${custRequestItemSeqId}&showAll=true") text=uiLabelMap.OrderShowAllNotes class="+${styles.action_run_sys!} ${styles.action_find!}" />
<#else>
  <@menuitem type="link" href=makeOfbizUrl("requestitemnotes?custRequestId=${custRequestId}&custRequestItemSeqId=${custRequestItemSeqId}&showAll=false") text=uiLabelMap.OrderShowThisItemsNotes class="+${styles.action_run_sys!} ${styles.action_find!}" />
</#if>
</@menu>
