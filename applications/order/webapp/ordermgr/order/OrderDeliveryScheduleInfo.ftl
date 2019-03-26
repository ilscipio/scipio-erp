<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if hasPermission>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
    <#if orderId??>
      <@menuitem type="link" href=makePageUrl("orderview?orderId=${orderId}") text=uiLabelMap.OrderViewOrder class="+${styles.action_nav!} ${styles.action_view!}" />
    </#if>
    </@menu>
  </#macro>
  <@section title=uiLabelMap.OrderScheduleDelivery menuContent=menuContent>
    <#if orderId?has_content>
      ${updatePODeliveryInfoWrapper.renderFormString(context)}
    <#else>
      ${uiLabelMap.OrderNoPurchaseSpecified}
    </#if>
  </@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>