<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if security.hasEntityPermission("ORDERMGR", "_UPDATE", request)>
  <@section title=uiLabelMap.OrderReceiveOfflinePayments>
      <a href="<@ofbizUrl>authview/${donePage}</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonBack}</a>
      <a href="javascript:document.paysetupform.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a>

      <form method="post" action="<@ofbizUrl>receiveOfflinePayments/${donePage}</@ofbizUrl>" name="paysetupform">
        <#if requestParameters.workEffortId??>
            <input type="hidden" name="workEffortId" value="${requestParameters.workEffortId}" />
        </#if>
        <input type="hidden" name="partyId" value="${orderRoles[0].partyId}" />

        <#if paymentMethods?has_content>
        <@table type="data-list">
         <@thead>
          <@tr class="header-row">
            <@th width="30%" align="right">${uiLabelMap.PaymentMethod}</@th>
            <@th>&nbsp;&nbsp;&nbsp;</@th>
            <@th>${uiLabelMap.OrderAmount}</@th>
            <@th>&nbsp;&nbsp;&nbsp;</@th>
            <@th width="60%">${uiLabelMap.OrderReference}</@th>
          </@tr>
          </@thead>
          <#list paymentMethods as payMethod>
          <@tr>
            <@td align="right">${payMethod.get("description",locale)?default(payMethod.paymentMethodId)}</@td>
            <@td>&nbsp;&nbsp;&nbsp;</@td>
            <@td><input type="text" size="7" name="${payMethod.paymentMethodId}_amount" /></@td>
            <@td>&nbsp;&nbsp;&nbsp;</@td>
            <@td><input type="text" size="15" name="${payMethod.paymentMethodId}_reference" /></@td>
          </@tr>
          </#list>
        </@table>
        </#if>
        
        <#if paymentMethodTypes?has_content>
        <@table type="data-list">
         <@thead>
          <@tr class="header-row">
            <@th width="30%" align="right">${uiLabelMap.OrderPaymentType}</@th>
            <@th>&nbsp;&nbsp;&nbsp;</@th>
            <@th>${uiLabelMap.OrderAmount}</@th>
            <@th>&nbsp;&nbsp;&nbsp;</@th>
            <@th width="60%">${uiLabelMap.OrderReference}</@th>
          </@tr>
         </@thead>
          <#list paymentMethodTypes as payType>
          <@tr>
            <@td align="right">${payType.get("description",locale)?default(payType.paymentMethodTypeId)}</@td>
            <@td>&nbsp;&nbsp;&nbsp;</@td>
            <@td><input type="text" size="7" name="${payType.paymentMethodTypeId}_amount" /></@td>
            <@td>&nbsp;&nbsp;&nbsp;</@td>
            <@td><input type="text" size="15" name="${payType.paymentMethodTypeId}_reference" /></@td>
          </@tr>
          </#list>
        </@table>
        </#if>
      </form>

      <a href="<@ofbizUrl>authview/${donePage}</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonBack}</a>
      <a href="javascript:document.paysetupform.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a>
  
  </@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>