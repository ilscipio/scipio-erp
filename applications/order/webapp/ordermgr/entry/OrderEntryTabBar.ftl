<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section>   
    <@menu type="button">
      <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=purchase&finalizeReqCustInfo=false&finalizeReqShipInfo=false&finalizeReqOptions=false&finalizeReqPayInfo=false") text=uiLabelMap.OrderFinalizeOrder 
          class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.getOrderPartyId() == "_NA_" || (shoppingCart.size() == 0))/>
      <#else>
        <@menuitem type="link" href=makeOfbizUrl("quickcheckout") text=uiLabelMap.OrderQuickFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.size() == 0)/>
        <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=init") text=uiLabelMap.OrderFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.size() == 0)/>
        <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=default") text=uiLabelMap.OrderFinalizeOrderDefault class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.size() == 0)/>
      </#if>
      <@menuitem type="link" href="javascript:document.cartform.submit()" text=uiLabelMap.OrderRecalculateOrder class="+${styles.action_run_session!} ${styles.action_update!}" disabled=(shoppingCart.size() == 0)/>
      <@menuitem type="link" href="javascript:removeSelected();" text=uiLabelMap.OrderRemoveSelected class="+${styles.action_run_session!} ${styles.action_remove!}" disabled=(shoppingCart.size() == 0)/>
      <@menuitem type="link" href=makeOfbizUrl("emptycart") text=uiLabelMap.OrderClearOrder class="+${styles.action_run_session!} ${styles.action_clear!}" />
    </@menu>
</@section>
