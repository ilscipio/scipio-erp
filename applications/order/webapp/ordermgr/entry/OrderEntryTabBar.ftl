<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section>   
      <@menu type="button">
      <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        <#if shoppingCart.getOrderPartyId() == "_NA_" || (shoppingCart.size() == 0)>
          <@menuitem type="link" text=uiLabelMap.OrderFinalizeOrder disabled=true class="+${styles.action_nav!} ${styles.action_complete!}"/>
        <#else>
          <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=purchase&finalizeReqCustInfo=false&finalizeReqShipInfo=false&finalizeReqOptions=false&finalizeReqPayInfo=false") text=uiLabelMap.OrderFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" />
        </#if>
      <#else>
        <#if shoppingCart.size() == 0>
          <@menuitem type="link" text=uiLabelMap.OrderQuickFinalizeOrder disabled=true class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" text=uiLabelMap.OrderFinalizeOrderDefault disabled=true class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" text=uiLabelMap.OrderFinalizeOrder disabled=true class="+${styles.action_nav!} ${styles.action_complete!}" />
        <#else>
          <@menuitem type="link" href=makeOfbizUrl("quickcheckout") text=uiLabelMap.OrderQuickFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=default") text=uiLabelMap.OrderFinalizeOrderDefault class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=init") text=uiLabelMap.OrderFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" />
        </#if>
      </#if>

      <#if (shoppingCart.size() > 0)>
        <@menuitem type="link" href="javascript:document.cartform.submit()" text=uiLabelMap.OrderRecalculateOrder class="+${styles.action_run_session!} ${styles.action_update!}" />
        <@menuitem type="link" href="javascript:removeSelected();" text=uiLabelMap.OrderRemoveSelected class="+${styles.action_run_session!} ${styles.action_remove!}" />
      <#else>
        <@menuitem type="link" text=uiLabelMap.OrderRecalculateOrder disabled=true class="+${styles.action_run_session!} ${styles.action_update!}" />
        <@menuitem type="link" text=uiLabelMap.OrderRemoveSelected disabled=true class="+${styles.action_run_session!} ${styles.action_remove!}" />
      </#if>
      <@menuitem type="link" href=makeOfbizUrl("emptycart") text=uiLabelMap.OrderClearOrder class="+${styles.action_run_session!} ${styles.action_clear!}" />
    </@menu>
</@section>
