<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if stepTitleId??>
    <#assign stepTitle = uiLabelMap[stepTitleId]>
</#if>

<@section title=rawLabel((shoppingCart.getOrderType() == "PURCHASE_ORDER")?then('OrderPurchaseOrder', 'OrderSalesOrder'))+": "+rawString(stepTitle!)>
    <@menu type="button">
      <#-- SCIPIO: Why reverse this? Silly
      <#list checkoutSteps?reverse as checkoutStep>-->
      <#list checkoutSteps as checkoutStep>
        <@menuitem type="link" href=makeOfbizUrl("${checkoutStep.uri}") text=uiLabelMap[checkoutStep.label] class="+${styles.action_nav!}" disabled=(checkoutStep.enabled == "N") />
      </#list>

      <#if "quick" == checkoutType!><#-- SCIPIO: new -->
        <#if isLastStep == "N">
          <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'DN', '');" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" />
        <#else>
          <@menuitem type="link" href=makeOfbizUrl("processorder") text=uiLabelMap.OrderCreateOrder class="+${styles.action_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
        </#if>
      <#else>
        <#if isLastStep == "N">
          <@menuitem type="link" href="javascript:document.checkoutsetupform.submit();" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" />
        <#else>
          <@menuitem type="link" href=makeOfbizUrl("processorder") text=uiLabelMap.OrderCreateOrder class="+${styles.action_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
        </#if>
      </#if>
    </@menu>
</@section>
