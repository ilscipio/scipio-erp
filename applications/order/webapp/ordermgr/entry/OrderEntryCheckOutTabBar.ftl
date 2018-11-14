<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if stepTitleId??>
    <#assign stepTitle = uiLabelMap.get(stepTitleId)>
</#if>

<#assign sectionTitle>
    <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        ${rawLabel('OrderPurchaseOrder')}
    <#else>
        ${rawLabel('OrderSalesOrder')}
    </#if>
    : ${rawString(stepTitle!)}
</#assign>

<@section title=sectionTitle>
    <@menu type="button">
      <#list checkoutSteps?reverse as checkoutStep>
        <#assign stepUiLabel = uiLabelMap.get(checkoutStep.label)>
        <#if checkoutStep.enabled == "N">
            <@menuitem type="link" text=stepUiLabel disabled=true class="+${styles.action_nav!}" />
        <#else>
            <@menuitem type="link" href=makeOfbizUrl("${checkoutStep.uri}") text=stepUiLabel class="+${styles.action_nav!}" />
        </#if>
      </#list>
      <#if isLastStep == "N">
        <@menuitem type="link" href="javascript:document.checkoutsetupform.submit();" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" />
      <#else>
        <@menuitem type="link" href=makeOfbizUrl("processorder") text=uiLabelMap.OrderCreateOrder class="+${styles.action_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
      </#if>
    </@menu>
</@section>
