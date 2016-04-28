<#--
Cato: Local order template common defs
-->
<#-- This may include more generic files as well, as long as careful about double includes
<#include "....ftl">
-->

<#-- Cato: local macro where cells of label and widget areas are inverted and tweaked -->
<#macro checkoutInvField type="generic" postfixColumns="" widgetAreaClass="" widgetPostfixColumns="" postfixContent="" postfix=false inlineArgs...>
<#--
  <#local gridStyles = getDefaultFieldGridStyles({"labelArea":true, "postfix":true, "postfixColumns":postfixColumns, "widgetPostfixCombined":false})>
  <@row>
    <@cell class=addClassArg(gridStyles.labelArea, "${styles.text_right!}")>
      <#nested>
    </@cell>
    <#local id = (getRequestVar("catoLastFieldInfo").id)!"">
    <@cell class=gridStyles.widgetArea>
      ${labelContent}
    </@cell>  
    <@cell class=gridStyles.postfixArea>
      ${postfixContent}
    </@cell>  
  </@row>
-->
  <#if !postfixColumns?has_content>
    <#local postfixColumns = 3>
  </#if>
  <#if !widgetPostfixColumns?has_content>
    <#local widgetPostfixColumns = 11>
  </#if>
  <#if postfixContent?is_directive || postfixContent?has_content>
    <#local postfix = true>
  </#if>
  <#local widgetAreaClass = addClassArg(widgetAreaClass, styles.text_right!)>
  <@field type=type inverted=true args=inlineArgs widgetAreaClass=widgetAreaClass postfix=postfix postfixContent=postfixContent widgetPostfixColumns=widgetPostfixColumns postfixColumns=postfixColumns><#nested></@field>
</#macro>

<#macro checkoutActionsMenu text="" formName="" directLinks=true>
    <#if !formName?has_content>
      <#if directLinks>
        <#local formName = parameters.formNameValue!"">
      <#else>
        <#local formName = "checkoutInfoForm">
      </#if>
    </#if>
    <@row>
      <@cell columns=6>
        <@menu type="button">
        <#if directLinks>
          <@menuitem type="link" href=makeOfbizUrl("showcart") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.OrderBacktoShoppingCart />
        <#else>
          <@menuitem type="link" href="javascript:submitForm(document.${formName?js_string}, 'CS', '');" class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.OrderBacktoShoppingCart />
        </#if>
        </@menu>
      </@cell>
      <@cell columns=6 class="+${styles.text_right!}">
      <#local mainButtons><#nested></#local>
      <#if mainButtons?has_content>
        <@menu type="button">
          <#nested><#-- NOTE: must re-run #nested here -->
        </@menu>
      <#else>
        <@menu type="button">
          <#if !text?has_content>
            <#local text = uiLabelMap.CommonContinue>
          </#if>
          <#local class = "+${styles.action_run_session!} ${styles.action_continue!}">
          <#if directLinks>
            <#local href = "javascript:document.${formName?js_string}.submit();">
          <#else>
            <#local href = "javascript:submitForm(document.${formName?js_string}, 'DN', '');">
          </#if>
          <@menuitem type="link" href=href class=class text=text />
        </@menu>
      </#if>
      </@cell>
    </@row>
</#macro>

<#macro formattedCreditCard creditCard paymentMethod={} verbose=true>
  <#if verbose>
    <#--
    <#if !paymentMethod?has_content>
      <#local paymentMethod = creditCard.getRelatedOne("PaymentMethod")>
    </#if>
    -->
    ${(delegator.findOne("Enumeration", {"enumId":creditCard.cardType!}, true).get("description", locale))!creditCard.cardType!}<#t>
    <#local cardNum = creditCard.cardNumber!?string>
    <#if cardNum?has_content>
      <#if (cardNum?length > 4)>
        <#t> ${cardNum[0..<(cardNum?length-4)]?replace('.','*','r')}${cardNum[(cardNum?length-4)..]}
      <#else>
        <#t> ${cardNum}
      </#if>
    </#if>
    <#if creditCard.expireDate?has_content>
      <#t> ${creditCard.expireDate}
    </#if>
  <#else>
    <#-- stock ofbiz method -->
    ${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}<#t>
  </#if>
</#macro>
