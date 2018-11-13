<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#if canNotView>
  <@commonMsg type="error-perm">${uiLabelMap.AccountingCardInfoNotBelongToYou}.</@commonMsg>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizUrl(donePage) class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
  </@menu>
<#else>

<#-- SCIPIO: This was a message to explain to "Go Back" kludge; however I have now recoded controller and screen
    to redirect automatically.
<@commonMsg type="info-important">${uiLabelMap.ShopSaveGoBackExplanation}</@commonMsg>-->

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl(donePage) class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
    <@menuitem type="link" href="javascript:document.editcreditcardform.submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
  </@menu>
</#macro>
<#if !creditCard??>
  <#assign sectionTitle = uiLabelMap.AccountingAddNewCreditCard/>
<#else>
  <#-- SCIPIO: duplicate: <#assign sectionTitle = uiLabelMap.AccountingEditCreditCard/>-->
  <#assign sectionTitle = ""/>
</#if>
<@section title=sectionTitle menuContent=menuContent menuLayoutGeneral="bottom">

  <form method="post" action="<@ofbizUrl><#if !creditCard??>createCreditCard?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done<#else>updateCreditCard?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</#if></@ofbizUrl>" name="editcreditcardform">
  
  <#if creditCard??>
    <input type="hidden" name="paymentMethodId" value="${paymentMethodId}" />
  </#if>  

     <@render resource="component://shop/widget/CustomerScreens.xml#creditCardFields" />
     <@field type="generic" label=uiLabelMap.PartyBillingAddress>
        <#-- SCIPIO: Factored out for reuse -->
        <@render resource="component://shop/widget/CustomerScreens.xml#billaddresspickfields" 
            ctxVars={"bapfUseNewAddr":true, "bapfNewAddrInline":false, "bapfFieldNamePrefix":""}/>
     </@field>

  </form>  
</@section>
</#if>

