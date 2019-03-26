<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#if canNotView>
  <@commonMsg type="error-perm">${uiLabelMap.AccountingEFTNotBelongToYou}.</@commonMsg>
  <@menu type="button">
    <@menuitem type="link" href=makePageUrl(donePage) class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
  </@menu>
<#else>

<#-- SCIPIO: This was a message to explain to "Go Back" kludge; however I have now recoded controller and screen
    to redirect automatically.
<@commonMsg type="info-important">${uiLabelMap.ShopSaveGoBackExplanation}</@commonMsg>-->

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makePageUrl(donePage) class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
    <@menuitem type="link" href="javascript:document.editeftaccountform.submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
  </@menu>
</#macro>
<#if !eftAccount??>
  <#assign sectionTitle = uiLabelMap.AccountingAddNewEftAccount />
<#else>
  <#-- SCIPIO: duplicate: <#assign sectionTitle = uiLabelMap.PageTitleEditEFTAccount/>-->
  <#assign sectionTitle = ""/>
</#if>
<@section title=sectionTitle menuContent=menuContent menuLayoutGeneral="bottom">

  <form method="post" action="<@pageUrl><#if !eftAccount??>createEftAccount?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done<#else>updateEftAccount?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</#if></@pageUrl>" name="editeftaccountform">

    <#if eftAccount??>
      <input type="hidden" name="paymentMethodId" value="${paymentMethodId}" />
    </#if>

    <@render resource="component://shop/widget/CustomerScreens.xml#eftAccountFields" ctxVars={"eafFieldNamePrefix":""} />

    <@field type="generic" label=uiLabelMap.PartyBillingAddress>
        <#-- SCIPIO: Billing fields are replaced with common new defs (old defs that were here discarded) -->
        <@render resource="component://shop/widget/CustomerScreens.xml#billaddresspickfields" 
            ctxVars={"bapfUseNewAddr":true, "bapfNewAddrInline":false, "bapfFieldNamePrefix":""}/>
    </@field>
  </form>
</@section>

</#if>

