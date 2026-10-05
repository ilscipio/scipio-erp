<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
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

