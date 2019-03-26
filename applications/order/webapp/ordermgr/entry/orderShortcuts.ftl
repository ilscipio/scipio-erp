<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://order/webapp/ordermgr/common/common.ftl">

<#-- SCIPIO: Must use context or accessor
<#assign shoppingCart = sessionAttributes.shoppingCart!>-->
<#assign shoppingCart = getShoppingCart()!>

<@section title=uiLabelMap.OrderOrderShortcuts>
    <@menu type="button">
        <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
          <@menuitem type="link" href=makePageUrl("RequirementsForSupplier") text=uiLabelMap.OrderRequirements class="+${styles.action_nav!}" />
        </#if>
        <#if shoppingCart.getOrderType()?has_content && shoppingCart.items()?has_content>
          <@menuitem type="link" href=makePageUrl("createQuoteFromCart?destroyCart=Y") text=uiLabelMap.OrderCreateQuoteFromCart class="+${styles.action_run_sys!} ${styles.action_add!}" />
          <@menuitem type="link" href=makePageUrl("FindQuoteForCart") text=uiLabelMap.OrderOrderQuotes class="+${styles.action_nav!}" />
        </#if>
        <#if shoppingCart.getOrderType() == "SALES_ORDER">
          <@menuitem type="link" href=makePageUrl("createCustRequestFromCart?destroyCart=Y") text=uiLabelMap.OrderCreateCustRequestFromCart class="+${styles.action_run_sys!} ${styles.action_add!}" />
        </#if>
        <@menuitem type="link" href=makeServerUrl("/partymgr/control/findparty?${externalKeyParam!}") text=uiLabelMap.PartyFindParty class="+${styles.action_nav!} ${styles.action_find!}" />
        <#if shoppingCart.getOrderType() == "SALES_ORDER">
          <@menuitem type="link" href=makePageUrl("setCustomer") text=uiLabelMap.PartyCreateNewCustomer class="+${styles.action_nav!} ${styles.action_add!}" />
        </#if>
        <@menuitem type="link" href=makePageUrl("checkinits") text=uiLabelMap.PartyChangeParty class="+${styles.action_nav!} ${styles.action_update!}" />
        <#if security.hasEntityPermission("CATALOG", "_CREATE", request)>
           <@menuitem type="link" href=makeServerUrl("/catalog/control/ViewProduct?${externalKeyParam!}") target="catalog" text=uiLabelMap.ProductCreateNewProduct class="+${styles.action_nav!} ${styles.action_add!}" />
        </#if>
        <@menuitem type="link" href=makePageUrl("quickadd") text=uiLabelMap.OrderQuickAdd class="+${styles.action_nav!} ${styles.action_add!}" />
        <#if shoppingLists??>
          <@menuitem type="link" href=makePageUrl("viewPartyShoppingLists?partyId=${partyId}") text=uiLabelMap.PageTitleShoppingList class="+${styles.action_nav!}" />
        </#if>
    </@menu>
</@section>
