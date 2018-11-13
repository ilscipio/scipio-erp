<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#assign shoppingCart = sessionAttributes.shoppingCart!>

<@section title=uiLabelMap.OrderOrderShortcuts>
    <@menu type="button">
        <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
          <@menuitem type="link" href=makeOfbizUrl("RequirementsForSupplier") text=uiLabelMap.OrderRequirements class="+${styles.action_nav!}" />
        </#if>
        <#if shoppingCart.getOrderType()?has_content && shoppingCart.items()?has_content>
          <@menuitem type="link" href=makeOfbizUrl("createQuoteFromCart?destroyCart=Y") text=uiLabelMap.OrderCreateQuoteFromCart class="+${styles.action_run_sys!} ${styles.action_add!}" />
          <@menuitem type="link" href=makeOfbizUrl("FindQuoteForCart") text=uiLabelMap.OrderOrderQuotes class="+${styles.action_nav!}" />
        </#if>
        <#if shoppingCart.getOrderType() == "SALES_ORDER">
          <@menuitem type="link" href=makeOfbizUrl("createCustRequestFromCart?destroyCart=Y") text=uiLabelMap.OrderCreateCustRequestFromCart class="+${styles.action_run_sys!} ${styles.action_add!}" />
        </#if>
        <@menuitem type="link" href=makeOfbizInterWebappUrl("/partymgr/control/findparty?${externalKeyParam!}") text=uiLabelMap.PartyFindParty class="+${styles.action_nav!} ${styles.action_find!}" />
        <#if shoppingCart.getOrderType() == "SALES_ORDER">
          <@menuitem type="link" href=makeOfbizUrl("setCustomer") text=uiLabelMap.PartyCreateNewCustomer class="+${styles.action_nav!} ${styles.action_add!}" />
        </#if>
        <@menuitem type="link" href=makeOfbizUrl("checkinits") text=uiLabelMap.PartyChangeParty class="+${styles.action_nav!} ${styles.action_update!}" />
        <#if security.hasEntityPermission("CATALOG", "_CREATE", session)>
           <@menuitem type="link" href=makeOfbizInterWebappUrl("/catalog/control/ViewProduct?${externalKeyParam!}") target="catalog" text=uiLabelMap.ProductCreateNewProduct class="+${styles.action_nav!} ${styles.action_add!}" />
        </#if>
        <@menuitem type="link" href=makeOfbizUrl("quickadd") text=uiLabelMap.OrderQuickAdd class="+${styles.action_nav!} ${styles.action_add!}" />
        <#if shoppingLists??>
          <@menuitem type="link" href=makeOfbizUrl("viewPartyShoppingLists?partyId=${partyId}") text=uiLabelMap.PageTitleShoppingList class="+${styles.action_nav!}" />
        </#if>
    </@menu>
</@section>
