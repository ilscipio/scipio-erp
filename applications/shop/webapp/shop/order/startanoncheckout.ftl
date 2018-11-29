<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: Must use context or accessor
<#assign shoppingCart = sessionAttributes.shoppingCart!>-->
<#assign shoppingCart = getShoppingCart()!>
<#if shoppingCart?has_content>
    <#assign shoppingCartSize = shoppingCart.size()>
<#else>
    <#assign shoppingCartSize = 0>
</#if>

<#if (shoppingCartSize > 0)>
  <#assign sectionTitle>${rawLabel('CommonOr')?upper_case} ${rawLabel('CommonCheckoutAnonymous')}</#assign>
  <@row>
    <@cell>
      <@section title=sectionTitle>
          <@menu type="button">
            <@menuitem type="link" href=makeOfbizUrl("anoncheckoutoptions") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderCheckout />
            <#-- SCIPIO: TODO: Hopefully can remove link to the old one when this is over... -->
            <@menuitem type="link" href=makeOfbizUrl("setCustomer") class="+${styles.action_run_session!} ${styles.action_continue!}" text="${rawLabel('OrderCheckout')} (Old - Deprecated)" />
            <#--<@menuitem type="link" href=makeOfbizUrl("quickAnonCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}"  text=uiLabelMap.OrderCheckoutQuick />-->
            <@menuitem type="link" href=makeOfbizUrl("anonOnePageCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}"  text=uiLabelMap.EcommerceOnePageCheckout />
          </@menu>
      </@section> 
    </@cell>
  </@row>
</#if>

