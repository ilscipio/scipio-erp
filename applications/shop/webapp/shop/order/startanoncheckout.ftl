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
            <@menuitem type="link" href=makePageUrl("anoncheckoutoptions") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderCheckout />
            <#-- SCIPIO: TODO: Hopefully can remove link to the old one when this is over... -->
            <@menuitem type="link" href=makePageUrl("setCustomer") class="+${styles.action_run_session!} ${styles.action_continue!}" text="${rawLabel('OrderCheckout')} (Old - Deprecated)" />
            <#--<@menuitem type="link" href=makePageUrl("quickAnonCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}"  text=uiLabelMap.OrderCheckoutQuick />-->
            <@menuitem type="link" href=makePageUrl("anonOnePageCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}"  text=uiLabelMap.EcommerceOnePageCheckout />
          </@menu>
      </@section> 
    </@cell>
  </@row>
</#if>

