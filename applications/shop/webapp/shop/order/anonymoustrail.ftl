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

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<#-- SCIPIO: NOTE: this omits the top @menu; the including template must provide its own -->
<#if shipAddr??>
  <#if anontrailMenuArgs?has_content>
    <#--<p><@objectAsScript lang="raw" object=anontrailMenuArgs /></p>-->
    <#-- SCIPIO: WARN: Although @menu will in theory support args maps from groovy context, at current
        time it is not well tested and could be a source of errors here... -->
    <@menu args=anontrailMenuArgs>
      <@menuitem type="link" href=makePageUrl("setShipping") class="+${styles.action_nav!} ${trailClass.shipAddr}" text=uiLabelMap.EcommerceChangeShippingAddress />
      <#if shipOptions??>
        <@menuitem type="link" href=makePageUrl("setShipOptions") class="+${styles.action_nav!} ${trailClass.shipOptions}" text=uiLabelMap.EcommerceChangeShippingOptions />
        <#if billing??>
          <@menuitem type="link" href=makePageUrl("setBilling?resetType=Y") class="+${styles.action_nav!} ${trailClass.paymentType}" text=uiLabelMap.EcommerceChangePaymentInfo />
        </#if>
      </#if>
    </@menu>
  <#else>
      <@menuitem type="link" href=makePageUrl("setShipping") class="+${styles.action_nav!} ${trailClass.shipAddr}" text=uiLabelMap.EcommerceChangeShippingAddress />
      <#if shipOptions??>
        <@menuitem type="link" href=makePageUrl("setShipOptions") class="+${styles.action_nav!} ${trailClass.shipOptions}" text=uiLabelMap.EcommerceChangeShippingOptions />
        <#if billing??>
          <@menuitem type="link" href=makePageUrl("setBilling?resetType=Y") class="+${styles.action_nav!} ${trailClass.paymentType}" text=uiLabelMap.EcommerceChangePaymentInfo />
        </#if>
      </#if>
  </#if>
</#if>
