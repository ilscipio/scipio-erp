<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<#-- SCIPIO: NOTE: this omits the top @menu; the including template must provide its own -->
<#if shipAddr??>
  <#if anontrailMenuArgs?has_content>
    <#--<p><@objectAsScript lang="raw" object=anontrailMenuArgs /></p>-->
    <#-- SCIPIO: WARN: Although @menu will in theory support args maps from groovy context, at current
        time it is not well tested and could be a source of errors here... -->
    <@menu args=anontrailMenuArgs>
      <@menuitem type="link" href=makeOfbizUrl("setShipping") class="+${styles.action_nav!} ${trailClass.shipAddr}" text=uiLabelMap.EcommerceChangeShippingAddress />
      <#if shipOptions??>
        <@menuitem type="link" href=makeOfbizUrl("setShipOptions") class="+${styles.action_nav!} ${trailClass.shipOptions}" text=uiLabelMap.EcommerceChangeShippingOptions />
        <#if billing??>
          <@menuitem type="link" href=makeOfbizUrl("setBilling?resetType=Y") class="+${styles.action_nav!} ${trailClass.paymentType}" text=uiLabelMap.EcommerceChangePaymentInfo />
        </#if>
      </#if>
    </@menu>
  <#else>
      <@menuitem type="link" href=makeOfbizUrl("setShipping") class="+${styles.action_nav!} ${trailClass.shipAddr}" text=uiLabelMap.EcommerceChangeShippingAddress />
      <#if shipOptions??>
        <@menuitem type="link" href=makeOfbizUrl("setShipOptions") class="+${styles.action_nav!} ${trailClass.shipOptions}" text=uiLabelMap.EcommerceChangeShippingOptions />
        <#if billing??>
          <@menuitem type="link" href=makeOfbizUrl("setBilling?resetType=Y") class="+${styles.action_nav!} ${trailClass.paymentType}" text=uiLabelMap.EcommerceChangePaymentInfo />
        </#if>
      </#if>
  </#if>
</#if>
