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
<#include "ordercommon.ftl">

<#if orderHeader?has_content>

<#-- SCIPIO: Moved to page title: <@heading>${uiLabelMap.EcommerceOrderConfirmation}</@heading>-->
<p>${uiLabelMap.ShopThankYouForOrder}</p>
<#assign printable = printable!false>
<#if !isDemoStore?? || isDemoStore>
  <#if printable>
    <p>${uiLabelMap.OrderDemoFrontNote}.</p>
  <#else>
    <@alert type="info">${uiLabelMap.OrderDemoFrontNote}.</@alert>
  </#if>
</#if>

  <@render resource="component://shop/widget/OrderScreens.xml#orderheader" />
  <@render resource="component://shop/widget/OrderScreens.xml#orderitems" />
  <#if !printable>
    <@menu type="button">
      <@menuitem type="link" href=makeOfbizUrl("main") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.EcommerceContinueShopping />
    </@menu>
  </#if>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderSpecifiedNotFound}.</@commonMsg>
</#if>
