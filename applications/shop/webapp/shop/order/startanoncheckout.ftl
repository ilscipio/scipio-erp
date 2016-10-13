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

<#assign shoppingCart = sessionAttributes.shoppingCart!>
<#if shoppingCart?has_content>
    <#assign shoppingCartSize = shoppingCart.size()>
<#else>
    <#assign shoppingCartSize = 0>
</#if>

<#if (shoppingCartSize > 0)>
  <#assign sectionTitle>${uiLabelMap.CommonOr?upper_case} ${uiLabelMap.CommonCheckoutAnonymous}</#assign>
  <@row>
    <@cell>
      <@section title=sectionTitle>
          <@menu type="button">
            <@menuitem type="link" href=makeOfbizUrl("anoncheckoutoptions") class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderCheckout />
            <#-- Scipio: TODO: Hopefully can remove link to the old one when this is over... -->
            <@menuitem type="link" href=makeOfbizUrl("setCustomer") class="+${styles.action_run_session!} ${styles.action_continue!}" text="${rawLabel('OrderCheckout')} (Old - Deprecated)" />
            <#--<@menuitem type="link" href=makeOfbizUrl("quickAnonCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}"  text=uiLabelMap.OrderCheckoutQuick />-->
            <@menuitem type="link" href=makeOfbizUrl("anonOnePageCheckout") class="+${styles.action_run_session!} ${styles.action_continue!}"  text=uiLabelMap.EcommerceOnePageCheckout />
          </@menu>
      </@section> 
    </@cell>
  </@row>
</#if>

