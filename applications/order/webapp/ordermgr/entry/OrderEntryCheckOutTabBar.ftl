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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<#if stepTitleId??>
    <#assign stepTitle = uiLabelMap[stepTitleId]>
</#if>

<@section title=rawLabel((shoppingCart.getOrderType() == "PURCHASE_ORDER")?then('OrderPurchaseOrder', 'OrderSalesOrder'))+": "+raw(stepTitle!)>
    <@menu type="button">
      <#-- SCIPIO: Why reverse this? Silly
      <#list checkoutSteps?reverse as checkoutStep>-->
      <#list checkoutSteps as checkoutStep>
        <@menuitem type="link" href=makePageUrl("${checkoutStep.uri}") text=uiLabelMap[checkoutStep.label] class="+${styles.action_nav!}" disabled=(checkoutStep.enabled == "N") />
      </#list>

      <#if "quick" == checkoutType!><#-- SCIPIO: new -->
        <#if isLastStep == "N">
          <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'DN', '');" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" />
        <#else>
          <@menuitem type="link" href=makePageUrl("processorder") text=uiLabelMap.OrderCreateOrder class="+${styles.action_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
        </#if>
      <#else>
        <#if isLastStep == "N">
          <@menuitem type="link" href="javascript:document.checkoutsetupform.submit();" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" />
        <#else>
          <@menuitem type="link" href=makePageUrl("processorder") text=uiLabelMap.OrderCreateOrder class="+${styles.action_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
        </#if>
      </#if>
    </@menu>
</@section>
