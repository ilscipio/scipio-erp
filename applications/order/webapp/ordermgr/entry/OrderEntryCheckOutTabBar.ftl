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

<#if stepTitleId??>
    <#assign stepTitle = uiLabelMap.get(stepTitleId)>
</#if>

<#assign sectionTitle>
    <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        ${uiLabelMap.OrderPurchaseOrder}
    <#else>
        ${uiLabelMap.OrderSalesOrder}
    </#if>
    :&nbsp;${stepTitle!}
</#assign>

<@section title=sectionTitle>
    <@menu type="button">
      <#list checkoutSteps?reverse as checkoutStep>
        <#assign stepUiLabel = uiLabelMap.get(checkoutStep.label)>
        <#if checkoutStep.enabled == "N">
            <@menuitem type="link" text=stepUiLabel disabled=true class="+${styles.action_nav!}" />
        <#else>
            <@menuitem type="link" href=makeOfbizUrl("${checkoutStep.uri}") text=stepUiLabel class="+${styles.action_nav!}" />
        </#if>
      </#list>
      <#if isLastStep == "N">
        <@menuitem type="link" href="javascript:document.checkoutsetupform.submit();" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" />
      <#else>
        <@menuitem type="link" href=makeOfbizUrl("processorder") text=uiLabelMap.OrderCreateOrder class="+${styles.action_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
      </#if>
    </@menu>
</@section>
