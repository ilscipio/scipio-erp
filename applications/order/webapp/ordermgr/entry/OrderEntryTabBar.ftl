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
<@section>   
      <@menu type="button">
      <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        <#if shoppingCart.getOrderPartyId() == "_NA_" || (shoppingCart.size() == 0)>
          <@menuitem type="link" text=uiLabelMap.OrderFinalizeOrder disabled=true class="+${styles.action_nav!} ${styles.action_complete!}"/>
        <#else>
          <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=purchase&finalizeReqCustInfo=false&finalizeReqShipInfo=false&finalizeReqOptions=false&finalizeReqPayInfo=false") text=uiLabelMap.OrderFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" />
        </#if>
      <#else>
        <#if shoppingCart.size() == 0>
          <@menuitem type="link" text=uiLabelMap.OrderQuickFinalizeOrder disabled=true class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" text=uiLabelMap.OrderFinalizeOrderDefault disabled=true class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" text=uiLabelMap.OrderFinalizeOrder disabled=true class="+${styles.action_nav!} ${styles.action_complete!}" />
        <#else>
          <@menuitem type="link" href=makeOfbizUrl("quickcheckout") text=uiLabelMap.OrderQuickFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=default") text=uiLabelMap.OrderFinalizeOrderDefault class="+${styles.action_nav!} ${styles.action_complete!}" />
          <@menuitem type="link" href=makeOfbizUrl("finalizeOrder?finalizeMode=init") text=uiLabelMap.OrderFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" />
        </#if>
      </#if>

      <#if (shoppingCart.size() > 0)>
        <@menuitem type="link" href="javascript:document.cartform.submit()" text=uiLabelMap.OrderRecalculateOrder class="+${styles.action_run_session!} ${styles.action_update!}" />
        <@menuitem type="link" href="javascript:removeSelected();" text=uiLabelMap.OrderRemoveSelected class="+${styles.action_run_session!} ${styles.action_remove!}" />
      <#else>
        <@menuitem type="link" text=uiLabelMap.OrderRecalculateOrder disabled=true class="+${styles.action_run_session!} ${styles.action_update!}" />
        <@menuitem type="link" text=uiLabelMap.OrderRemoveSelected disabled=true class="+${styles.action_run_session!} ${styles.action_remove!}" />
      </#if>
      <@menuitem type="link" href=makeOfbizUrl("emptycart") text=uiLabelMap.OrderClearOrder class="+${styles.action_run_session!} ${styles.action_clear!}" />
    </@menu>
</@section>
