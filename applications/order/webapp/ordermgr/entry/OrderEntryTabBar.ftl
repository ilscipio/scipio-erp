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
<@section>   
    <@menu type="button">
      <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        <@menuitem type="link" href=makePageUrl("finalizeOrder?finalizeMode=purchase&finalizeReqCustInfo=false&finalizeReqShipInfo=false&finalizeReqOptions=false&finalizeReqPayInfo=false") text=uiLabelMap.OrderFinalizeOrder 
          class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.getOrderPartyId() == "_NA_" || (shoppingCart.size() == 0))/>
      <#else>
        <@menuitem type="link" href=makePageUrl("quickcheckout") text=uiLabelMap.OrderQuickFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.size() == 0)/>
        <@menuitem type="link" href=makePageUrl("finalizeOrder?finalizeMode=init") text=uiLabelMap.OrderFinalizeOrder class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.size() == 0)/>
        <@menuitem type="link" href=makePageUrl("finalizeOrder?finalizeMode=default") text=uiLabelMap.OrderFinalizeOrderDefault class="+${styles.action_nav!} ${styles.action_complete!}" disabled=(shoppingCart.size() == 0)/>
      </#if>
      <@menuitem type="link" href="javascript:document.cartform.submit()" text=uiLabelMap.OrderRecalculateOrder class="+${styles.action_run_session!} ${styles.action_update!}" disabled=(shoppingCart.size() == 0)/>
      <@menuitem type="link" href="javascript:removeSelected();" text=uiLabelMap.OrderRemoveSelected class="+${styles.action_run_session!} ${styles.action_remove!}" disabled=(shoppingCart.size() == 0)/>
      <@menuitem type="link" href=makePageUrl("emptycart") text=uiLabelMap.OrderClearOrder class="+${styles.action_run_session!} ${styles.action_clear!}" />
    </@menu>
</@section>
