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

<#if security.hasEntityPermission("ORDERMGR", "_SEND_CONFIRMATION", session)>
  <@section title="${uiLabelMap.OrderSendConfirmationEmail}">
    <@row>
      <@cell columns=6>
    
      <#macro actionMenu>
        <@menu type="button">
          <@menuitem type="link" href=makeOfbizUrl("authview/${donePage}?orderId=${orderId}") text="${uiLabelMap.CommonGoBack}" class="+${styles.action_nav!} ${styles.action_cancel!}" />
          <@menuitem type="link" href="javascript:document.sendConfirmationForm.submit()" text="${uiLabelMap.CommonSend}" class="+${styles.action_run_sys!} ${styles.action_send!}" />
        </@menu>
      </#macro>
      
      <@actionMenu />
      
      <form method="post" action="<@ofbizUrl>sendconfirmationmail/${donePage}</@ofbizUrl>" name="sendConfirmationForm">
        <input type="hidden" name="orderId" value="${orderId!}" />
        <#if ! productStoreEmailSetting??>
            <#assign productStoreEmailSetting = {} />
        </#if>
        <input type="hidden" name="partyId" value="${partyId!}" />
        <input type="hidden" name="contentType" value="${productStoreEmailSetting.contentType?default("")}" />
        <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderSendConfirmationEmailSubject}&nbsp;</@td>
                <@td>
                    <input type="text" size="40" name="subject" value="${productStoreEmailSetting.subject?default(uiLabelMap.OrderOrderConfirmation + " " + uiLabelMap.OrderNbr + orderId)?replace("\\$\\{orderId\\}",orderId,"r")}" />
                </@td>
            </@tr>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderSendConfirmationEmailSendTo}&nbsp;</@td>
                <@td>
                    <input type="text" size="40" name="sendTo" value="${sendTo}"/>
                </@td>
            </@tr>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderSendConfirmationEmailCCTo}&nbsp;</@td>
                <@td>
                    <input type="text" size="40" name="sendCc" value="${productStoreEmailSetting.ccAddress?default("")}" />
                </@td>
            </@tr>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderSendConfirmationEmailBCCTo}&nbsp;</@td>
                <@td>
                    <input type="text" size="40" name="sendBcc" value="${productStoreEmailSetting.bccAddress?default("")}" />
                </@td>
            </@tr>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonFrom}&nbsp;</@td>
                <@td>
                    <#if productStoreEmailSetting.fromAddress??>
                        <input type="hidden" name="sendFrom" value="${productStoreEmailSetting.fromAddress}" />
                    <#else>
                        <input type="text" size="40" name="sendFrom" value="" />
                    </#if>
                </@td>
            </@tr>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderSendConfirmationEmailContentType}&nbsp;</@td>
                <@td>${productStoreEmailSetting.contentType?default("text/html")}</@td>
            </@tr>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderSendConfirmationEmailBody}&nbsp;</@td>
                <@td>
                    <textarea name="body" rows="30" cols="80">${screens.render(productStoreEmailSetting.bodyScreenLocation?default(""))}</textarea>
                </@td>
            </@tr>
        </@table>
      </form>
      
      <@actionMenu />
      
      </@cell>
    </@row>
  </@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>