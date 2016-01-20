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

<#-- TODO: Convert hard-coded text to UI label properties -->

<#if security.hasEntityPermission("PAYPROC", "_VIEW", session)>

    <#macro menuContent menuArgs={}>
      <@menu type="button" class="+button-style-1" args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("paysetup") text="Payment Setup" selected=true />
      </@menu>
    </#macro>
    <@section title="Payment Processor Setup" menuContent=menuContent>
        <#if paymentSetups?has_content> 
          <@table type="data-list" autoAltRows=false width="100%" cellpadding="2" cellspacing="2" border="0">
            <@thead>
              <@tr class="header-row">
                <@th nowrap="nowrap">WebSite</@th>
                <@th nowrap="nowrap">PayMethod Type</@th>
                <@th nowrap="nowrap">Auth Service</@th>
                <@th nowrap="nowrap">Re-Auth Service</@th>
                <@th nowrap="nowrap">Capture Service</@th>
                <@th nowrap="nowrap">Refund Service</@th>
                <@th nowrap="nowrap">Payment Config</@th>
                <@th nowrap="nowrap">&nbsp;</@th>
              </@tr>
            </@thead>
            <@tbody>
                <#list paymentSetups as paymentSetting>
                  <#if rowStyle?? && rowStyle == "alternate-row">
                    <#assign rowStyle = "alternate-rowSelected">
                  <#else>
                    <#assign rowStyle = "alternate-row">
                  </#if>
                  <@tr alt=true selected=(rowStyle == "alternate-rowSelected")>
                    <@td>${paymentSetting.siteName!}</@td>
                    <@td>${paymentSetting.description!}</@td>
                    <@td>${paymentSetting.paymentAuthService!}</@td>
                    <@td>${paymentSetting.paymentReAuthService!}</@td>
                    <@td>${paymentSetting.paymentCaptureService!}</@td>
                    <@td>${paymentSetting.paymentRefundService!}</@td>
                    <@td>${paymentSetting.paymentConfiguration!}</@td>
                    <@td nowrap="nowrap">&nbsp;
                      <#if security.hasEntityPermission("PAYPROC", "_UPDATE", session)>
                        <a href="<@ofbizUrl>paysetup?webSiteId=${paymentSetting.webSiteId!}&amp;paymentMethodTypeId=${paymentSetting.paymentMethodTypeId!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a>&nbsp;
                      </#if>
                      <#if security.hasEntityPermission("PAYPROC", "_DELETE", session)>
                        <a href="<@ofbizUrl>removeWebSitePaymentSetting?webSiteId=${paymentSetting.webSiteId!}&amp;paymentMethodTypeId=${paymentSetting.paymentMethodTypeId!}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>&nbsp;
                      </#if>
                      </@td>
                  </@tr>
                </#list>
            </@tbody>
          </@table>
        <#else>
            <@resultMsg>No settings found.</@resultMsg>
        </#if>
    </@section>

<#if security.hasEntityPermission("PAYPROC", "_CREATE", session)>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
      <#if webSitePayment?has_content>
        <@menuitem type="link" href=makeOfbizUrl("paysetup") text="Add New" />
      </#if>
      </@menu>
    </#macro>
    <#if webSitePayment?has_content>
      <#assign sectionTitle = "Update Setting">
    <#else>
      <#assign sectionTitle = "Add New Setting">
    </#if>
    <@section title=sectionTitle menuContent=menuContent>
            <#if webSitePayment?has_content>
              <form method="post" action="<@ofbizUrl>updateWebSitePaymentSetting</@ofbizUrl>">
            <#else>
              <form method="post" action="<@ofbizUrl>createWebSitePaymentSetting</@ofbizUrl>">
            </#if>
              <@field type="generic" label="WebSite">
                  <#if webSitePayment?has_content>
                    <input type='hidden' name='webSiteId' value='${webSitePayment.webSiteId}' />
                    <div>
                      <b>${webSitePayment.siteName}</b> (This cannot be changed without re-creating the setting.)
                    </div>
                  <#else>
                    <select name="webSiteId">
                      <#list webSites as nextWebSite>
                        <option value='${nextWebSite.webSiteId}'>${nextWebSite.siteName}</option>
                      </#list>
                    </select>
                  </#if>
              </@field>
              <@field type="generic" label="Payment Method Type">
                  <#if webSitePayment?has_content>
                    <input type='hidden' name='paymentMethodTypeId' value='${webSitePayment.paymentMethodTypeId}' />
                    <div>
                      <b>${webSitePayment.description}</b> (This cannot be changed without re-creating the setting.)
                    </div>
                  <#else>
                    <select name="paymentMethodTypeId">
                      <#list paymentMethodTypes as nextPayType>
                        <option value='${nextPayType.paymentMethodTypeId}'>${nextPayType.description}</option>
                      </#list>
                    </select>
                  </#if>
              </@field>

              <@field type="generic" label="Processor Auth Service">
                  <input type="text" name="paymentAuthService" value="${payInfo.paymentAuthService!}" size="30" maxlength="60" />
              </@field>
              <@field type="generic" label="Processor Re-Auth Service">
                  <input type="text" name="paymentReAuthService" value="${payInfo.paymentReAuthService!}" size="30" maxlength="60" />
              </@field>
              <@field type="generic" label="Processor Capture Service">
                  <input type="text" name="paymentCaptureService" value="${payInfo.paymentCaptureService!}" size="30" maxlength="60" />
              </@field>
              <@field type="generic" label="Processor Refund Service">
                  <input type="text" name="paymentRefundService" value="${payInfo.paymentRefundService!}" size="30" maxlength="60" />
              </@field>
              <@field type="generic" label="Processor Properties URL">
                  <input type="text" name="paymentConfiguration" value="${payInfo.paymentConfiguration!}" size="30" maxlength="60" />
              </@field>
              <@field type="submitarea">
                  <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}" />
              </@field>
            </form>
    </@section>
</#if>

<#else>
  <@alert type="error">You do not have permission to view this page. ("PAYSETUP_VIEW" or "PAYSETUP_ADMIN" needed)</@alert>
</#if>
