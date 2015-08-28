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
  <ul class="${styles.menu_button!} button-style-1">
    <li><a href="<@ofbizUrl>paysetup</@ofbizUrl>" class="${styles.menu_button_itemlink!} selected">Payment&nbsp;Setup</a></li>
  </ul>
</#if>

<#if security.hasEntityPermission("PAYPROC", "_VIEW", session)>
<@table type="fields" autoAltRows=false border="0" width='100%' cellpadding='0' cellspacing=0 class='boxoutside'>
  <@tr>
    <@td width='100%'>
      <@table type="fields" width='100%' border='0' cellpadding='0' cellspacing='0' class='boxtop'>
        <@tr>
          <@td>
            <div class='boxhead'>&nbsp;Payment Processor Setup</div>
          </@td>
        </@tr>
      </@table>
    </@td>
  </@tr>
  <@tr>
    <@td width='100%'>
      <@table type="fields" autoAltRows=false width='100%' border='0' cellpadding='0' cellspacing='0' class='boxbottom'>
        <@tr>
          <@td>
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
              <#if paymentSetups?has_content>
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
                        <a href="<@ofbizUrl>paysetup?webSiteId=${paymentSetting.webSiteId!}&amp;paymentMethodTypeId=${paymentSetting.paymentMethodTypeId!}</@ofbizUrl>" class="${styles.button_default!}">Edit</a>&nbsp;
                        </#if>
                        <#if security.hasEntityPermission("PAYPROC", "_DELETE", session)>
                        <a href="<@ofbizUrl>removeWebSitePaymentSetting?webSiteId=${paymentSetting.webSiteId!}&amp;paymentMethodTypeId=${paymentSetting.paymentMethodTypeId!}</@ofbizUrl>" class="${styles.button_default!}">Remove</a>&nbsp;
                        </#if>
                      </@td>
                  </@tr>
                </#list>
              <#else>
                <@tr metaRow=true>
                  <@td colspan="8"><@resultMsg>No settings found.</@resultMsg></@td>
                </@tr>
              </#if>
            </@table>
          </@td>
        </@tr>
      </@table>
    </@td>
  </@tr>
</@table>

<#if security.hasEntityPermission("PAYPROC", "_CREATE", session)>
<@table type="fields" border="0" width='100%' cellpadding='0' cellspacing=0 class='boxoutside'>
  <@tr>
    <@td width='100%'>
      <@table type="fields" width='100%' border='0' cellpadding='0' cellspacing='0' class='boxtop'>
        <@tr>
          <@td width='90%'>
            <#if webSitePayment?has_content>
              <div class='boxhead'>&nbsp;Update&nbsp;Setting</div>
            <#else>
              <div class='boxhead'>&nbsp;Add&nbsp;New&nbsp;Setting</div>
            </#if>
          </@td>
          <#if webSitePayment?has_content>
            <@td align='right' width='10%'><a href="<@ofbizUrl>paysetup</@ofbizUrl>" class="${styles.button_default!}">Add New</a></@td>
          <#else>
            <@td align='right' width='10%'></@td>
          </#if>
        </@tr>
      </@table>
    </@td>
  </@tr>
  <@tr>
    <@td width='100%'>
      <@table type="fields" width='100%' border='0' cellpadding='0' cellspacing='0' class='boxbottom'>
        <@tr>
          <@td>
            <#if webSitePayment?has_content>
              <form method="post" action="<@ofbizUrl>updateWebSitePaymentSetting</@ofbizUrl>">
            <#else>
              <form method="post" action="<@ofbizUrl>createWebSitePaymentSetting</@ofbizUrl>">
            </#if>
            <@table type="fields" border='0' cellpadding='2' cellspacing='0'>
              <@tr>
                <@td width="26%" align="right">WebSite</@td>
                <@td>&nbsp;</@td>
                <@td width="74%">
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
                </@td>
              </@tr>
              <@tr>
                <@td width="26%" align="right">Payment Method Type</@td>
                <@td>&nbsp;</@td>
                <@td width="74%">
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
                </@td>
              </@tr>

              <@tr>
                <@td width="26%" align="right">Processor Auth Service</@td>
                <@td>&nbsp;</@td>
                <@td width="74%"><input type="text" name="paymentAuthService" value="${payInfo.paymentAuthService!}" size="30" maxlength="60" /></@td>
              </@tr>
              <@tr>
                <@td width="26%" align="right">Processor Re-Auth Service</@td>
                <@td>&nbsp;</@td>
                <@td width="74%"><input type="text" name="paymentReAuthService" value="${payInfo.paymentReAuthService!}" size="30" maxlength="60" /></@td>
              </@tr>
              <@tr>
                <@td width="26%" align="right">Processor Capture Service</@td>
                <@td>&nbsp;</@td>
                <@td width="74%"><input type="text" name="paymentCaptureService" value="${payInfo.paymentCaptureService!}" size="30" maxlength="60" /></@td>
              </@tr>
              <@tr>
                <@td width="26%" align="right">Processor Refund Service</@td>
                <@td>&nbsp;</@td>
                <@td width="74%"><input type="text" name="paymentRefundService" value="${payInfo.paymentRefundService!}" size="30" maxlength="60" /></@td>
              </@tr>
              <@tr>
                <@td width="26%" align="right">Processor Properties URL</@td>
                <@td>&nbsp;</@td>
                <@td width="74%"><input type="text" name="paymentConfiguration" value="${payInfo.paymentConfiguration!}" size="30" maxlength="60" /></@td>
              </@tr>
              <@tr>
                <@td colspan='2'>&nbsp;</@td>
                <@td colspan='1'><input type="submit" value="${uiLabelMap.CommonUpdate}" /></@td>
              </@tr>
            </@table>
            </form>
          </@td>
        </@tr>
      </@table>
    </@td>
  </@tr>
</@table>
</#if>

<#else>
  <@alert type="error">You do not have permission to view this page. ("PAYSETUP_VIEW" or "PAYSETUP_ADMIN" needed)</@alert>
</#if>
