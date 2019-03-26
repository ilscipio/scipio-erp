<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- TODO: Convert hard-coded text to UI label properties -->

<#if security.hasEntityPermission("PAYPROC", "_VIEW", request)>

    <#macro menuContent menuArgs={}>
      <@menu type="button" class="+button-style-1" args=menuArgs>
        <@menuitem type="link" href=makePageUrl("paysetup") text="Payment Setup" selected=true class="+${styles.action_nav!} ${styles.action_configure!}" />
      </@menu>
    </#macro>
    <@section title="Payment Processor Setup" menuContent=menuContent>
        <#if paymentSetups?has_content> 
          <@table type="data-list" autoAltRows=false width="100%" class="+${styles.table_spacing_small_hint!}">
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
                      <#if security.hasEntityPermission("PAYPROC", "_UPDATE", request)>
                        <a href="<@pageUrl>paysetup?webSiteId=${paymentSetting.webSiteId!}&amp;paymentMethodTypeId=${paymentSetting.paymentMethodTypeId!}</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a>&nbsp;
                      </#if>
                      <#if security.hasEntityPermission("PAYPROC", "_DELETE", request)>
                        <a href="<@pageUrl>removeWebSitePaymentSetting?webSiteId=${paymentSetting.webSiteId!}&amp;paymentMethodTypeId=${paymentSetting.paymentMethodTypeId!}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>&nbsp;
                      </#if>
                      </@td>
                  </@tr>
                </#list>
            </@tbody>
          </@table>
        <#else>
            <@commonMsg type="result-norecord">No settings found.</@commonMsg>
        </#if>
    </@section>

<#if security.hasEntityPermission("PAYPROC", "_CREATE", request)>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
      <#if webSitePayment?has_content>
        <@menuitem type="link" href=makePageUrl("paysetup") text="Add New" class="+${styles.action_nav!} ${styles.action_add!}" />
      </#if>
      </@menu>
    </#macro>
    <#if webSitePayment?has_content>
      <#assign sectionTitle = "Update Setting">
    <#else>
      <#assign sectionTitle = "Add New Setting">
    </#if>
    <@section title=sectionTitle menuContent=menuContent>
        <#assign formAction><#if webSitePayment?has_content><@pageUrl escapeAs='html'>updateWebSitePaymentSetting</@pageUrl><#else><@pageUrl escapeAs='html'>createWebSitePaymentSetting</@pageUrl></#if></#assign>
        <form method="post" action="${formAction}">

        <#if webSitePayment?has_content>
          <input type="hidden" name="webSiteId" value="${webSitePayment.webSiteId}" />
          <@field type="display" label="WebSite">
              <div>
                <b>${webSitePayment.siteName}</b> (This cannot be changed without re-creating the setting.)
              </div>
          </@field>
        <#else>
          <@field type="select" name="webSiteId" label="WebSite">
            <#list webSites as nextWebSite>
              <option value="${nextWebSite.webSiteId}">${nextWebSite.siteName}</option>
            </#list>
          </@field>
        </#if>
        <#if webSitePayment?has_content>
          <input type="hidden" name="paymentMethodTypeId" value="${webSitePayment.paymentMethodTypeId}" />
          <@field type="display" label="Payment Method Type">
              <div>
                <b>${webSitePayment.description}</b> (This cannot be changed without re-creating the setting.)
              </div>
          </@field>
        <#else>
          <@field type="select" name="paymentMethodTypeId" label="Payment Method Type">
            <#list paymentMethodTypes as nextPayType>
              <option value="${nextPayType.paymentMethodTypeId}">${nextPayType.description}</option>
            </#list>
          </@field>
        </#if>

          <@field type="input" label="Processor Auth Service" name="paymentAuthService" value=(payInfo.paymentAuthService!) size="30" maxlength="60" />
          <@field type="input" label="Processor Re-Auth Service" name="paymentReAuthService" value=(payInfo.paymentReAuthService!) size="30" maxlength="60" />
          <@field type="input" label="Processor Capture Service" name="paymentCaptureService" value=(payInfo.paymentCaptureService!) size="30" maxlength="60" />
          <@field type="input" label="Processor Refund Service" name="paymentRefundService" value=(payInfo.paymentRefundService!) size="30" maxlength="60" />
          <@field type="input" label="Processor Properties URL" name="paymentConfiguration" value=(payInfo.paymentConfiguration!) size="30" maxlength="60" />

          <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
        </form>
    </@section>
</#if>

<#else>
  <@commonMsg type="error">You do not have permission to view this page. ("PAYSETUP_VIEW" or "PAYSETUP_ADMIN" needed)</@commonMsg>
</#if>
