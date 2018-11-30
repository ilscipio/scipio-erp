<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#assign username = requestParameters.USERNAME!(autoUserLogin.userLoginId)!""><#-- SCIPIO: Don't use sessionAttributes here -->
<#assign tenantId = requestParameters.userTenantId!>

<@row>
<@cell class="${styles.grid_large!}3 ${styles.grid_large!}centered login-box" last=true id="login">
  <div id="login-box-title">
    <@heading level=1>${uiLabelMap.CommonPasswordChange}</@heading>
  </div>
  <@section id="login-box-content">
    <form method="post" action="<@ofbizUrl>login</@ofbizUrl>" name="loginform">
      <input type="hidden" name="requirePasswordChange" value="Y"/>
      <input type="hidden" name="USERNAME" value="${username}"/>
      <input type="hidden" name="userTenantId" value="${tenantId!}"/>
      <@field type="display" label=uiLabelMap.CommonUsername>${username}</@field>
      <@field type="password" label=uiLabelMap.CommonCurrentPassword name="PASSWORD" value="" size="20" />
      <@field type="password" label=uiLabelMap.CommonNewPassword name="newPassword" value="" size="20" />
      <@field type="password" label=uiLabelMap.CommonNewPasswordVerify name="newPasswordVerify" value="" size="20" />
      
      <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_update!}"/>
    </form>
</@section>
</@cell>
</@row>

<@script>
    jQuery(document).ready(function() {
        document.loginform.PASSWORD.focus();
    });
</@script>
