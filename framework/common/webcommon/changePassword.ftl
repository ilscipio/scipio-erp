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

<#assign username = requestParameters.USERNAME!(sessionAttributes.autoUserLogin.userLoginId)!"">
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
