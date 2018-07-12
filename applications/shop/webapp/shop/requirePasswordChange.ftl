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

<#assign username = ""/>
<#if requestParameters.USERNAME?has_content>
  <#assign username = requestParameters.USERNAME/>
<#elseif autoUserLogin?has_content>
  <#assign username = autoUserLogin.userLoginId/>
</#if>

<@heading level=1>${uiLabelMap.CommonLogin}</@heading>

<@section title=uiLabelMap.CommonPasswordChange style="float: center; width: 49%; margin-right: 5px; text-align: center;">
  <form method="post" action="<@ofbizUrl>login${previousParams}</@ofbizUrl>" name="loginform">
      <input type="hidden" name="requirePasswordChange" value="Y"/>
      <input type="hidden" name="USERNAME" value="${username}"/>
      <@field type="display" label=uiLabelMap.CommonUsername value=username />

      <#if autoUserLogin?has_content>
          <div>
              (${uiLabelMap.CommonNot}&nbsp;${autoUserLogin.userLoginId}?&nbsp;<a href="<@ofbizUrl>${autoLogoutUrl}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_login!}">${uiLabelMap.CommonClickHere}</a>)
          </div>
      </#if>

      <@field type="password" name="PASSWORD" value="" size="20" label=uiLabelMap.CommonPassword required=true />
      <@field type="password" name="newPassword" value="" size="20" label=uiLabelMap.CommonNewPassword required=true />
      <@field type="password" name="newPasswordVerify" value="" size="20" label=uiLabelMap.CommonNewPasswordVerify required=true />

      <@field type="submit" class="${styles.link_run_session!} ${styles.action_login!}" text=uiLabelMap.CommonLogin/>
 
      </form>
</@section>

<@script>
    jQuery(document).ready(function() {
      <#-- SCIPIO: 2018-07-11: this is flawed, and the above might not even be using autoUserLogin at all...
      <#if autoUserLogin?has_content>
        document.loginform.PASSWORD.focus();
      <#else>
        document.loginform.USERNAME.focus();
      </#if>-->
        var loginform = document.loginform;
        if ($('input[name=USERNAME]', loginform).val()) {
            loginform.PASSWORD.focus();
        } else {
            loginform.USERNAME.focus();
        }
    });
</@script>

