<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->

<#assign username = ""/>
<#if requestParameters.USERNAME?has_content>
  <#assign username = requestParameters.USERNAME/>
<#elseif userLogin??>
    <#assign username = userLogin.userLoginId/>
<#elseif autoUserLogin??>
  <#assign username = autoUserLogin.userLoginId/>
</#if>

<@heading level=1>${uiLabelMap.CommonLogin}</@heading>

<@section title=uiLabelMap.CommonPasswordChange style="float: center; width: 49%; margin-right: 5px; text-align: center;">
  <form method="post" action="<@pageUrl>login${previousParams}</@pageUrl>" name="loginform">
      <input type="hidden" name="requirePasswordChange" value="Y"/>
      <input type="hidden" name="USERNAME" value="${username}"/>
      <@field type="display" label=uiLabelMap.CommonUsername value=username />

      <#if userLogin?? || autoUserLogin???>
          <div>
              (${uiLabelMap.CommonNot}&nbsp;${(userLogin.userLoginId)!(autoUserLogin.userLoginId)!}?&nbsp;<a href="<@pageUrl>${autoLogoutUrl}</@pageUrl>" class="${styles.link_nav!} ${styles.action_login!}">${uiLabelMap.CommonClickHere}</a>)
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

