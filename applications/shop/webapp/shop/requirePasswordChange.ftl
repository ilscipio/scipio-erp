<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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

