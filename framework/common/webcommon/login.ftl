<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if layoutSettings.headerImageUrl??>
    <#assign headerImageUrl = layoutSettings.headerImageUrl>
<#elseif layoutSettings.commonHeaderImageUrl??>
    <#assign headerImageUrl = layoutSettings.commonHeaderImageUrl>
<#elseif layoutSettings.VT_HDR_IMAGE_URL??>
    <#assign headerImageUrl = layoutSettings.VT_HDR_IMAGE_URL.get(0)>
</#if>
<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>
<#assign useMultitenant = getPropertyValue("general", "multitenant")!"">
<#assign logo><img src="<@contentUrl escapeAs='html'><#if headerImageUrl?has_content>${raw(headerImageUrl)}<#else>/images/scipio/scipio-logo-small.png</#if></@contentUrl>" style="height:32px;"/></#assign>
<#assign username = requestParameters.USERNAME!(userLogin.userLoginId)!(autoUserLogin.userLoginId)!"">
<#if username != "">
  <#assign focusName = false>
<#else>
  <#assign focusName = true>
</#if>

<@row>
<#-- NOTE: login_wrap contains grid size -->
<@cell class="${styles.grid_large!}centered ${styles.login_wrap!}" last=true id="login">
  <div id="login-box-title" class="${styles.login_header!}">
    <@heading level=1>${logo} ${uiLabelMap.CommonLogin!}</@heading>
  </div>

  <@section id="login-box-content">
    <#if uiLabelMap.WebtoolsForSomethingInteresting?has_content 
       && uiLabelMap.WebtoolsForSomethingInteresting != "WebtoolsForSomethingInteresting">
      <@alert type="info">
        ${uiLabelMap.WebtoolsForSomethingInteresting}
      </@alert>
    </#if>

    <div class="${styles.login_body!}">
      <form method="post" action="<@pageUrl>login</@pageUrl>" name="loginform">
       <#assign labelUsername><i class="${styles.icon!} ${styles.icon_user!}"></i></#assign>
       <#assign labelPassword><i class="${styles.icon!} ${styles.icon_password!}"></i></#assign>
       <#assign labelTenant><i class="${styles.icon!} ${styles.icon_tenant!}"></i></#assign>
       <@field type="input" name="USERNAME" value=username size="20" collapse=true placeholder=uiLabelMap.CommonUsername tooltip=uiLabelMap.CommonUsername label=wrapAsRaw({'htmlmarkup':labelUsername, 'raw':rawLabel('CommonUsername')})/>
       <@field type="password" name="PASSWORD" value="" size="20" collapse=true placeholder=uiLabelMap.CommonPassword tooltip=uiLabelMap.CommonPassword label=wrapAsRaw({'htmlmarkup':labelPassword, 'raw':rawLabel('CommonPassword')})/>

          <#if ("Y" == useMultitenant) >
              <#--<#if !requestAttributes.userTenantId??>-->
              <@field type="input" name="userTenantId" value=(parameters.userTenantId!) size="20" placeholder=uiLabelMap.CommonTenantId collapse=true tooltip=uiLabelMap.CommonTenantId label=wrapAsRaw({'htmlmarkup':labelTenant, 'raw':rawLabel('CommonTenantId')})/>
              <#--<#else>
                  <input type="hidden" name="userTenantId" value="${requestAttributes.userTenantId!}"/>
              </#if>-->
          </#if>
         
         <@row>
             <@cell class="+${styles.text_left!}" columns=7>
                <small><a href="<@pageUrl>forgotPassword</@pageUrl>">${uiLabelMap.CommonForgotYourPassword}</a></small>
             </@cell>
            <@cell class="+${styles.text_right!}" columns=5>
                <#--<input type="hidden" name="JavaScriptEnabled" value="N"/>-->
                <input type="submit" value="${uiLabelMap.CommonLogin}" class="${styles.link_run_session!} ${styles.action_login!}"/>
            </@cell>
        </@row>
      </form>
    </div>

  </@section>
</@cell>
</@row>

<@script>
  <#--document.loginform.JavaScriptEnabled.value = "Y";-->
  <#if focusName>
    document.loginform.USERNAME.focus();
  <#else>
    document.loginform.PASSWORD.focus();
  </#if>
</@script>
