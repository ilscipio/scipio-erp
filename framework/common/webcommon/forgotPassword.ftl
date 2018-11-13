<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#if layoutSettings.headerImageUrl??>
    <#assign headerImageUrl = layoutSettings.headerImageUrl>
<#elseif layoutSettings.commonHeaderImageUrl??>
    <#assign headerImageUrl = layoutSettings.commonHeaderImageUrl>
<#elseif layoutSettings.VT_HDR_IMAGE_URL??>
    <#assign headerImageUrl = layoutSettings.VT_HDR_IMAGE_URL.get(0)>
</#if>
<#assign logo><img src="<@ofbizContentUrl escapeAs='html'><#if headerImageUrl?has_content>${rawString(headerImageUrl)}<#else>/images/scipio/scipio-logo-small.png</#if></@ofbizContentUrl>" style="height:32px;"/></#assign>
<#assign labelUsername><i class="${styles.icon!} ${styles.icon_user!}"></i></#assign>
<#assign labelPassword><i class="${styles.icon!} ${styles.icon_password!}"></i></#assign>
<#assign labelTenant><i class="${styles.icon!} ${styles.icon_tenant!}"></i></#assign>
<#assign username = requestParameters.USERNAME!(sessionAttributes.autoUserLogin.userLoginId)!"">

<@row>
    <@cell class="${styles.grid_large!}centered ${styles.login_wrap!}" last=true id="login">
        <div id="login-box-title" class="${styles.login_header!}">
                <@heading level=1>${uiLabelMap.CommonPassword!}</@heading>
        </div>
        <@section id="login-box-content">
            <div class="${styles.login_body!}">
                <form method="post" action="<@ofbizUrl>forgotPassword${previousParams!}</@ofbizUrl>" name="forgotpassword">
                    <@field type="input" name="USERNAME" value=username size="20" collapse=true placeholder=uiLabelMap.CommonUsername tooltip=uiLabelMap.CommonUsername label=wrapAsRaw({'htmlmarkup':labelUsername, 'raw':rawLabel('CommonUsername')})/>
                    <@row>
                        <@cell columns=12 >
                            <a href="<@ofbizUrl>authview</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonGoBack}</a>
                            <@field type="submit" name="GET_PASSWORD_HINT" class="${styles.link_run_sys!} ${styles.action_view!}" text=uiLabelMap.CommonGetPasswordHint widgetOnly=true/>
                            <@field type="submit" name="EMAIL_PASSWORD" class="${styles.link_run_sys!} ${styles.action_send!}" text=uiLabelMap.CommonEmailPassword widgetOnly=true/>
                            <input type="hidden" name="JavaScriptEnabled" value="N"/>
                        </@cell>
                    </@row>
                </form>
            </div>
        </@section>
  </@cell>
</@row>
