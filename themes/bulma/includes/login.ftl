<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>
<#assign useMultitenant = getPropertyValue("general", "multitenant")!"">
<#assign logo><@img height="32px" src='/base-theme/images/scipio-logo-small.png' /></#assign>
<#assign username = requestParameters.USERNAME!(userLogin.userLoginId)!(autoUserLogin.userLoginId)!"">
<#if username != "">
  <#assign focusName = false>
<#else>
  <#assign focusName = true>
</#if>

<div class="container">
    <@row class="is-fullheight-100v is-vcentered">
        <@cell class="${styles.grid_medium!}6 ${styles.grid_medium_offset!}3">
            <div class="${styles.login_wrap!} is-primary has-background-white has-box-shadow" id="login">
                <div class="${styles.login_header!} is-flex is-justify-content-space-between">
                    ${logo} ${uiLabelMap.CommonLogin!}
                </div>


                    <#if uiLabelMap.WebtoolsForSomethingInteresting?has_content
                        && uiLabelMap.WebtoolsForSomethingInteresting != "WebtoolsForSomethingInteresting">
                        <div class="${styles.login_body!}">
                        <@alert type="blank">
                            ${uiLabelMap.WebtoolsForSomethingInteresting}
                        </@alert>
                        </div>
                    </#if>

                <form method="post" action="<@pageUrl>login</@pageUrl>" name="loginform">
                    <#assign labelUsername><i class="${styles.icon!} ${styles.icon_user!}"></i></#assign>
                    <#assign labelPassword><i class="${styles.icon!} ${styles.icon_password!}"></i></#assign>
                    <#assign labelTenant><i class="${styles.icon!} ${styles.icon_tenant!}"></i></#assign>
                    <div class="${styles.login_body!}">
                        <@field type="input" name="USERNAME" value=username size="20" collapse=true placeholder=uiLabelMap.CommonUsername tooltip=uiLabelMap.CommonUsername label=wrapAsRaw({'htmlmarkup':labelUsername, 'raw':rawLabel('CommonUsername')})/>
                    </div>
                    <div class="${styles.login_body!}">
                        <@field type="password" name="PASSWORD" value="" size="20" collapse=true placeholder=uiLabelMap.CommonPassword tooltip=uiLabelMap.CommonPassword label=wrapAsRaw({'htmlmarkup':labelPassword, 'raw':rawLabel('CommonPassword')})/>
                    </div>
                    <#if ("Y" == useMultitenant) >
                        <div class="${styles.login_body!}">
                            <#--<#if !requestAttributes.userTenantId??>-->
                            <@field type="input" name="userTenantId" value=(parameters.userTenantId!) size="20" placeholder=uiLabelMap.CommonTenantId collapse=true tooltip=uiLabelMap.CommonTenantId label=wrapAsRaw({'htmlmarkup':labelTenant, 'raw':rawLabel('CommonTenantId')})/>
                            <#--<#else>
                                  <input type="hidden" name="userTenantId" value="${requestAttributes.userTenantId!}"/>
                              </#if>-->
                        </div>
                    </#if>
                   <div class="panel-block is-flex is-justify-content-space-between is-4 mt-3">
                        <#--<input type="hidden" name="JavaScriptEnabled" value="N"/>-->
                        <input type="submit" value="${uiLabelMap.CommonLogin}" class="button is-success is-fullwidth mr-2 ml-2"/>
                        <a href="<@pageUrl>forgotPassword</@pageUrl>" class="button is-danger is-fullwidth mr-2 ml-2">${uiLabelMap.CommonForgotYourPassword}</a>
                    </div>
                </form>
            </div>
        </@cell>
    </@row>
</div>
