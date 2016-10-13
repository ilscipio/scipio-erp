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
<#if layoutSettings.headerImageUrl??>
    <#assign headerImageUrl = layoutSettings.headerImageUrl>
<#elseif layoutSettings.commonHeaderImageUrl??>
    <#assign headerImageUrl = layoutSettings.commonHeaderImageUrl>
<#elseif layoutSettings.VT_HDR_IMAGE_URL??>
    <#assign headerImageUrl = layoutSettings.VT_HDR_IMAGE_URL.get(0)>
</#if>
<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>
<#assign useMultitenant = getPropertyValue("general.properties", "multitenant")!"">
<#assign logo><img src="<@ofbizContentUrl><#if headerImageUrl?has_content>${rawString(headerImageUrl)}<#else>/images/scipio/scipio-logo-small.png</#if></@ofbizContentUrl>"/></#assign>
<#assign username = requestParameters.USERNAME!(sessionAttributes.autoUserLogin.userLoginId)!"">
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
      <form method="post" action="<@ofbizUrl>login</@ofbizUrl>" name="loginform">
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
                <small><a href="<@ofbizUrl>forgotPassword</@ofbizUrl>">${uiLabelMap.CommonForgotYourPassword}</a></small>
             </@cell>
            <@cell class="+${styles.text_right!}" columns=5>
                <input type="hidden" name="JavaScriptEnabled" value="N"/>
                <input type="submit" value="${uiLabelMap.CommonLogin}" class="${styles.link_run_session!} ${styles.action_login!}"/>
            </@cell>
        </@row>
      </form>
    </div>

  </@section>
</@cell>
</@row>

<@script>
  document.loginform.JavaScriptEnabled.value = "Y";
  <#if focusName>
    document.loginform.USERNAME.focus();
  <#else>
    document.loginform.PASSWORD.focus();
  </#if>
</@script>
