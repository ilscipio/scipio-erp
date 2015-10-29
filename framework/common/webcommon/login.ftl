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

<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>
<#assign useMultitenant = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("general.properties", "multitenant")>
<#assign logo><img src="<@ofbizContentUrl>/images/feather-tiny.png</@ofbizContentUrl>"/></#assign>
<#assign username = requestParameters.USERNAME?default((sessionAttributes.autoUserLogin.userLoginId)?default(""))>
<#if username != "">
  <#assign focusName = false>
<#else>
  <#assign focusName = true>
</#if>

<div class="${styles.grid_large!}3 ${styles.grid_large!}centered ${styles.grid_cell} ${styles.login_wrap!}" id="login">
<div id="login-box-title" class="${styles.login_header!}">
    <h1>${logo} ${uiLabelMap.CommonLogin!}</h1>
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
       <@field type="input" name="USERNAME" value="${username}" size="20" placeholder="${uiLabelMap.CommonUsername}" tooltip="${uiLabelMap.CommonUsername}" label="${labelUsername!}"/>
       <@field type="password" name="PASSWORD" value="" size="20" placeholder="${uiLabelMap.CommonPassword}" tooltip="${uiLabelMap.CommonPassword}" label="${labelPassword!}"/>

          <#if ("Y" == useMultitenant) >
              <#if !requestAttributes.userTenantId??>
              <@field type="input" name="userTenantId" value="${parameters.userTenantId!}" size="20" placeholder="${uiLabelMap.CommonTenantId}" tooltip="${uiLabelMap.CommonTenantId}" label="${labelTenant!}"/>
              <#else>
                  <input type="hidden" name="userTenantId" value="${requestAttributes.userTenantId!}"/>
              </#if>
          </#if>
         
         <@row>
             <@cell class="${styles.grid_large!}12 text-left">
                <small><a href="<@ofbizUrl>forgotPassword</@ofbizUrl>">${uiLabelMap.CommonForgotYourPassword}?</a></small>
             </@cell>
        </@row>
        <@row>
            <@cell class="${styles.grid_large!}12 text-right">
                <input type="hidden" name="JavaScriptEnabled" value="N"/>
                <input type="submit" value="${uiLabelMap.CommonLogin}" class="button"/>
            </@cell>
        </@row>
      </form>
    </div>

</@section>
</div>
<script language="JavaScript" type="text/javascript">
  document.loginform.JavaScriptEnabled.value = "Y";
  <#if focusName>
    document.loginform.USERNAME.focus();
  <#else>
    document.loginform.PASSWORD.focus();
  </#if>
</script>
