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

<div class="${style_grid_large!}3 ${style_grid_large!}centered columns login-box" id="login">
<div id="login-box-title">
    <h1>${logo} ${uiLabelMap.CommonLogin!}</h1>
    </div>

<@section id="login-box-content">
  <#if uiLabelMap.WebtoolsForSomethingInteresting?has_content 
       && uiLabelMap.WebtoolsForSomethingInteresting != "WebtoolsForSomethingInteresting">
  <@alert type="secondary">
      ${uiLabelMap.WebtoolsForSomethingInteresting}
  </@alert>
  </#if>
  <@row>
    <div class="${style_grid_large!}12 columns auth-plain">
     <div class="signup-panel right-solid">
      <form method="post" action="<@ofbizUrl>login</@ofbizUrl>" name="loginform">
       <@row>
        <div class="${style_grid_large!}12 columns">
          <div class="row collapse">
            <div class="${style_grid_small!}3 columns">
              <span class="prefix"><i class="fi-torso-female"></i></span>
            </div>
            <div class="${style_grid_small!}9 columns">
              <input type="text" name="USERNAME" value="${username}" size="20" placeholder="${uiLabelMap.CommonUsername}" title="${uiLabelMap.CommonUsername}" data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" />
            </div>
          </div>
        </div>
       </@row>
      <@row>
        <div class="${style_grid_large!}12 columns">
          <div class="row collapse">
            <div class="${style_grid_small!}3 columns">
              <span class="prefix"><i class="fi-lock"></i></span>
            </div>
            <div class="${style_grid_small!}9 columns">
              <input type="password" name="PASSWORD" value="" size="20" placeholder="${uiLabelMap.CommonPassword}" title="${uiLabelMap.CommonPassword}" data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" />
            </div>
          </div>
        </div>
       </@row>
          <#if ("Y" == useMultitenant) >
              <#if !requestAttributes.userTenantId??>
              <div class="row">
                <div class="${style_grid_large!}12 columns">
                  <div class="row collapse">
                    <div class="${style_grid_small!}3 columns">
                      <span class="prefix">${uiLabelMap.CommonTenantId}</span>
                    </div>
                    <div class="${style_grid_small!}9 columns">
                      <input type="text" name="userTenantId" value="${parameters.userTenantId!}" size="20"/>
                    </div>
                  </div>
                </div>
               </div>
              <#else>
                  <input type="hidden" name="userTenantId" value="${requestAttributes.userTenantId!}"/>
              </#if>
          </#if>
         
         <@row>
             <@cell class="${style_grid_large!}9 columns text-left">
                <small><a href="<@ofbizUrl>forgotPassword</@ofbizUrl>">${uiLabelMap.CommonForgotYourPassword}?</a></small>
                
             </@cell>
            <@cell class="${style_grid_large!}12 ${style_grid_large!}centered columns text-right">
        <input type="hidden" name="JavaScriptEnabled" value="N"/>
                <input type="submit" value="${uiLabelMap.CommonLogin}" class="button"/>
            </@cell>
        </@row>
      </form>
    </div>
  </div>
  </@row>
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
