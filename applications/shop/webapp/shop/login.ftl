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
<#-- SCIPIO: now points to shop -->
<#assign janrainEnabled = getPropertyValue("shop.properties", "janrain.enabled")!"">
<#assign appName = getPropertyValue("shop.properties", "janrain.appName")!"">
<#assign useMultitenant = getPropertyValue("general.properties", "multitenant")!"">

<@row class="+Signlogin">
    <@cell columns=6>
        <@section title=uiLabelMap.CommonRegistered>
            <#-- SCIPIO: Janrain is not fully tested. Use at your own risk -->
            <#if janrainEnabled == "Y">
                <@script>
                (function() {
                    if (typeof window.janrain !== 'object') window.janrain = {};
                    window.janrain.settings = {};
                    
                    janrain.settings.tokenUrl = '<@ofbizUrl fullPath="true" secure="true">janrainCheckLogin</@ofbizUrl>';
                
                    function isReady() { janrain.ready = true; };
                    if (document.addEventListener) {
                      document.addEventListener("DOMContentLoaded", isReady, false);
                    } else {
                      window.attachEvent('onload', isReady);
                    }
                
                    var e = document.createElement('script');
                    e.type = 'text/javascript';
                    e.id = 'janrainAuthWidget';
                
                    if (document.location.protocol === 'https:') {
                      e.src = 'https://rpxnow.com/js/lib/${appName}/engage.js';
                    } else {
                      e.src = 'http://widget-cdn.rpxnow.com/js/lib/${appName}/engage.js';
                    }
                
                    var s = document.getElementsByTagName('script')[0];
                    s.parentNode.insertBefore(e, s);
                })();
                </@script>
        
                      <form method="post" action="<@ofbizUrl>login</@ofbizUrl>" name="loginform" class="horizontal">              
                            <label for="userName">${uiLabelMap.CommonUsername}</label>
                            <input type="text" id="userName" name="USERNAME" value="<#if requestParameters.USERNAME?has_content>${requestParameters.USERNAME}<#elseif autoUserLogin?has_content>${autoUserLogin.userLoginId}</#if>"/>                          
                              <#if autoUserLogin?has_content>
                                <p>(${uiLabelMap.CommonNot} ${autoUserLogin.userLoginId}? <a href="<@ofbizUrl>${autoLogoutUrl}</@ofbizUrl>">${uiLabelMap.CommonClickHere}</a>)</p>
                              </#if>
                            <label for="password">${uiLabelMap.CommonPassword}:</label>
                            <input type="password" id="password" name="PASSWORD" value=""/>
                            <input type="submit" class="${styles.link_run_session!} ${styles.action_login!}" value="${uiLabelMap.CommonLogin}"/>
                      </form>
                     <div id="janrainEngageEmbed"></div>
        
            <#else><#-- Default login -->
                    <form method="post" action="<@ofbizUrl>login</@ofbizUrl>" name="loginform">
                       <#assign labelUsername><i class="${styles.icon!} ${styles.icon_user!}"></i></#assign>
                       <#assign labelPassword><i class="${styles.icon!} ${styles.icon_password!}"></i></#assign>
                       <#assign labelTenant><i class="${styles.icon!} ${styles.icon_tenant!}"></i></#assign>
                       <@field type="input" name="USERNAME" value=username size="20" collapse=true placeholder=uiLabelMap.CommonUsername tooltip=uiLabelMap.CommonUsername label=wrapAsRaw({'htmlmarkup':labelUsername, 'raw':rawLabel('CommonUsername')})/>
                       <@field type="password" name="PASSWORD" value="" size="20" collapse=true placeholder=uiLabelMap.CommonPassword tooltip=uiLabelMap.CommonPassword label=wrapAsRaw({'htmlmarkup':labelPassword, 'raw':rawLabel('CommonPassword')})/>
                
                          <#if ("Y" == useMultitenant) >
                              <#if !requestAttributes.userTenantId??>
                              <@field type="input" name="userTenantId" value=(parameters.userTenantId!) size="20" placeholder=uiLabelMap.CommonTenantId collapse=true tooltip=uiLabelMap.CommonTenantId label=wrapAsRaw({'htmlmarkup':labelTenant, 'raw':rawLabel('CommonTenantId')})/>
                              <#else>
                                  <input type="hidden" name="userTenantId" value="${requestAttributes.userTenantId!}"/>
                              </#if>
                          </#if>
                        <input type="hidden" name="JavaScriptEnabled" value="N"/>
                        <input type="submit" style="display: none;"/>
                    </form>
                         <@row>
                             <@cell class="+${styles.text_left!}" columns=9>
                                <small>
                                    <@modal id="modal_login_forgotpassword" label=uiLabelMap.CommonForgotYourPassword class="${styles.medium!}">
                                        <@row>
                                            <@cell class="large-centered ">
                                                <@section title=uiLabelMap.CommonPassword>
                                                    <#-- SCIPIO: WARN: Proper HTML-escaping of params high importance here -->
                                                    <form method="post" action="${escapeFullUrl(makeOfbizUrl("forgotPassword" + rawString(previousParams!"")), 'html')}" name="forgotpassword">
                                                        <@field type="input" name="USERNAME" value=username size="20" collapse=true placeholder=uiLabelMap.CommonUsername tooltip=uiLabelMap.CommonUsername label=wrapAsRaw(labelUsername!)/>
                                                        <@row>
                                                            <@cell columns=12>
                                                                <a href="<@ofbizUrl>login</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonGoBack}</a>
                                                            </@cell>
                                                        </@row>
                                                        <@row>
                                                            <@cell columns=6>
                                                                <@field type="submit" name="GET_PASSWORD_HINT" class="${styles.link_run_sys!} ${styles.action_view!}" text=uiLabelMap.CommonGetPasswordHint widgetOnly=true/>
                                                            </@cell>
                                                            <@cell columns=6>
                                                                <@field type="submit" name="EMAIL_PASSWORD" class="${styles.link_run_sys!} ${styles.action_send!}" text=uiLabelMap.CommonEmailPassword widgetOnly=true/>
                                                            </@cell>
                                                        </@row>
                                                    </form>
                                                </@section>
                                            </@cell>
                                        </@row>
                                    </@modal>                                
                                </small>
                             </@cell>
                            <@cell class="+${styles.text_right!}" columns=3>
                                <#-- SCIPIO: workaround for form-within-form and form fields getting mixed up (form closed earlier):
                                <input type="submit" value="${uiLabelMap.CommonLogin}" class="${styles.link_run_session!} ${styles.action_login!}"/>-->
                                <@field type="submit" submitType="link" href="javascript:document.loginform.submit();" widgetOnly=true value=uiLabelMap.CommonLogin class="${styles.link_run_session!} ${styles.action_login!}"/>
                            </@cell>
                        </@row>
            </#if>
        
        </@section>
            <@script>
              jQuery(document).ready(function() {
                <#if autoUserLogin?has_content>
                  document.loginform.PASSWORD.focus();
                <#else>
                  document.loginform.USERNAME.focus();
                </#if>
              });
            </@script>
    </@cell>
    <@cell columns=6>
        <@section title=uiLabelMap.CommonNewUser>
            <@panel>
            <@row>
                <@cell>
             Create a customer account:
              <ul class="checkmark">
                    <li>Place orders quickly and easily</li>
                    <li>View orders and track your shipping status</li>
                    <li>Receive special deals</li>
              </ul>
             </@cell>
            </@row>
            <@row>
                          <@cell class="+${styles.text_right!}">
                <a href="<@ofbizUrl>newcustomer</@ofbizUrl>" class="${styles.link_run_local!} ${styles.action_add!}">${uiLabelMap.EcommerceRegister}</a>
            </@cell>
            </@row>
             </@panel>
        </@section>

    </@cell>
</@row>
<#-- SOCIAL PLugins - requires Facebook, Google or Twitter Authentication Addon -->
<#if getPropertyMsg("shop.properties","facebook.enabled") == "Y" 
    || getPropertyMsg("shop.properties","google.enabled") == "Y" 
    || getPropertyMsg("shop.properties","twitter.enabled") == "Y">
<@section>
<@row>
    <@cell columns=6>
            
            <@heading>${uiLabelMap.CommonOrLoginWith}</@heading>
            <#-- Facebook Login (Requires Facebook Authentication Addon)-->
            <#if getPropertyMsg("shop.properties","facebook.enabled") == "Y">
                <#include "component://auth-facebook/webapp/facebook/fb-common.ftl"/>
                <@fbButton/>
            </#if>
            <#if getPropertyMsg("shop.properties","google.enabled") == "Y">
                <#include "component://auth-google/webapp/google/google-common.ftl"/>
                <@googleButton/>
            </#if>
            <#if getPropertyMsg("shop.properties","twitter.enabled") == "Y">
                <#include "component://auth-twitter/webapp/twitter/twitter-common.ftl"/>
                <@twitterButton/>
            </#if>
         </@cell>
    </@row>
</@section>
</#if>
<#--  
<@section title=uiLabelMap.CommonForgotYourPassword>
  <form method="post" action="<@ofbizUrl>forgotpassword</@ofbizUrl>" class="horizontal">
    
      <label for="forgotpassword_userName">${uiLabelMap.CommonUsername}</label>
      <input type="text" id="forgotpassword_userName" name="USERNAME" value="<#if requestParameters.USERNAME?has_content>${requestParameters.USERNAME}<#elseif autoUserLogin?has_content>${autoUserLogin.userLoginId}</#if>"/>
    
    
      <input type="submit" class="${styles.link_run_sys!} ${styles.action_view!}" name="GET_PASSWORD_HINT" value="${uiLabelMap.CommonGetPasswordHint}"/>
      <input type="submit" class="${styles.link_run_sys!} ${styles.action_send!}" name="EMAIL_PASSWORD" value="${uiLabelMap.CommonEmailPassword}"/>
    
  </form>
</@section>
-->