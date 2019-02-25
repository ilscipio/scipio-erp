<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- SCIPIO: now points to shop -->
<#assign janrainEnabled = getPropertyValue("shop", "janrain.enabled")!"">
<#assign appName = getPropertyValue("shop", "janrain.appName")!"">
<#assign useMultitenant = getPropertyValue("general", "multitenant")!"">

<@row class="+Signlogin">
    <@cell columns=6>
        <@section title=uiLabelMap.CommonRegistered>
            <#-- SCIPIO: Janrain is not fully tested. Use at your own risk -->
            <#if janrainEnabled == "Y">
                <@script>
                (function() {
                    if (typeof window.janrain !== 'object') window.janrain = {};
                    window.janrain.settings = {};
                    
                    janrain.settings.tokenUrl = '<@pageUrl fullPath="true" secure="true">janrainCheckLogin</@pageUrl>';
                
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
        
                      <form method="post" action="<@pageUrl>login</@pageUrl>" name="loginform" class="horizontal">              
                            <label for="userName">${uiLabelMap.CommonUsername}</label>
                            <input type="text" id="userName" name="USERNAME" value="<#if requestParameters.USERNAME?has_content>${requestParameters.USERNAME}<#elseif autoUserLogin?has_content>${autoUserLogin.userLoginId}</#if>"/>                          
                              <#if autoUserLogin?has_content>
                                <p>(${uiLabelMap.CommonNot} ${autoUserLogin.userLoginId}? <a href="<@pageUrl>${autoLogoutUrl}</@pageUrl>">${uiLabelMap.CommonClickHere}</a>)</p>
                              </#if>
                            <label for="password">${uiLabelMap.CommonPassword}:</label>
                            <input type="password" id="password" name="PASSWORD" value=""/>
                            <input type="submit" class="${styles.link_run_session!} ${styles.action_login!}" value="${uiLabelMap.CommonLogin}"/>
                      </form>
                     <div id="janrainEngageEmbed"></div>
        
            <#else><#-- Default login -->
                    <form method="post" action="<@pageUrl>login</@pageUrl>" name="loginform">
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
                                    <@modal id="modal_login_forgotpassword" label=uiLabelMap.CommonForgotYourPassword linkClass="${styles.medium!}">
                                        <@row>
                                            <@cell class="large-centered ">
                                                <@section title=uiLabelMap.CommonPassword>
                                                    <#-- SCIPIO: WARN: Proper HTML-escaping of params high importance here -->
                                                    <form method="post" action="${escapeFullUrl(makePageUrl("forgotPassword" + raw(previousParams!"")), 'html')}" name="forgotpassword">
                                                        <@field type="input" name="USERNAME" value=username size="20" collapse=true placeholder=uiLabelMap.CommonUsername tooltip=uiLabelMap.CommonUsername label=wrapAsRaw(labelUsername!)/>
                                                        <@row>
                                                            <@cell columns=12>
                                                                <a href="<@pageUrl>login</@pageUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonGoBack}</a>
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
                <a href="<@pageUrl>newcustomer</@pageUrl>" class="${styles.link_run_local!} ${styles.action_add!}">${uiLabelMap.EcommerceRegister}</a>
            </@cell>
            </@row>
             </@panel>
        </@section>

    </@cell>
</@row>
<#-- SOCIAL PLugins - requires Facebook, Google or Twitter Authentication Addon -->
<#if ("Y" == getPropertyValue("shop", "facebook.enabled")!)
    || ("Y" == getPropertyValue("shop", "google.enabled")!)
    || ("Y" == getPropertyValue("shop", "twitter.enabled")!)
    || ("Y" == getPropertyValue("shop", "linkedin.enabled")!)>
<@section>
<@row>
    <@cell columns=6>
            <@heading>${uiLabelMap.CommonOrLoginWith}</@heading>
            <#-- Facebook Login (Requires Facebook Authentication Addon)-->
            <#if "Y" == getPropertyValue("shop", "facebook.enabled")!>
                <#include "component://auth-facebook/webapp/facebook/fb-common.ftl"/>
                <@fbButton/>
            </#if>
            <#if "Y" == getPropertyValue("shop", "google.enabled")!>
                <#include "component://auth-google/webapp/google/google-common.ftl"/>
                <@googleButton/>
            </#if>
            <#if "Y" == getPropertyValue("shop", "twitter.enabled")!>
                <#include "component://auth-twitter/webapp/twitter/twitter-common.ftl"/>
                <@twitterButton/>
            </#if>
            <#if "Y" == getPropertyValue("shop", "linkedin.enabled")!>
                <#include "component://auth-linkedin/webapp/linkedin/linkedin-common.ftl"/>
                <@linkedinButton/>
            </#if>
         </@cell>
    </@row>
</@section>
</#if>
<#--  
<@section title=uiLabelMap.CommonForgotYourPassword>
  <form method="post" action="<@pageUrl>forgotpassword</@pageUrl>" class="horizontal">
    
      <label for="forgotpassword_userName">${uiLabelMap.CommonUsername}</label>
      <input type="text" id="forgotpassword_userName" name="USERNAME" value="<#if requestParameters.USERNAME?has_content>${requestParameters.USERNAME}<#elseif autoUserLogin?has_content>${autoUserLogin.userLoginId}</#if>"/>
    
    
      <input type="submit" class="${styles.link_run_sys!} ${styles.action_view!}" name="GET_PASSWORD_HINT" value="${uiLabelMap.CommonGetPasswordHint}"/>
      <input type="submit" class="${styles.link_run_sys!} ${styles.action_send!}" name="EMAIL_PASSWORD" value="${uiLabelMap.CommonEmailPassword}"/>
    
  </form>
</@section>
-->