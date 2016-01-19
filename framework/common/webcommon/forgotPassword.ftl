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
<#assign logo><img src="<@ofbizContentUrl>/images/feather-tiny.png</@ofbizContentUrl>"/></#assign>

<@row>
  <@cell class="${styles.grid_large!}3 ${styles.grid_large!}centered login-box" last=true id="login">
    <div id="login-box-title">
        <@heading level=1>${logo} ${uiLabelMap.CommonRegistered!}</@heading>
    </div>
    <@section id="login-box-content">
            <@panel>
              <form method="post" action="<@ofbizUrl>forgotPassword${previousParams!}</@ofbizUrl>" name="forgotpassword">
               <@row>
                <@cell class="auth-plain">
                  <@row collapse=true class="+prefix-radius">
                    <@cell columns=3>
                      <span class="prefix">${uiLabelMap.CommonUsername}</span>
                    </@cell>
                    <@cell columns=9>
                      <input type="text" name="USERNAME" value="<#if requestParameters.USERNAME?has_content>${requestParameters.USERNAME}<#elseif autoUserLogin?has_content>${autoUserLogin.userLoginId}</#if>" size="20" placeholder="admin"/>
                    </@cell>
                  </@row>
                </@cell>
               </@row>
              <@row>
                <@cell columns=12>
                      <input type="submit" name="GET_PASSWORD_HINT" class="${styles.link_run_sys!} ${styles.action_view!}" value="${uiLabelMap.CommonGetPasswordHint}"/>
                      <input type="submit" name="EMAIL_PASSWORD" class="${styles.link_run_sys!} ${styles.action_send!}" value="${uiLabelMap.CommonEmailPassword}"/>
                      <#--<a href='<@ofbizUrl>authview</@ofbizUrl>' class="${styles.link_nav_cancel!}">${uiLabelMap.CommonGoBack}</a>-->
                </@cell>
               </@row>
               <input type="hidden" name="JavaScriptEnabled" value="N"/>
              </form>
            </@panel>
    </@section>
  </@cell>
</@row>
