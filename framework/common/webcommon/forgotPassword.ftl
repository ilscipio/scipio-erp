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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<#if layoutSettings.headerImageUrl??>
    <#assign headerImageUrl = layoutSettings.headerImageUrl>
<#elseif layoutSettings.commonHeaderImageUrl??>
    <#assign headerImageUrl = layoutSettings.commonHeaderImageUrl>
<#elseif layoutSettings.VT_HDR_IMAGE_URL??>
    <#assign headerImageUrl = layoutSettings.VT_HDR_IMAGE_URL.get(0)>
</#if>
<#assign logo><img src="<@contentUrl escapeAs='html'><#if headerImageUrl?has_content>${raw(headerImageUrl)}<#else>/images/scipio/scipio-logo-small.png</#if></@contentUrl>" style="height:32px;"/></#assign>
<#assign labelUsername><i class="${styles.icon!} ${styles.icon_user!}"></i></#assign>
<#assign labelPassword><i class="${styles.icon!} ${styles.icon_password!}"></i></#assign>
<#assign labelTenant><i class="${styles.icon!} ${styles.icon_tenant!}"></i></#assign>
<#assign username = requestParameters.USERNAME!(userLogin.userLoginId)!(autoUserLogin.userLoginId)!"">

<@row>
    <@cell class="${styles.grid_large!}centered ${styles.login_wrap!}" last=true id="login">
        <div id="login-box-title" class="${styles.login_header!}">
                <@heading level=1>${uiLabelMap.CommonPassword!}</@heading>
        </div>
        <@section id="login-box-content">
            <div class="${styles.login_body!}">
                <form method="post" action="<@pageUrl>forgotPassword${previousParams!}</@pageUrl>" name="forgotpassword">
                    <@field type="input" name="USERNAME" value=username size="20" collapse=true placeholder=uiLabelMap.CommonUsername tooltip=uiLabelMap.CommonUsername label=wrapAsRaw({'htmlmarkup':labelUsername, 'raw':rawLabel('CommonUsername')})/>
                    <@row>
                        <@cell columns=12 >
                            <a href="<@pageUrl>authview</@pageUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonGoBack}</a>
                            <@field type="submit" name="GET_PASSWORD_HINT" class="${styles.link_run_sys!} ${styles.action_view!}" text=uiLabelMap.CommonGetPasswordHint widgetOnly=true/>
                            <@field type="submit" name="EMAIL_PASSWORD" class="${styles.link_run_sys!} ${styles.action_send!}" text=uiLabelMap.CommonEmailPassword widgetOnly=true/>
                            <#--<input type="hidden" name="JavaScriptEnabled" value="N"/>-->
                        </@cell>
                    </@row>
                </form>
            </div>
        </@section>
  </@cell>
</@row>
