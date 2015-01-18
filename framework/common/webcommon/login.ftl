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

<#assign username = requestParameters.USERNAME?default((sessionAttributes.autoUserLogin.userLoginId)?default(""))>
<#if username != "">
  <#assign focusName = false>
<#else>
  <#assign focusName = true>
</#if>




  <@row>
      <h1>${uiLabelMap.CommonRegistered}</h1>
    <div class="panel">
      <form method="post" action="<@ofbizUrl>login</@ofbizUrl>" name="loginform">
       <div class="row">
        <div class="large-6 columns">
          <div class="row collapse prefix-radius">
            <div class="small-3 columns">
              <span class="prefix">${uiLabelMap.CommonUsername}</span>
            </div>
            <div class="small-9 columns">
              <input type="text" name="USERNAME" value="${username}" size="20" placeholder="admin"/>
            </div>
          </div>
        </div>
       </div>
      <div class="row">
        <div class="large-6 columns">
          <div class="row collapse prefix-radius">
            <div class="small-3 columns">
              <span class="prefix">${uiLabelMap.CommonPassword}</span>
            </div>
            <div class="small-9 columns">
              <input type="password" name="PASSWORD" value="" size="20" placeholder="ofbiz"/>
            </div>
          </div>
        </div>
       </div>
          <#if ("Y" == useMultitenant) >
              <#if !requestAttributes.tenantId??>
              <div class="row">
                <div class="large-6 columns">
                  <div class="row collapse prefix-radius">
                    <div class="small-3 columns">
                      <span class="prefix">${uiLabelMap.CommonTenantId}</span>
                    </div>
                    <div class="small-9 columns">
                      <input type="text" name="tenantId" value="${parameters.tenantId!}" size="20"/>
                    </div>
                  </div>
                </div>
               </div>
              <#else>
                  <input type="hidden" name="tenantId" value="${requestAttributes.tenantId!}"/>
              </#if>
          </#if>
          <tr>
            <td colspan="2" align="center">
              <input type="submit" value="${uiLabelMap.CommonLogin}" class="button"/>
            </td>
          </tr>
        </table>
        <input type="hidden" name="JavaScriptEnabled" value="N"/>
        <br />
        <a href="<@ofbizUrl>forgotPassword</@ofbizUrl>">${uiLabelMap.CommonForgotYourPassword}?</a>
      </form>
    </div>
  </@row>
<script language="JavaScript" type="text/javascript">
  document.loginform.JavaScriptEnabled.value = "Y";
  <#if focusName>
    document.loginform.USERNAME.focus();
  <#else>
    document.loginform.PASSWORD.focus();
  </#if>
</script>
