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


<div class="row">
    <div class="large-12 columns">
      <h1>${uiLabelMap.CommonRegistered}</h1>
    <div class="panel">
      <form method="post" action="<@ofbizUrl>forgotPassword${previousParams!}</@ofbizUrl>" name="forgotpassword">
       <div class="row">
        <div class="large-6 columns">
          <div class="row collapse prefix-radius">
            <div class="small-3 columns">
              <span class="prefix">${uiLabelMap.CommonUsername}</span>
    </div>
            <div class="small-9 columns">
              <input type="text" name="USERNAME" value="<#if requestParameters.USERNAME?has_content>${requestParameters.USERNAME}<#elseif autoUserLogin?has_content>${autoUserLogin.userLoginId}</#if>" size="20" placeholder="admin"/>
            </div>
          </div>
        </div>
       </div>
      <div class="row">
        <div class="large-6 columns">
          <div class="row collapse prefix-radius">
            <div class="small-12 columns">
              <input type="submit" name="GET_PASSWORD_HINT" class="smallSubmit" value="${uiLabelMap.CommonGetPasswordHint}"/>&nbsp;<input type="submit" name="EMAIL_PASSWORD" class="smallSubmit" value="${uiLabelMap.CommonEmailPassword}"/>
                <#--<a href='<@ofbizUrl>authview</@ofbizUrl>' class="button">${uiLabelMap.CommonGoBack}</a>-->
        <input type="hidden" name="JavaScriptEnabled" value="N"/>
            </div>
          </div>
        </div>
       </div>
      </form>
    </div>
  </div>
</div>
