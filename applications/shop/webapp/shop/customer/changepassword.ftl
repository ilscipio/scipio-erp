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

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" id="CommonGoBack1" href=makeOfbizUrl("${donePage}") class="+${styles.action_nav_cancel!}" text=uiLabelMap.CommonGoBack />
        <@menuitem type="link" id="CommonSave1" href="javascript:document.getElementById('changepasswordform').submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
    </@menu>
</#macro>
<@section title=uiLabelMap.PartyChangePassword menuContent=menuContent>
    <form id="changepasswordform" method="post" action="<@ofbizUrl>updatePassword/${donePage}</@ofbizUrl>">
      <fieldset>
        <div>
          <label for="currentPassword">${uiLabelMap.PartyOldPassword}</label>
          <input type="password" class="inputBox" name="currentPassword" id="currentPassword" maxlength="20" />*
        </div>
        <div>
          <label for="newPassword">${uiLabelMap.PartyNewPassword}</label>
          <input type="password" class="inputBox" name="newPassword" id="newPassword" maxlength="20" />*
        </div>
        <div>
          <label for="newPasswordVerify">${uiLabelMap.PartyNewPasswordVerify}</label>
          <input type="password" class="inputBox" name="newPasswordVerify" id="newPasswordVerify" maxlength="20" />*
        </div>
        <div>
          <label for="passwordHint">${uiLabelMap.PartyPasswordHint}</label>
          <input type="text" class="inputBox" maxlength="100" name="passwordHint" id="passwordHint" value="${userLoginData.passwordHint!}" />
        </div>
        <label>${uiLabelMap.CommonFieldsMarkedAreRequired}</label>
      </fieldset>
    </form>
</@section>