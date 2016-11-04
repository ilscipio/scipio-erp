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
<#include "customercommon.ftl">

<#-- SCIPIO: This was a message to explain to "Go Back" kludge; however I have now recoded controller and screen
    to redirect automatically.
<@commonMsg type="info-important">${uiLabelMap.ShopSaveGoBackExplanation}</@commonMsg>-->

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" id="CommonGoBack1" href=makeOfbizUrl(donePage) class="+${styles.action_nav_cancel!}" text=uiLabelMap.CommonGoBack />
        <@menuitem type="link" id="CommonSave1" href="javascript:document.getElementById('changepasswordform').submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
    </@menu>
</#macro>
<@section menuContent=menuContent menuLayoutGeneral="bottom"><#--title=uiLabelMap.PartyChangePassword-->
    <#-- SCIPIO: NOTE: view-switch replaced with redirect -->
    <form id="changepasswordform" method="post" action="<@ofbizUrl>updatePassword?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</@ofbizUrl>">

      <@field type="password" name="currentPassword" required=true id="currentPassword" maxlength="20" label=uiLabelMap.PartyOldPassword/>
      <@field type="password" name="newPassword" required=true id="newPassword" maxlength="20" label=uiLabelMap.PartyNewPassword/>
      <@field type="password" name="newPasswordVerify" required=true id="newPasswordVerify" maxlength="20" label=uiLabelMap.PartyNewPasswordVerify/>
      <@field type="input" maxlength="100" name="passwordHint" id="passwordHint" value=(userLoginData.passwordHint!) label=uiLabelMap.PartyPasswordHint/>
      
    </form>
</@section>
