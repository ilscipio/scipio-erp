<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#-- SCIPIO: This was a message to explain to "Go Back" kludge; however I have now recoded controller and screen
    to redirect automatically.
<@commonMsg type="info-important">${uiLabelMap.ShopSaveGoBackExplanation}</@commonMsg>-->

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" id="CommonGoBack1" href=makePageUrl(donePage) class="+${styles.action_nav_cancel!}" text=uiLabelMap.CommonGoBack />
        <@menuitem type="link" id="CommonSave1" href="javascript:document.getElementById('changepasswordform').submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
    </@menu>
</#macro>
<@section menuContent=menuContent menuLayoutGeneral="bottom"><#--title=uiLabelMap.PartyChangePassword-->
    <#assign partyId = request.getAttribute("pwdRecoveryPartyId")!>
    <#if partyId?has_content>
        <#assign updatePwdUrl><@pageUrl>updatePasswordFromHash?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</@pageUrl></#assign>
    <#else>
        <#assign updatePwdUrl><@pageUrl>updatePassword?DONE_PAGE=${donePage}&amp;targetPageResponse=redirect-done</@pageUrl></#assign>
    </#if>


    <#-- SCIPIO: NOTE: view-switch replaced with redirect -->
    <form id="changepasswordform" method="post" action="${updatePwdUrl}">
        <#if partyId?has_content>
            <#assign hash = request.getAttribute("hash")!>
            <input type="hidden" name="pwdRecoveryPartyId" value="${partyId}">
            <input type="hidden" name="hash" value="${hash}">
        <#else>
            <@field type="password" name="currentPassword" required=true id="currentPassword" maxlength="20" label=uiLabelMap.PartyOldPassword/>
        </#if>
        <@field type="password" name="newPassword" required=true id="newPassword" maxlength="20" label=uiLabelMap.PartyNewPassword/>
        <@field type="password" name="newPasswordVerify" required=true id="newPasswordVerify" maxlength="20" label=uiLabelMap.PartyNewPasswordVerify/>
        <@field type="input" maxlength="100" name="passwordHint" id="passwordHint" value=(userLoginData.passwordHint!) label=uiLabelMap.PartyPasswordHint/>
    </form>
</@section>
