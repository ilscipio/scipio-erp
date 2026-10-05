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
<div>
    ${content}
</div>
<#assign verifyUrl = baseEcommerceSecureUrl+'/'+"updateContactListPartyNoUserLogin" />
<form method="post" action="${verifyUrl}">
    <fieldset>
        <label>E-mail: ${emailAddress}</label>
        <input type="hidden" name="contactListId" value="${contactListId}" />
        <input type="hidden" name="partyId" value="${partyId}" />
        <input type="hidden" name="preferredContactMechId" value="${preferredContactMechId!}" />
        <input type="hidden" name="fromDate" value="${fromDate}" />
        <input type="hidden" name="statusId" value="CLPT_UNSUBS_PENDING" />
        <input type="hidden" name="optInVerifyCode" value="${optInVerifyCode!}" />
        <input type="submit" name="submitButton" value="Click here to unsubscribe your newsletter subscription." class="${styles.link_run_sys!} ${styles.action_remove!}" />
    </fieldset>
</form>