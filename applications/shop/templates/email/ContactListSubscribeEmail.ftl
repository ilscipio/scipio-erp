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

<p>Hello ${partyName.firstName!} ${partyName.lastName!} ${partyName.groupName!}!</p>
<p>Successfully subscribed from ${contactList.contactListName} contact list.</p>

<#assign verifyUrl = baseEcommerceSecureUrl+'/'+'updateContactListPartyNoUserLogin?contactListId='+contactListParty.contactListId+'&amp;partyId='+contactListParty.partyId+'&amp;fromDate='+contactListParty.fromDate+'&amp;statusId=CLPT_UNSUBS_PENDING&amp;optInVerifyCode='+contactListPartyStatus.optInVerifyCode+'&amp;baseLocation='+baseLocation!>
<#if (contactListParty.preferredContactMechId)??>
    <#assign verifyUrl= verifyUrl+"&amp;preferredContactMechId="+contactListParty.preferredContactMechId>
</#if>
<a href="${verifyUrl}">If this was by mistake, click here to unsubscribe your subscription again.</a>
