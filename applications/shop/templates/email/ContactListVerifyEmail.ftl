<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#-- custom logo or text can be inserted here -->
<h1>${title!}</h1>
<#if note??><p>${note}</p></#if>

<p>Hello ${partyName.firstName!} ${partyName.lastName!} ${partyName.groupName!}!</p>
<p>We have received a request for subscription to the ${contactList.contactListName} contact list.</p>
<p>To complete your subscription click the on the following link:</p>

<#assign verifyUrl = baseEcommerceSecureUrl+'/'+'updateContactListPartyNoUserLogin?contactListId='+contactListParty.contactListId+'&amp;partyId='+contactListParty.partyId+'&amp;fromDate='+contactListParty.fromDate+'&amp;statusId=CLPT_ACCEPTED&amp;optInVerifyCode='+contactListPartyStatus.optInVerifyCode+'&amp;baseLocation='+baseLocation!>
<#if (contactListParty.preferredContactMechId)??>
    <#assign verifyUrl= verifyUrl+"&amp;preferredContactMechId="+contactListParty.preferredContactMechId>
</#if>
<a href="${verifyUrl}">Please click here to verify your subscription.</a>
