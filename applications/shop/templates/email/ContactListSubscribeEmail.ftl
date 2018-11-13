<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<p>Hello ${partyName.firstName!} ${partyName.lastName!} ${partyName.groupName!}!</p>
<p>Successfully subscribed from ${contactList.contactListName} contact list.</p>

<#assign verifyUrl = baseEcommerceSecureUrl+'/'+'updateContactListPartyNoUserLogin?contactListId='+contactListParty.contactListId+'&amp;partyId='+contactListParty.partyId+'&amp;fromDate='+contactListParty.fromDate+'&amp;statusId=CLPT_UNSUBS_PENDING&amp;optInVerifyCode='+contactListPartyStatus.optInVerifyCode+'&amp;baseLocation='+baseLocation!>
<#if (contactListParty.preferredContactMechId)??>
    <#assign verifyUrl= verifyUrl+"&amp;preferredContactMechId="+contactListParty.preferredContactMechId>
</#if>
<a href="${verifyUrl}">If this was by mistake, click here to unsubscribe your subscription again.</a>
