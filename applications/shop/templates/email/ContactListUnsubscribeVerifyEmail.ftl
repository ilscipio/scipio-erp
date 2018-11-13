<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
   
<#-- custom logo or text can be inserted here -->
<h1>${title!}</h1>
<#if note??><p>${note}</p></#if>

<p>Hello ${partyName.firstName!} ${partyName.lastName!} ${partyName.groupName!}!</p>
<p>We have received a request for unsubscription to the ${contactList.contactListName} contact list.</p>
<p>To complete your unsubscription click the on the following link:</p>

<#assign verifyUrl = baseEcommerceSecureUrl+'/'+'contactListOptOut?contactListId='+contactListParty.contactListId+'&amp;communicationEventId='+communicationEventId!+'&amp;partyId='+contactListParty.partyId+'&amp;fromDate='+contactListParty.fromDate+'&amp;statusId=CLPT_UNSUBSCRIBED&amp;optInVerifyCode='+contactListPartyStatus.optInVerifyCode>
<#if (contactListParty.preferredContactMechId)??>
    <#assign verifyUrl= verifyUrl+"&amp;preferredContactMechId="+contactListParty.preferredContactMechId>
</#if>
<a href="${verifyUrl}">Please click here to verify your unsubscription.</a>
