<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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