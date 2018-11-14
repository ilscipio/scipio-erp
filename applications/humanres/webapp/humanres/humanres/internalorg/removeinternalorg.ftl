<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign partyGroup = delegator.findOne("PartyGroup", {"partyId" : parameters.partyId!}, true)/>
<#if partyGroup?has_content>
    <#assign partyname = partyGroup.groupName!/>
</#if>
<@script>
    var answer = confirm ("Are you sure you want to remove '<#if partyname??>${partyname}<#else>${parameters.partyId!}</#if>'?")
    if (answer)
       document.removeInternalOrg.submit();
    else
       window.close();
</@script>
<div id="rmvinternalorg" title="Remove Internal Organization">
    <form name="removeInternalOrg" method="post" action="<@ofbizUrl>removeInternalOrg</@ofbizUrl>">
        <input type="hidden" name="partyId" value="${parameters.partyId!}"/>
        <input type="hidden" name="parentpartyId" value="${parameters.parentpartyId!}"/>
    </form>
</div>