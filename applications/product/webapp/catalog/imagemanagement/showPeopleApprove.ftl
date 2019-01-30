<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script src=makeContentUrl("/images/imagemanagement/sizzle.min.js") />
<@script>
<#-- SCIPIO: this breaks everything (?)
jQuery.noConflict();-->
jQuery(document).ready(function(){
    jQuery('input:radio').click(function(){
        var elementVal = jQuery(this).val();
        var elementList = elementVal.split('__');
        var result = elementList[0] + "/" + elementList[1];
        jQuery('input:radio[value=' + result + ']').attr('checked',true);
    });
    jQuery('input:radio[value^="IM_APPROVED"]').each( function() {
        this.checked = true;
    });
});
</@script>
      
   
    
<#if partyRoles?has_content> 
        <#list partyRoles as partyRole>
          <@row>
            <@cell>
              <ul class="${styles.list_inline}">
                    <#assign userLoginApprovers  = delegator.findByAnd("UserLogin",{"partyId":partyRole.partyId})/>
                    <#assign userLoginApprover = userLoginApprovers[0]>
                    <#assign userLoginAndPartyDetails = delegator.findOne("UserLoginAndPartyDetails", {"partyId":userLoginApprover.partyId, "userLoginId":userLoginApprover.userLoginId}, false)!>
                    <#if userLoginAndPartyDetails?has_content>
                        <#assign partyContentDetail  = delegator.findByAnd("ContentApproval",{"roleTypeId":"IMAGEAPPROVER", "approvalStatusId":"IM_PENDING", "partyId":userLoginAndPartyDetails.partyId})/>
                        <#assign imageApproveSize = partyContentDetail.size()>
                        <#if userLoginAndPartyDetails.userLoginId == userLogin.userLoginId>
                            <#if userMap.checkUser == userLoginAndPartyDetails.userLoginId>
                                <li>
                                        <b>${userLoginAndPartyDetails.firstName!} ${userLoginAndPartyDetails.middleName!} ${userLoginAndPartyDetails.lastName!} (${imageApproveSize})</b>
                                </li>
                            <#else>
                                <li>
                                        <b><a href="<@pageUrl>ImageApprove</@pageUrl>" class="text">${userLoginAndPartyDetails.firstName!} ${userLoginAndPartyDetails.middleName!} ${userLoginAndPartyDetails.lastName!} (${imageApproveSize})</a></b>
                                </li>
                            </#if>
                        <#else>
                            <#if userMap.checkUser == userLoginAndPartyDetails.userLoginId>
                                <li>
                                        <b>${userLoginAndPartyDetails.firstName!} ${userLoginAndPartyDetails.middleName!} ${userLoginAndPartyDetails.lastName!} (${imageApproveSize})</b>
                                </li>
                            <#else>
                                <li>
                                        <b><a href="<@pageUrl>ListPeopleApproved?createdByUserLogin=${userLoginAndPartyDetails.userLoginId}</@pageUrl>" class="text">${userLoginAndPartyDetails.firstName!} ${userLoginAndPartyDetails.middleName!} ${userLoginAndPartyDetails.lastName!} (${imageApproveSize})</a></b>
                                </li>
                            </#if>
                        </#if>
                    </#if>
              </ul>
            </@cell>
          </@row>
        </#list>
        <#if userMap.checkUser == "REJECTED">
            <#-- SCIPIO: too confusing:
            <a href="javascript:void(0)" class="${styles.link_run_sys!} ${styles.action_find!} ${styles.disabled!}">Rejected</a>-->
        <#else>
            <a href="<@pageUrl>ListPeopleRejected</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_find!}">Rejected</a>
        </#if>
<#else>
  <@commonMsg type="result-norecord"/>
</#if>

