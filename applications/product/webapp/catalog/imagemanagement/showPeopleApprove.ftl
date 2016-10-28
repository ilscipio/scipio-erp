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
<@script src=makeOfbizContentUrl("/images/imagemanagement/sizzle.min.js") />
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
                                        <b><a href="<@ofbizUrl>ImageApprove</@ofbizUrl>" class="text">${userLoginAndPartyDetails.firstName!} ${userLoginAndPartyDetails.middleName!} ${userLoginAndPartyDetails.lastName!} (${imageApproveSize})</a></b>
                                </li>
                            </#if>
                        <#else>
                            <#if userMap.checkUser == userLoginAndPartyDetails.userLoginId>
                                <li>
                                        <b>${userLoginAndPartyDetails.firstName!} ${userLoginAndPartyDetails.middleName!} ${userLoginAndPartyDetails.lastName!} (${imageApproveSize})</b>
                                </li>
                            <#else>
                                <li>
                                        <b><a href="<@ofbizUrl>ListPeopleApproved?createdByUserLogin=${userLoginAndPartyDetails.userLoginId}</@ofbizUrl>" class="text">${userLoginAndPartyDetails.firstName!} ${userLoginAndPartyDetails.middleName!} ${userLoginAndPartyDetails.lastName!} (${imageApproveSize})</a></b>
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
            <a href="<@ofbizUrl>ListPeopleRejected</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_find!}">Rejected</a>
        </#if>
<#else>
  <@commonMsg type="result-norecord"/>
</#if>

