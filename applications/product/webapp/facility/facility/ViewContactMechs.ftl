<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<div>
  <#if contactMeches?has_content>
    <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->      
      <@thead>
        <@tr>
           <@th>${uiLabelMap.PartyContactType}</@th>
            <@th>${uiLabelMap.PartyContactInformation}</@th>
            <@th colspan=4></@th>            
        </@tr>
      </@thead>
      <@tbody>
          <#list contactMeches as contactMechMap>
              <#assign contactMech = contactMechMap.contactMech>
              <#assign facilityContactMech = contactMechMap.facilityContactMech>
              <@tr>
                <@td valign="top">
                  ${contactMechMap.contactMechType.get("description",locale)}
                </@td>
                <@td valign="top">
                  <#list contactMechMap.facilityContactMechPurposes as facilityContactMechPurpose>
                      <#assign contactMechPurposeType = facilityContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                          <#if contactMechPurposeType?has_content>
                            <b>${contactMechPurposeType.get("description",locale)}</b>
                          <#else>
                            <b>${uiLabelMap.ProductPurposeTypeNotFoundWithId}: "${facilityContactMechPurpose.contactMechPurposeTypeId}"</b>
                          </#if>
                          <#if facilityContactMechPurpose.thruDate?has_content>
                          (${uiLabelMap.CommonExpire}: ${facilityContactMechPurpose.thruDate.toString()})
                          </#if>
                          <br />
                  </#list>
                  <#if "POSTAL_ADDRESS" == contactMech.contactMechTypeId>
                      <#assign postalAddress = contactMechMap.postalAddress>
                        <#if postalAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b> ${postalAddress.toName}<br /></#if>
                        <#if postalAddress.attnName?has_content><b>${uiLabelMap.CommonAttn}:</b> ${postalAddress.attnName}<br /></#if>
                        ${postalAddress.address1!}<br />
                        <#if postalAddress.address2?has_content>${postalAddress.address2!}<br /></#if>
                        ${postalAddress.city!},
                        ${postalAddress.stateProvinceGeoId!}
                        ${postalAddress.postalCode!}
                        <#if postalAddress.countryGeoId?has_content><br />${postalAddress.countryGeoId}</#if>
                      <#if postalAddress.geoPointId?has_content>
                        <#if contactMechPurposeType?has_content>
                          <#assign popUptitle = contactMechPurposeType.get("description",locale) + uiLabelMap.CommonGeoLocation>
                        </#if>
                        <br /><a href="javascript:popUp('<@ofbizUrl>GetPartyGeoLocation?geoPointId=${postalAddress.geoPointId}&partyId=${facility.ownerPartyId}</@ofbizUrl>', '${popUptitle!?html}', '450', '550')" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.CommonGeoLocation}</a>
                      </#if>
                  <#elseif "TELECOM_NUMBER" == contactMech.contactMechTypeId>
                      <#assign telecomNumber = contactMechMap.telecomNumber!>
                        ${telecomNumber.countryCode!}
                        <#if telecomNumber.areaCode?has_content>${telecomNumber.areaCode}-</#if>${telecomNumber.contactNumber!}
                        <#if facilityContactMech.extension?has_content>${uiLabelMap.CommonExt} ${facilityContactMech.extension}</#if>                        
                  <#elseif "EMAIL_ADDRESS" == contactMech.contactMechTypeId>
                        ${contactMech.infoString!}
                        <a href="mailto:${contactMech.infoString!}" class="${styles.link_run_sys!} ${styles.action_send!} ${styles.action_external!}">${uiLabelMap.CommonSendEmail}</a>
                  <#elseif "WEB_ADDRESS" == contactMech.contactMechTypeId>
                        ${contactMech.infoString!}
                        <#assign openAddress = contactMech.infoString?default("")>
                        <#if !openAddress?starts_with("http") && !openAddress?starts_with("HTTP")><#assign openAddress = "http://" + openAddress></#if>
                        <a target="_blank" href="${openAddress}" class="${styles.link_nav!}">((${uiLabelMap.CommonOpenPageNewWindow})</a>
                  <#else>
                        ${contactMech.infoString!}
                  </#if>
                  <br />(${uiLabelMap.CommonUpdated}: ${facilityContactMech.fromDate.toString()})
                  <#if facilityContactMech.thruDate?has_content><br /><b>${uiLabelMap.CommonUpdatedEffectiveThru}:&nbsp;${facilityContactMech.thruDate.toString()}</b></#if>
                </@td>
                <@td class="button-col">
                  &nbsp;
                  <#if security.hasEntityPermission("FACILITY", "_UPDATE", session)>
                    <a href="<@ofbizUrl>EditContactMech?facilityId=${facilityId}&amp;contactMechId=${contactMech.contactMechId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                  </#if>
                </@td>
                <@td>
                  <#if security.hasEntityPermission("FACILITY", "_DELETE", session)>
                    <form action="<@ofbizUrl>deleteContactMech/ViewContactMechs</@ofbizUrl>" name="deleteContactForm_${contactMechMap_index}" method="post">
                      <input type="hidden" name="facilityId" value="${facilityId!}"/>
                      <input type="hidden" name="contactMechId" value="${contactMech.contactMechId!}"/>
                    </form>
                    <a href="javascript:document.deleteContactForm_${contactMechMap_index}.submit()" class="${styles.link_run_sys!} ${styles.action_terminate!}">${uiLabelMap.CommonExpire}</a>
                  </#if>
    
                </@td>
              </@tr>
          </#list>
        </@tbody>
    </@table>
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.CommonNoContactInformationOnFile}.</@commonMsg>
  </#if>
</div>
