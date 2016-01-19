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

<div>
  <#if contactMeches?has_content>
    <@table type="data-list" cellspacing="0"> <#-- orig: class="basic-table" -->
      <#list contactMeches as contactMechMap>
          <#assign contactMech = contactMechMap.contactMech>
          <#assign facilityContactMech = contactMechMap.facilityContactMech>
          <@tr type="util"><@td colspan="3"><hr/></@td></@tr>
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
              <#if "POSTAL_ADDRESS" = contactMech.contactMechTypeId>
                  <#assign postalAddress = contactMechMap.postalAddress>
                    <#if postalAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b> ${postalAddress.toName}<br /></#if>
                    <#if postalAddress.attnName?has_content><b>${uiLabelMap.CommonAttn}:</b> ${postalAddress.attnName}<br /></#if>
                    ${postalAddress.address1!}<br />
                    <#if postalAddress.address2?has_content>${postalAddress.address2!}<br /></#if>
                    ${postalAddress.city!},
                    ${postalAddress.stateProvinceGeoId!}
                    ${postalAddress.postalCode!}
                    <#if postalAddress.countryGeoId?has_content><br />${postalAddress.countryGeoId}</#if>
                  <#if (postalAddress?has_content && !postalAddress.countryGeoId?has_content) || postalAddress.countryGeoId = "USA">
                      <#assign addr1 = postalAddress.address1!>
                      <#if (addr1.indexOf(" ") > 0)>
                        <#assign addressNum = addr1.substring(0, addr1.indexOf(" "))>
                        <#assign addressOther = addr1.substring(addr1.indexOf(" ")+1)>
                        <br /><a target='_blank' href='${uiLabelMap.CommonLookupWhitepagesAddressLink}' class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                      </#if>
                  </#if>
                  <#if postalAddress.geoPointId?has_content>
                    <#if contactMechPurposeType?has_content>
                      <#assign popUptitle = contactMechPurposeType.get("description",locale) + uiLabelMap.CommonGeoLocation>
                    </#if>
                    <br /><a href="javascript:popUp('<@ofbizUrl>geoLocation?geoPointId=${postalAddress.geoPointId}</@ofbizUrl>', '${popUptitle!}', '450', '550')" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.CommonGeoLocation}</a>
                  </#if>
              <#elseif "TELECOM_NUMBER" = contactMech.contactMechTypeId>
                  <#assign telecomNumber = contactMechMap.telecomNumber!>
                    ${telecomNumber.countryCode!}
                    <#if telecomNumber.areaCode?has_content>${telecomNumber.areaCode}-</#if>${telecomNumber.contactNumber!}
                    <#if facilityContactMech.extension?has_content>${uiLabelMap.CommonExt} ${facilityContactMech.extension}</#if>
                    <#if (telecomNumber?has_content && !telecomNumber.countryCode?has_content) || telecomNumber.countryCode! = "011">
                      <br /><a target='_blank' href='${uiLabelMap.CommonLookupAnywhoLink}' class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupAnywho}</a>
                      <a target='_blank' href='${uiLabelMap.CommonLookupWhitepagesTelNumberLink}' class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                    </#if>
              <#elseif "EMAIL_ADDRESS" = contactMech.contactMechTypeId>
                    ${contactMech.infoString!}
                    <a href='mailto:${contactMech.infoString!}' class="${styles.link_run_sys!} ${styles.action_send!} ${styles.action_external!}">${uiLabelMap.CommonSendEmail}</a>
              <#elseif "WEB_ADDRESS" = contactMech.contactMechTypeId>
                    ${contactMech.infoString!}
                    <#assign openAddress = contactMech.infoString?default("")>
                    <#if !openAddress?starts_with("http") && !openAddress?starts_with("HTTP")><#assign openAddress = "http://" + openAddress></#if>
                    <a target='_blank' href='${openAddress}' class="${styles.link_nav!}">((${uiLabelMap.CommonOpenPageNewWindow})</a>
              <#else>
                    ${contactMech.infoString!}
              </#if>
              <br />(${uiLabelMap.CommonUpdated}: ${facilityContactMech.fromDate.toString()})
              <#if facilityContactMech.thruDate?has_content><br /><b>${uiLabelMap.CommonUpdatedEffectiveThru}:&nbsp;${facilityContactMech.thruDate.toString()}</b></#if>
            </@td>
            <@td class="button-col">
              &nbsp;
              <#if security.hasEntityPermission("FACILITY", "_UPDATE", session)>
                <a href='<@ofbizUrl>EditContactMech?facilityId=${facilityId}&amp;contactMechId=${contactMech.contactMechId}</@ofbizUrl>' class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
              </#if>
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
    </@table>
  <#else>
    <@resultMsg>${uiLabelMap.CommonNoContactInformationOnFile}.</@resultMsg>
  </#if>
</div>
