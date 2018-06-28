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

  <#assign pcntListReadOnly = (pcntListReadOnly!false) == true><#-- SCIPIO -->
  
  <@section id="partyContentList">
      <#if partyContent?has_content>
        <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@tbody>
          <#list partyContent as pContent>
            <#assign content = pContent.getRelatedOne("Content", false)>
            <#assign contentType = content.getRelatedOne("ContentType", true)>
            <#assign mimeType = content.getRelatedOne("MimeType", true)!>
          <#if (pcntListShowStatus!true) == true>
            <#assign status = content.getRelatedOne("StatusItem", true)!>
          </#if>
            <#assign pcType = pContent.getRelatedOne("PartyContentType", false)>
            <@tr>
              <#-- SCIPIO: for inter-app linking 
                  TODO: REVIEW: it might be sane to assume the default for pcntListEditInterApp to be true instead of false... -->
              <#assign pcntListEditUri>EditPartyContents?contentId=${pContent.contentId}&amp;partyId=${pContent.partyId}&amp;partyContentTypeId=${pContent.partyContentTypeId}&amp;fromDate=${pContent.fromDate}</#assign>
              <#if (pcntListEditInterApp!false) == true>
                <#assign pcntListEditLink><@ofbizInterWebappUrl extLoginKey=true>/partymgr/control/${pcntListEditUri}</@ofbizInterWebappUrl></#assign>
              <#else>
                <#assign pcntListEditLink><@ofbizUrl>${pcntListEditUri}</@ofbizUrl></#assign>
              </#if>
              <@td class="button-col"><a href="${pcntListEditLink}">${content.contentId}</a></@td>
              
            <#if !pcntPartyContentTypeId?has_content>
              <@td>${(pcType.get("description", locale))!}</@td>
            </#if>
            
              <@td>${content.contentName!}</@td>
              <#-- take too much space -->
              <#--<@td>${(contentType.get("description",locale))!}</@td>-->
              <#--<@td>${(mimeType.description)!}</@td>-->
            <#if (pcntListShowStatus!true) == true>
              <@td>${(status.get("description",locale))!}</@td>
            </#if>
              <@td>${pContent.fromDate!}</@td>
              <@td class="button-col">
                <#-- SCIPIO: 2018-04-10: the img? request is old and has issues with permissions and denies admins
                <a href="<@ofbizUrl>img<#if (content.contentName?has_content)>/${content.contentName}</#if>?imgId=${(content.dataResourceId)!}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>-->
                <a href="<@ofbizUrl>stream?contentId=${(content.contentId)!}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
                
              <#if !pcntListReadOnly>
                <#assign pcntListRemoveUri = pcntListRemoveUri!("removePartyContent/"+rawString(pcntListRemoveDonePage!"viewprofile"))><#-- SCIPIO -->
                <#-- SCIPIO: TODO: WARN: this only removes the association, not the content itself! -->
                <form name="removePartyContent_${pContent_index}" method="post" action="<@ofbizUrl uri=pcntListRemoveUri escapeAs='html'/>">
                  <input type="hidden" name="contentId" value="${pContent.contentId}" />
                  <input type="hidden" name="partyId" value="${pContent.partyId}" />
                  <input type="hidden" name="partyContentTypeId" value="${pContent.partyContentTypeId}" />
                  <input type="hidden" name="fromDate" value="${pContent.fromDate}" />
                  <#if pcntListRemoveExtraParams?has_content><#-- SCIPIO -->
                    <#assign pcntListRemoveExtraParams = toSimpleMap(pcntListRemoveExtraParams)>
                    <#list pcntListRemoveExtraParams?keys as paramName>
                      <input type="hidden" name="${paramName}" value="${escapeVal(pcntListRemoveExtraParams[rawString(paramName)]!, 'html')}"/>
                    </#list>
                  </#if>
                  <a href="javascript:document.removePartyContent_${pContent_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
                </form>
              </#if>
              
              </@td>
            </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoContent}</@commonMsg>
      </#if>
  </@section>