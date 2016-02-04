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
<@section title="${uiLabelMap.PageTitleRequestItems}">
    <@table type="data-complex" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
    <@thead>
      <@tr class="header-row">
           <@th>
              ${uiLabelMap.CommonNbr}
           </@th>
           <@th colspan="2">
              ${uiLabelMap.CommonDescription}
           </@th>
           <@th>
           </@th>
      </@tr>
    </@thead>
    <#list custRequestItems as custRequestItemList>
    <#if custRequestItemList.custRequestItemSeqId?has_content>
        <@tr>
            <@td>
              <a href="<@ofbizUrl>requestitem?custRequestId=${custRequestItemList.custRequestId}&amp;custRequestItemSeqId=${custRequestItemList.custRequestItemSeqId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${custRequestItemList.custRequestItemSeqId}</a>
            </@td>
            <@td colspan="2">
              <#if custRequestItemList.story?has_content>
                <textarea readonly="readonly" rows="15" cols="72">${custRequestItemList.story}</textarea>
              </#if>
            </@td>
            
            <#-- now show notes details per line item -->
            <@td colspan="1" align="right" valign="top" width="50%" nowrap="nowrap" style="background-color:white; vertical-align: top;">
                <#if custRequestItemNoteViews?has_content>
                    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                        <@thead>
                        <@tr class="header-row">
                            <@th>
                            </@th>
                            <@th>
                                ${uiLabelMap.CommonNbr}
                            </@th>
                            <@th>
                                ${uiLabelMap.CommonNote}
                            </@th>
                            <@th>
                                ${uiLabelMap.PartyParty} ${uiLabelMap.PartyName}
                            </@th>
                            <@th>
                                ${uiLabelMap.CommonDate}
                            </@th>
                        </@tr>
                        </@thead>
                        <#list custRequestItemNoteViews as custRequestItemNoteViewList>
                            <#if custRequestItemNoteViewList.custRequestItemSeqId == custRequestItemList.custRequestItemSeqId>
                            <#assign partyNameView = delegator.findOne("PartyNameView", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", custRequestItemNoteViewList.partyId), false)!/>
                            <@tr>
                                <@td>
                                </@td>
                                <@td>
                                   ${custRequestItemNoteViewList.noteId}
                                </@td>
                                <@td>
                                   ${custRequestItemNoteViewList.noteInfo}
                                </@td>
                                <@td>
                                   ${partyNameView.groupName!} ${partyNameView.firstName!} ${partyNameView.lastName!}
                                </@td>
                                <@td>
                                   ${custRequestItemNoteViewList.noteDateTime.toString().substring(0,10)}
                                </@td>
                            </@tr>
                            </#if>
                        </#list>
                    </@table>
                </#if>
                <a href="<@ofbizUrl>requestitemnotes?custRequestId=${custRequestItemList.custRequestId}&amp;custRequestItemSeqId=${custRequestItemList.custRequestItemSeqId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.OrderAddNote}</a>
            </@td>
        </@tr>
    </#if>
    </#list>
    </@table>
</@section>