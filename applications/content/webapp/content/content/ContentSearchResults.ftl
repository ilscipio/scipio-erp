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
<@section title="${uiLabelMap.ContentSearchContents}, ${uiLabelMap.ProductSearchFor}:">
    <#list searchConstraintStrings as searchConstraintString>
        <div><a href="<@ofbizUrl>ContentSearchResults?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_action_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
    </#list>
    <div><span>${uiLabelMap.CommonSortedBy}</span> ${searchSortOrderString}</div>
    <div><a href="<@ofbizUrl>ContentSearchOptions</@ofbizUrl>" class="${styles.link_nav!} ${styles.link_update!} ${styles.action_scope_session!}">${uiLabelMap.CommonRefineSearch}</a></div>

    <#if !contentIds?has_content>
      <@resultMsg>${uiLabelMap.ProductNoResultsFound}.</@resultMsg>
    </#if>

    <#macro paginateContentResults>
      <#if (0 < listSize?int)>
      <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("ContentSearchResults/~VIEW_INDEX=${viewIndex-1}/~VIEW_SIZE=${viewSize}/~clearSearch=N") text="${uiLabelMap.CommonPrevious}" disabled=(!(0 < viewIndex?int)) />
        <@menuitem type="text" text="${lowIndex+1} - ${highIndex} ${uiLabelMap.CommonOf} ${listSize}" />        
        <@menuitem type="link" href=makeOfbizUrl("ContentSearchResults/~VIEW_INDEX=${viewIndex+1}/~VIEW_SIZE=${viewSize}/~clearSearch=N") text="${uiLabelMap.CommonNext}" disabled=(!(highIndex?int < listSize?int)) />
      </@menu>
      </#if>
    </#macro>
    
    <#if contentIds?has_content>
    
    <@paginateContentResults />
    <center>
      <@table type="data-complex" width="100%" cellpadding="0" cellspacing="0"> <#-- orig: class="" -->
        <#assign listIndex = lowIndex>
        <#list contentIds as contentId><#-- note that there is no boundary range because that is being done before the list is put in the content -->
          <#assign content = delegator.findOne("Content", Static["org.ofbiz.base.util.UtilMisc"].toMap("contentId", contentId), false)>
          <@tr>
            <@td>
              <a href="<@ofbizUrl>editContent?contentId=${contentId}</@ofbizUrl>" class="${styles.link_nav_record_idname!}">${contentId} ${(content.contentName)!}</a>
            </@td>
          </@tr>
        </#list>
      </@table>
    </center>
    <@paginateContentResults />
    
    </#if>

</@section>
