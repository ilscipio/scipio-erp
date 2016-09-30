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
<@section title="${rawString(uiLabelMap.ContentSearchContents)}, ${rawString(uiLabelMap.ProductSearchFor)}:">
    <#list searchConstraintStrings as searchConstraintString>
        <div><a href="<@ofbizUrl>ContentSearchResults?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
    </#list>
    <div><span>${uiLabelMap.CommonSortedBy}</span> ${searchSortOrderString}</div>
    <div><a href="<@ofbizUrl>ContentSearchOptions</@ofbizUrl>" class="${styles.link_nav!} ${styles.link_update!} ${styles.action_scope_session!}">${uiLabelMap.CommonRefineSearch}</a></div>

    <#if !contentIds?has_content>
      <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
    </#if>
    
  <#if contentIds?has_content>
    <#-- Scipio: FIXME: see ContentSearchEvents.java -->
    <p>NOTE: This search currently only shows the first ${listSize!0} results and supports no pagination.</p>
    <@paginate mode="content" url=makeOfbizUrl("ContentSearchResults") paramStr="/~clearSearch=N" paramDelim="/" paramPrefix="~" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
      <@table type="data-complex" width="100%"> <#-- orig: class="" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" -->
        <#assign listIndex = lowIndex>
        <#list contentIds as contentId><#-- note that there is no boundary range because that is being done before the list is put in the content -->
          <#assign content = delegator.findOne("Content", {"contentId":contentId}, false)>
          <@tr>
            <@td>
              <a href="<@ofbizUrl>editContent?contentId=${contentId}</@ofbizUrl>" class="${styles.link_nav_info_idname!}">${contentId} ${(content.contentName)!}</a>
            </@td>
          </@tr>
        </#list>
      </@table>
    </@paginate>
  </#if>

</@section>
