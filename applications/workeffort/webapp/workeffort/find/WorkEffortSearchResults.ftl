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
<@section title="${rawString(uiLabelMap.WorkEffortSearchWorkEfforts)}, ${rawString(uiLabelMap.ProductSearchFor)}:">
    <#list searchConstraintStrings as searchConstraintString>
        <div>&nbsp;<a href="<@ofbizUrl>WorkEffortSearchResults?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
    </#list>
    <div><span>${uiLabelMap.CommonSortedBy}</span> ${searchSortOrderString}</div>
    <div><a href="<@ofbizUrl>WorkEffortSearchOptions</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a></div>

    <#if !workEffortIds?has_content>
      <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
    <#else>

    <#-- Scipio: FIXME: the java in org.ofbiz.workeffort.workeffort.WorkEffortSearchEvents.getWorkEffortSearchResult doesn't actually support pagination; though this fails gracefully -->
    <@paginate mode="content" url=makeOfbizUrl("WorkEffortSearchResults") paramStr="~clearSearch=N" paramDelim="/" paramPrefix="~" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
      <@table type="data-list" width="100%"> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" -->
        <#assign listIndex = lowIndex>
        <#list workEffortIds as workEffortId><#-- note that there is no boundary range because that is being done before the list is put in the content -->
          <#assign workEffort = delegator.findOne("WorkEffort", {"workEffortId":workEffortId}, false)>
          <@tr>
            <@td>
              <a href="<@ofbizUrl>EditWorkEffort?workEffortId=${workEffortId}</@ofbizUrl>" class="${styles.link_nav_info_idname!}">${workEffortId} ${(workEffort.workEffortName)!}</a>
            </@td>
          </@tr>
        </#list>
      </@table>
    </@paginate>
    
    </#if>
</@section>
