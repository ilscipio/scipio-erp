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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<@section title="${rawLabel('WorkEffortSearchWorkEfforts')}, ${rawLabel('ProductSearchFor')}:">
    <#list searchConstraintStrings as searchConstraintString>
        <div>&nbsp;<a href="<@pageUrl>WorkEffortSearchResults?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@pageUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
    </#list>
    <div><span>${uiLabelMap.CommonSortedBy}</span> ${searchSortOrderString}</div>
    <div><a href="<@pageUrl>WorkEffortSearchOptions</@pageUrl>" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a></div>

    <#if !workEffortIds?has_content>
      <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
    <#else>

    <#-- SCIPIO: FIXME: the java in org.ofbiz.workeffort.workeffort.WorkEffortSearchEvents.getWorkEffortSearchResult doesn't actually support pagination; though this fails gracefully -->
    <@paginate mode="content" url=makePageUrl("WorkEffortSearchResults") paramStr="~clearSearch=N" paramDelim="/" paramPrefix="~" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
      <@table type="data-list" width="100%">
        <#assign listIndex = lowIndex>
        <#list workEffortIds as workEffortId><#-- note that there is no boundary range because that is being done before the list is put in the content -->
          <#assign workEffort = delegator.findOne("WorkEffort", {"workEffortId":workEffortId}, false)>
          <@tr>
            <@td>
              <a href="<@pageUrl>EditWorkEffort?workEffortId=${workEffortId}</@pageUrl>" class="${styles.link_nav_info_idname!}">${workEffortId} ${(workEffort.workEffortName)!}</a>
            </@td>
          </@tr>
        </#list>
      </@table>
    </@paginate>
    
    </#if>
</@section>
