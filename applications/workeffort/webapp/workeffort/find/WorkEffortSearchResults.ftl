<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title="${rawLabel('WorkEffortSearchWorkEfforts')}, ${rawLabel('ProductSearchFor')}:">
    <#list searchConstraintStrings as searchConstraintString>
        <div>&nbsp;<a href="<@ofbizUrl>WorkEffortSearchResults?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
    </#list>
    <div><span>${uiLabelMap.CommonSortedBy}</span> ${searchSortOrderString}</div>
    <div><a href="<@ofbizUrl>WorkEffortSearchOptions</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a></div>

    <#if !workEffortIds?has_content>
      <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
    <#else>

    <#-- SCIPIO: FIXME: the java in org.ofbiz.workeffort.workeffort.WorkEffortSearchEvents.getWorkEffortSearchResult doesn't actually support pagination; though this fails gracefully -->
    <@paginate mode="content" url=makeOfbizUrl("WorkEffortSearchResults") paramStr="~clearSearch=N" paramDelim="/" paramPrefix="~" viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=listSize!0>
      <@table type="data-list" width="100%">
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
