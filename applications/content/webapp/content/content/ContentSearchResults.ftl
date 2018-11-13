<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section title="${rawLabel('ContentSearchContents')}, ${rawLabel('ProductSearchFor')}:">
    <#list searchConstraintStrings as searchConstraintString>
        <div><a href="<@ofbizUrl>ContentSearchResults?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
    </#list>
    <div><span>${uiLabelMap.CommonSortedBy}</span> ${searchSortOrderString}</div>
    <div><a href="<@ofbizUrl>ContentSearchOptions</@ofbizUrl>" class="${styles.link_nav!} ${styles.link_update!} ${styles.action_scope_session!}">${uiLabelMap.CommonRefineSearch}</a></div>

    <#if !contentIds?has_content>
      <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
    </#if>
    
  <#if contentIds?has_content>
    <#-- SCIPIO: FIXME: see ContentSearchEvents.java -->
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
