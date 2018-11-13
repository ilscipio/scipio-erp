<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@section title=uiLabelMap.ProductProductSearch>

<#-- SCIPIO: ToDo: reimplement refined search
<@row>
    <@cell>
    ${uiLabelMap.ProductYouSearchedFor}
    <#list searchConstraintStrings as searchConstraintString>
      <a href="<@ofbizUrl>keywordsearch?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</li>
    </#list>
    </@cell>
</@row>-->

<@row>
    <@cell>
        ${uiLabelMap.CommonSortedBy}: ${searchSortOrderString} (<a href="<@ofbizUrl>advancedsearch?SEARCH_CATEGORY_ID=${(requestParameters.SEARCH_CATEGORY_ID)!}</@ofbizUrl>" class="${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a>)
    </@cell>
</@row>

<#if !productIds?has_content>
  <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
<#else>
  <#-- SCIPIO: 2017-08-16: special pagination check. in legacy code this basically was always true. -->
  <#if !pagingEnabled?? || !pagingEnabled?is_boolean>
    <#if (paging!) == "Y">
      <#assign pagingEnabled = true>
    <#elseif (paging!) == "N">
      <#assign pagingEnabled = false>
    <#else>
      <#assign pagingEnabled = ((viewSize!1) > 0)><#-- (default is enabled) -->
    </#if>
  </#if>
  <@paginate enabled=pagingEnabled mode="content" url=makeOfbizUrl("keywordsearch") paramStr="~clearSearch=N" paramDelim="/" paramPrefix="~" viewSize=(viewSize!1) viewIndex=(viewIndex!0) listSize=(listSize!0)>
    <@grid columns=4>    
        <#list productIds as productId> 
            <#-- note that there is no boundary range because that is being done before the list is put in the content -->
            <li><@render resource=productsummaryScreen reqAttribs={"optProductId":productId, "listIndex":productId_index}/></li>
        </#list>
    </@grid>
  </@paginate>
</#if>

</@section>
