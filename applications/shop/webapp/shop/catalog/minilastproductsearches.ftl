<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign maxToShow = 4/>
<#assign searchOptionsHistoryList = Static["org.ofbiz.product.product.ProductSearchSession"].getSearchOptionsHistoryList(session)!/>
<#if searchOptionsHistoryList?has_content>
    <#if (searchOptionsHistoryList?size > maxToShow)>
      <#assign limit = maxToShow/>
    <#else>
      <#assign limit = (searchOptionsHistoryList?size-1)/>
    </#if>
    <#macro menuContent menuArgs={}>
        <@menu args=menuArgs>
          <@menuitem type="link" href=makePageUrl("clearLastViewed") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.CommonClear />
          <#if (searchOptionsHistoryList?size > maxToShow)>
            <@menuitem type="link" href=makePageUrl("advancedsearch") class="+${styles.action_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonMore />
          </#if>
        </@menu>
    </#macro>
    <@section id="minilastproductsearches" title="${rawLabel('OrderLastSearches')}..." menuContent=menuContent>
      <ul>
        <#list searchOptionsHistoryList[0..limit] as searchOptions>
          <#-- searchOptions type is ProductSearchSession.ProductSearchOptions -->
          <li>
          ${uiLabelMap.EcommerceSearchNumber} ${searchOptions_index + 1}
            <ul>
              <li>
                <a href="<@pageUrl>setCurrentSearchFromHistoryAndSearch?searchHistoryIndex=${searchOptions_index}&amp;clearSearch=N</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_find!}">${uiLabelMap.CommonSearch}</a>
                <a href="<@pageUrl>setCurrentSearchFromHistory?searchHistoryIndex=${searchOptions_index}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_find!}">${uiLabelMap.CommonRefine}</a>
              </li>
              <#assign constraintStrings = searchOptions.searchGetConstraintStrings(false, delegator, locale)>
              <#list constraintStrings as constraintString>
                <li>${constraintString}</li>
                </#list>
            </ul>
          </li>
        </#list>
      </ul>
    </@section>
</#if>
