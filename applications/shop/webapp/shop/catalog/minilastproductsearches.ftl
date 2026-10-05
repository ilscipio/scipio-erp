<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
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
