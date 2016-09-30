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
<#include "catalogcommon.ftl">

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
          <@menuitem type="link" href=makeOfbizUrl("clearLastViewed") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.CommonClear />
          <#if (searchOptionsHistoryList?size > maxToShow)>
            <@menuitem type="link" href=makeOfbizUrl("advancedsearch") class="+${styles.action_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonMore />
          </#if>
        </@menu>
    </#macro>
    <@section id="minilastproductsearches" title="${rawString(uiLabelMap.OrderLastSearches)}..." menuContent=menuContent>
      <ul>
        <#list searchOptionsHistoryList[0..limit] as searchOptions>
          <#-- searchOptions type is ProductSearchSession.ProductSearchOptions -->
          <li>
          ${uiLabelMap.EcommerceSearchNumber} ${searchOptions_index + 1}
            <ul>
              <li>
                <a href="<@ofbizUrl>setCurrentSearchFromHistoryAndSearch?searchHistoryIndex=${searchOptions_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_find!}">${uiLabelMap.CommonSearch}</a>
                <a href="<@ofbizUrl>setCurrentSearchFromHistory?searchHistoryIndex=${searchOptions_index}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_find!}">${uiLabelMap.CommonRefine}</a>
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
