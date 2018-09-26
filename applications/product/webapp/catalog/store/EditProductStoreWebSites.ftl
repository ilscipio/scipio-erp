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
<#if productStoreId?has_content>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <#-- SCIPIO: This is now accessible internally from product app: makeOfbizInterWebappUrl("/content/control/EditWebSite...&externalLoginKey=${requestAttributes.externalLoginKey} -->
    <@menuitem type="link" href=makeOfbizUrl("EditWebSite?productStoreId=${productStoreId}") text=uiLabelMap.ProductCreateNewProductStoreWebSite class="+${styles.action_nav!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleEditProductStoreWebSites menuContent=menuContent>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.ProductWebSiteId}</@th>
              <@th>${uiLabelMap.ProductHost}</@th>
              <@th>${uiLabelMap.ProductPort}</@th>
              <@th>${uiLabelMap.FormFieldTitle_isStoreDefault}</@th>
              <@th>&nbsp;</@th>
            </@tr>
           </@thead>
         <#if storeWebSites?has_content>
           <@tbody>
              <#list storeWebSites as webSite>
                <@tr valign="middle">
                  <#-- SCIPIO: This is now accessible internally from product app: <@ofbizInterWebappUrl>/content/control/EditWebSite...&amp;externalLoginKey=${requestAttributes.externalLoginKey} -->
                  <@td><a href="<@ofbizUrl>EditWebSite?webSiteId=${webSite.webSiteId}</@ofbizUrl>" class="${styles.link_nav_info_idname!}">${webSite.siteName!} [${webSite.webSiteId}]</a></@td>
                  <#-- SCIPIO: sort by HTTPS first, not HTTP -->
                  <@td>${webSite.httpsHost!webSite.httpHost!'&nbsp;'}</@td>
                  <@td>${webSite.httpsPort!webSite.httpPort!'&nbsp;'}</@td>
                  <@td>${webSite.isStoreDefault!}</@td><#-- SCIPIO -->
                  <@td align="center">
                    <a href="javascript:document.storeUpdateWebSite_${webSite_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                    <form name="storeUpdateWebSite_${webSite_index}" method="post" action="<@ofbizUrl>storeUpdateWebSite</@ofbizUrl>">
                        <input type="hidden" name="viewProductStoreId" value="${productStoreId}"/>
                        <input type="hidden" name="productStoreId" value=""/>
                        <input type="hidden" name="webSiteId" value="${webSite.webSiteId}"/>
                    </form>                      
                  </@td>
                </@tr>
              </#list>
            </@tbody>
          </#if>
        </@table>
</@section>
<@section title=uiLabelMap.ProductSetStoreOnWebSite>
        <form name="addWebSite" action="<@ofbizUrl>storeUpdateWebSite</@ofbizUrl>" method="post">
            <input type="hidden" name="viewProductStoreId" value="${productStoreId}" />
            <input type="hidden" name="productStoreId" value="${productStoreId}" />
            <select name="webSiteId">
              <#list webSites as webSite>
                <option value="${webSite.webSiteId}">${webSite.siteName!} [${webSite.webSiteId}]</option>
              </#list>
            </select>
            <input type="submit" class="${styles.link_run_sys!} ${styles.action_update!}" value="${uiLabelMap.CommonUpdate}" />
        </form>
</@section>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderProductStoreNotExist}</@commonMsg>
</#if>
