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
<#assign menuHtml>
  <@menu type="section" inlineItems=true>
    <@menuitem type="link" href="/content/control/EditWebSite?productStoreId=${productStoreId}&amp;externalLoginKey=${requestAttributes.externalLoginKey}" text="${uiLabelMap.ProductCreateNewProductStoreWebSite}" />
  </@menu>
</#assign>
<@section title="${uiLabelMap.PageTitleEditProductStoreWebSites}" menuHtml=menuHtml>
        <@table type="data-list" autoAltRows=true cellspacing="0" class="${styles.table!}">
          <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.ProductWebSiteId}</@th>
              <@th>${uiLabelMap.ProductHost}</@th>
              <@th>${uiLabelMap.ProductPort}</@th>
              <@th>&nbsp;</@th>
            </@tr>
           </@thead>
         <#if storeWebSites?has_content>
           <@tbody>
              <#list storeWebSites as webSite>
                <@tr valign="middle">
                  <@td><a href="/content/control/EditWebSite?webSiteId=${webSite.webSiteId}&amp;externalLoginKey=${requestAttributes.externalLoginKey}" class="${styles.button_default!}">${webSite.siteName!} [${webSite.webSiteId}]</a></@td>
                  <@td>${webSite.httpHost?default('&nbsp;')}</@td>
                  <@td>${webSite.httpPort?default('&nbsp;')}</@td>
                  <@td align="center">
                    <a href="javascript:document.storeUpdateWebSite_${webSite_index}.submit();" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a>
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
<@section title="${uiLabelMap.ProductSetStoreOnWebSite}">
        <form name="addWebSite" action="<@ofbizUrl>storeUpdateWebSite</@ofbizUrl>" method="post">
            <input type="hidden" name="viewProductStoreId" value="${productStoreId}" />
            <input type="hidden" name="productStoreId" value="${productStoreId}" />
            <select name="webSiteId">
              <#list webSites as webSite>
                <option value="${webSite.webSiteId}">${webSite.siteName!} [${webSite.webSiteId}]</option>
              </#list>
            </select>
            <input type="submit" class="${styles.button_default!}" value="${uiLabelMap.CommonUpdate}" />
        </form>
</@section>
