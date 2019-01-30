<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://product/webapp/catalog/store/storecommon.ftl">

<#if productStoreId?has_content>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <#-- SCIPIO: This is now accessible internally from product app: makeServerUrl("/content/control/EditWebSite...&externalLoginKey=${requestAttributes.externalLoginKey} -->
    <@menuitem type="link" href=makePageUrl("EditWebSite?productStoreId=${productStoreId}") text=uiLabelMap.ProductCreateNewProductStoreWebSite class="+${styles.action_nav!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleEditProductStoreWebSites menuContent=menuContent>
    <@webSiteWarnings webSiteList=(storeWebSites!)/>
        <@table type="data-list" autoAltRows=true>
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
                  <#-- SCIPIO: This is now accessible internally from product app: <@serverUrl>/content/control/EditWebSite...&amp;externalLoginKey=${requestAttributes.externalLoginKey} -->
                  <@td><a href="<@pageUrl>EditWebSite?webSiteId=${webSite.webSiteId}</@pageUrl>" class="${styles.link_nav_info_idname!}">${webSite.siteName!} [${webSite.webSiteId}]</a></@td>
                  <#-- SCIPIO: sort by HTTPS first, not HTTP -->
                  <@td>${webSite.httpsHost!webSite.httpHost!'&nbsp;'}</@td>
                  <@td>${webSite.httpsPort!webSite.httpPort!'&nbsp;'}</@td>
                  <@td>${webSite.isStoreDefault!}</@td><#-- SCIPIO -->
                  <@td align="center">
                    <a href="javascript:document.storeUpdateWebSite_${webSite_index}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                    <form name="storeUpdateWebSite_${webSite_index}" method="post" action="<@pageUrl>storeUpdateWebSite</@pageUrl>">
                        <input type="hidden" name="viewProductStoreId" value="${productStoreId}"/>
                        <input type="hidden" name="productStoreId" value=""/>
                        <input type="hidden" name="webSiteId" value="${webSite.webSiteId}"/>
                    </form>
                    
                    <a href="javascript:document.setProductStoreDefaultWebSite_${webSite_index}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSetDefault}</a>
                    <form name="setProductStoreDefaultWebSite_${webSite_index}" method="post" action="<@pageUrl>setProductStoreDefaultWebSite</@pageUrl>">
                        <input type="hidden" name="productStoreId" value="${productStoreId}"/>
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
        <form name="addWebSite" action="<@pageUrl>storeUpdateWebSite</@pageUrl>" method="post">
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
