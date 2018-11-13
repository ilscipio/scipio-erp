<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#include "component://product/webapp/catalog/store/storecommon.ftl">

<#if productStoreId?has_content>
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <#-- SCIPIO: This is now accessible internally from product app: makeOfbizInterWebappUrl("/content/control/EditWebSite...&externalLoginKey=${requestAttributes.externalLoginKey} -->
    <@menuitem type="link" href=makeOfbizUrl("EditWebSite?productStoreId=${productStoreId}") text=uiLabelMap.ProductCreateNewProductStoreWebSite class="+${styles.action_nav!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section title=uiLabelMap.PageTitleEditProductStoreWebSites menuContent=menuContent>
    <@webSiteWarnings webSiteList=(storeWebSites!)/>
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
                    
                    <a href="javascript:document.setProductStoreDefaultWebSite_${webSite_index}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSetDefault}</a>
                    <form name="setProductStoreDefaultWebSite_${webSite_index}" method="post" action="<@ofbizUrl>setProductStoreDefaultWebSite</@ofbizUrl>">
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
