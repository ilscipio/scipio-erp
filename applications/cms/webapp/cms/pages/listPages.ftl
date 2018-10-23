<#include "pagescommon.ftl">

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("editPage") class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsCreatePage/>
    </@menu>  
</#macro>
<@section title=uiLabelMap.CommonPages menuContent=menuContent>

  <@section>
    <@form action=makeOfbizUrl("pages") method="post">
        <@webSiteSelectField name="webSiteId" value=(webSiteId!) valueUnsafe=true required=false
            tooltip="${rawLabel('CmsOnlyHookedWebSitesListed')}" emptyLabel=uiLabelMap.CommonAny />
        <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonFind />
    </@form>
  </@section>

  <#if pages?has_content>
    <#assign paramStr = addParamsToStr("", {"webSiteId": rawString(webSiteId!"")}, "&", false)>
    <@paginate mode="content" url=makeOfbizUrl("pages") paramStr=paramStr viewSize=(viewSize!50) viewIndex=(viewIndex!0) listSize=(listSize!0)>
        <@table type="data-list" autoAltRows=true>
            <@thead>
                <@tr class="header-row">
                    <@th width="100px">${uiLabelMap.CommonPage}</@th>
                    <@th width="100px">${uiLabelMap.CmsWebSite}</@th>
                    <@th width="200px">${uiLabelMap.CommonPath}</@th>
                    <@th>${uiLabelMap.CommonName}</@th>
                    <@th>${uiLabelMap.CommonDescription}</@th>
                </@tr>
            </@thead>
            <#assign defaultLabelVal = uiLabelMap.CommonDefault?lower_case>
            <#-- this sort will not work as intended - query should handle it
            <#list pages?sort as item>-->
            <#list pages as item>
                <@tr>
                    <@td>${item.id}</@td>
                    <@td>
                        <#-- NOTE: the page can have mapping(s) to websites through primary process mappings (primary mechanism),
                            and it can have a legacy CmsPage.webSiteId field (not used functionally, for organization only i.e. this page), 
                            and also this page can have a webSiteId filter applied, 
                            and this is not counting possible future enhancements,
                            so this printout needs to be informative -->
                        <#if item.webSiteId?has_content>
                            ${item.webSiteId}<#if item.defaultWebSiteId?has_content><#if (item.webSiteId != item.defaultWebSiteId)> (${defaultLabelVal}: ${item.defaultWebSiteId})<#else> (${defaultLabelVal})</#if></#if>
                        <#elseif item.defaultWebSiteId?has_content>
                            ${item.defaultWebSiteId} (${defaultLabelVal})
                        </#if>
                    </@td>
                    <@td>
                      <#-- TODO?: REVIEW: in future, consider scrapping the path-based link and using only the pageId one instead...
                          it would simplify the UI code... -->
                      <#if item.path?has_content>
                        <#assign editPageLink = "editPage?path=${escapeVal(item.path, 'url')}&webSiteId=${item.webSiteId!item.defaultWebSiteId!}">
                        <a href="<@ofbizUrl uri=editPageLink escapeAs='html'/>">${escapeFullUrl(item.path, 'html')}</a>
                      <#else>
                        <#assign editPageLink = "editPage?pageId=${item.id}&webSiteId=${item.webSiteId!item.defaultWebSiteId!}">
                      </#if>
                    </@td>
                    <#-- FIXME: the name should always get the pageId link, but right now the javascript has issues -->
                    <@td><a href="<@ofbizUrl uri=editPageLink escapeAs='html'/>">${item.name!""}</a></@td>
                    <@td>${makeShortCmsDesc(item.description!)}</@td>
                </@tr>
            </#list>
        </@table>
    </@paginate>
  <#else>
    <@commonMsg type="result-norecord"/>
  </#if>
</@section>