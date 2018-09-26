<#include "component://setup/webapp/setup/common/common.ftl">

    <@heading>${uiLabelMap.ContentWebSites}</@heading>

    <@table type="data-list">
      <@thead>
        <@tr>
          <@th>${uiLabelMap.CommonId}</@th>
          <@th>${uiLabelMap.CommonName}</@th>
          <@th>${uiLabelMap.FormFieldTitle_isStoreDefault}</@th>
          <@th></@th>
        </@tr>
      </@thead>
      <@tbody>
        <#list webSiteList as currWebSite>
          <@tr>
            <@td><@setupExtAppLink uri="/catalog/control/EditWebSite?webSiteId=${rawString(currWebSite.webSiteId)}" text=currWebSite.webSiteId/></@td>
            <@td>${currWebSite.siteName!}</@td>
            <@td>${currWebSite.isStoreDefault!}</@td>
            <@td>
              <form method="get" action="<@ofbizUrl uri=makeSetupStepUri("store") escapeAs="html"/>">
                <@setupStepFields name="store" exclude=["webSiteId"]/>
                <input type="hidden" name="webSiteId" value="${currWebSite.webSiteId}"/>
                <@field type="submit" text=uiLabelMap.CommonSelect class="+${styles.link_nav!} ${styles.action_update!}"/>
              </form>
              
              <a href="javascript:document.setProductStoreDefaultWebSite_${currWebSite_index}.submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSetDefault}</a>
              <form name="setProductStoreDefaultWebSite_${currWebSite_index}" method="post" action="<@ofbizUrl>setProductStoreDefaultWebSite</@ofbizUrl>">
                <@setupStepFields name="store" exclude=["webSiteId"]/>
                <input type="hidden" name="webSiteId" value="${currWebSite.webSiteId}"/>
              </form>
            </@td>
          </@tr>
        </#list>
      </@tbody>
    </@table>
