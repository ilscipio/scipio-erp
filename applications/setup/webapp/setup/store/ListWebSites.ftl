<#include "component://setup/webapp/setup/common/common.ftl">

    <@heading>${uiLabelMap.ContentWebSites}</@heading>

    <@table type="data-list">
      <@thead>
        <@tr>
          <@th>${uiLabelMap.CommonId}</@th>
          <@th>${uiLabelMap.CommonName}</@th>
          <@th></@th>
        </@tr>
      </@thead>
      <@tbody>
        <#list webSiteList as currWebSite>
          <@tr>
            <@td><@setupExtAppLink uri="/catalog/control/EditWebSite?webSiteId=${rawString(currWebSite.webSiteId)}" text=currWebSite.webSiteId/></@td>
            <@td>${currWebSite.siteName!}</@td>
            <@td>
              <form method="get" action="<@ofbizUrl uri=makeSetupStepUri("store") escapeAs="html"/>">
                <@setupStepFields name="store" exclude=["webSiteId"]/>
                <input type="hidden" name="webSiteId" value="${currWebSite.webSiteId}"/>
                <@field type="submit" text=uiLabelMap.CommonSelect class="+${styles.link_nav!} ${styles.action_update!}"/>
              </form>
            </@td>
          </@tr>
        </#list>
      </@tbody>
    </@table>
