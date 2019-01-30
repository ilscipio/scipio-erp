<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

    <#if !userLogin?has_content>
    <#--
      <div>${uiLabelMap.WebtoolsForSomethingInteresting}.</div>
      <br />
      <div>${uiLabelMap.WebtoolsNoteAntRunInstall}</div>
      <br />
      <div><a href="<@pageUrl>checkLogin</@pageUrl>" class="button">${uiLabelMap.CommonLogin}</a></div>
    -->     
    <#include "component://common/webcommon/login.ftl"/>
    </#if>

    <#if userLogin?has_content>
    <@alert type="info">${uiLabelMap.WebtoolsNoteAntRunInstall}</@alert>
    <@section>
        <@grid columns=4>
            <li>
              <@pul title=uiLabelMap.WebtoolsCacheDebugTools>
                <@pli><a href="<@pageUrl>FindUtilCache</@pageUrl>">${uiLabelMap.WebtoolsCacheMaintenance}</a></@pli>
                <@pli><a href="<@pageUrl>LogView</@pageUrl>">${uiLabelMap.WebtoolsViewLog}</a></@pli>
                <@pli><a href="<@pageUrl>ViewComponents</@pageUrl>">${uiLabelMap.WebtoolsViewComponents}</a></@pli>
              </@pul>  
            </li>
            <#if security.hasPermission("ARTIFACT_INFO_VIEW", request)>
            <li>
              <@pul title=uiLabelMap.WebtoolsGeneralArtifactInfoTools>
                <@pli><a href="<@pageUrl>ViewComponents</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsArtifactInfo}</a></@pli>
                <@pli><a href="<@pageUrl>entityref</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceInteractiveVersion}</a></@pli>
                <@pli><a href="<@pageUrl>ServiceList</@pageUrl>">${uiLabelMap.WebtoolsServiceReference}</a></@pli>
              </@pul>
            </li>
            </#if>
            <#if security.hasPermission("LABEL_MANAGER_VIEW", request)>
            <li>
              <@pul title=uiLabelMap.WebtoolsLabelManager>
                <@pli><a href="<@pageUrl>SearchLabels</@pageUrl>">${uiLabelMap.WebtoolsLabelManager}</a></@pli>
              </@pul>
            </li>
            </#if>
            <#if security.hasPermission("ENTITY_MAINT", request)>
            <li>
                <@pul title=uiLabelMap.WebtoolsEntityEngineTools>
              <@pli><a href="<@pageUrl>entitymaint</@pageUrl>">${uiLabelMap.WebtoolsEntityDataMaintenance}</a></@pli>
              <@pli><a href="<@pageUrl>entityref</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceInteractiveVersion}</a></@pli>
              <@pli><a href="<@pageUrl>entityref?forstatic=true</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceStaticVersion}</a></@pli>
              <@pli><a href="<@pageUrl>entityrefReport</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReferencePdf}</a></@pli>
              <@pli><a href="<@pageUrl>entitymaint</@pageUrl>">${uiLabelMap.WebtoolsEntityDataMaintenance}</a></@pli>
              <@pli><a href="<@pageUrl>entityref</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceInteractiveVersion}</a></@pli>
              <@pli><a href="<@pageUrl>entityref?forstatic=true</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceStaticVersion}</a></@pli>
              <@pli><a href="<@pageUrl>entityrefReport</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReferencePdf}</a></@pli>
              <@pli><a href="<@pageUrl>EntitySQLProcessor</@pageUrl>">${uiLabelMap.PageTitleEntitySQLProcessor}</a></@pli>
              <@pli><a href="<@pageUrl>EntitySyncStatus</@pageUrl>">${uiLabelMap.WebtoolsEntitySyncStatus}</a></@pli>
              <@pli><a href="<@pageUrl>view/ModelInduceFromDb</@pageUrl>" target="_blank">${uiLabelMap.WebtoolsInduceModelXMLFromDatabase}</a></@pli>
              <@pli><a href="<@pageUrl>EntityEoModelBundle</@pageUrl>">${uiLabelMap.WebtoolsExportEntityEoModelBundle}</a></@pli>
              <@pli><a href="<@pageUrl>view/checkdb</@pageUrl>">${uiLabelMap.WebtoolsCheckUpdateDatabase}</a></@pli>
              <@pli><a href="<@pageUrl>ConnectionPoolStatus</@pageUrl>">${uiLabelMap.ConnectionPoolStatus}</a></@pli>
              <#-- want to leave these out because they are only working so-so, and cause people more problems that they solve, IMHO
              <@pli><a href="<@pageUrl>view/EditEntity</@pageUrl>"  target="_blank">Edit Entity Definitions</a></@pli>
              <@pli><a href="<@pageUrl>ModelWriter</@pageUrl>" target="_blank">Generate Entity Model XML (all in one)</a></@pli>
              <@pli><a href="<@pageUrl>ModelWriter?savetofile=true</@pageUrl>" target="_blank">Save Entity Model XML to Files</a></@pli>
              -->
              <#-- not working right now anyway
              <@pli><a href="<@pageUrl>ModelGroupWriter</@pageUrl>" target="_blank">Generate Entity Group XML</a></@pli>
              <@pli><a href="<@pageUrl>ModelGroupWriter?savetofile=true</@pageUrl>" target="_blank">Save Entity Group XML to File</a></@pli>
              -->
              <#--
              <@pli><a href="<@pageUrl>view/tablesMySql</@pageUrl>">MySQL Table Creation SQL</a></@pli>
              <@pli><a href="<@pageUrl>view/dataMySql</@pageUrl>">MySQL Auto Data SQL</a></@pli>
              -->
            </@pul>
            </li>
            </#if>
            <#if security.hasPermission("SERVICE_MAINT", request)>
            <li>
                <@pul title=uiLabelMap.WebtoolsServiceEngineTools>
                  <@pli><a href="<@pageUrl>ServiceList</@pageUrl>">${uiLabelMap.WebtoolsServiceReference}</a></@pli>
                  <@pli><a href="<@pageUrl>scheduleJob</@pageUrl>">${uiLabelMap.PageTitleScheduleJob}</a></@pli>
                  <@pli><a href="<@pageUrl>runService</@pageUrl>">${uiLabelMap.PageTitleRunService}</a></@pli>
                  <@pli><a href="<@pageUrl>FindJob</@pageUrl>">${uiLabelMap.PageTitleJobList}</a></@pli>
                  <@pli><a href="<@pageUrl>threadList</@pageUrl>">${uiLabelMap.PageTitleThreadList}</a></@pli>
                  <#-- SCIPIO: 2018-08-28: TODO: REVIEW: JobManagerLock
                  <@pli><a href="<@pageUrl>FindJobManagerLock</@pageUrl>">${uiLabelMap.PageTitleJobManagerLockList}</a></@pli>-->
                  <@pli><a href="<@pageUrl>ServiceLog</@pageUrl>">${uiLabelMap.WebtoolsServiceLog}</a></@pli>
                </@pul>
            </li>
            </#if>         
            <#if security.hasPermission("DATAFILE_MAINT", request)>
            <li>
              <@pul title=uiLabelMap.WebtoolsDataFileTools>
                <@pli><a href="<@pageUrl>viewdatafile</@pageUrl>">${uiLabelMap.WebtoolsWorkWithDataFiles}</a></@pli>
              </@pul>
            </li>
            </#if>
            <li>
            <@pul title=uiLabelMap.WebtoolsMiscSetupTools>
                <#if security.hasPermission("PORTALPAGE_ADMIN", request)>
                <@pli><a href="<@pageUrl>FindPortalPage</@pageUrl>">${uiLabelMap.WebtoolsAdminPortalPage}</a></@pli>
                <@pli><a href="<@pageUrl>FindGeo</@pageUrl>">${uiLabelMap.WebtoolsGeoManagement}</a></@pli>
                <@pli><a href="<@pageUrl>WebtoolsLayoutDemo</@pageUrl>">${uiLabelMap.WebtoolsLayoutDemo}</a></@pli>
                </#if>
                <#if security.hasPermission("ENUM_STATUS_MAINT", request)>
                <#--
                <@pli><a href="<@pageUrl>EditEnumerationTypes</@pageUrl>">Edit Enumerations</a></@pli>
                <@pli><a href="<@pageUrl>EditStatusTypes</@pageUrl>">Edit Status Options</a></@pli>
                -->
                </#if>
            </@pul>
            </li>
            <li>
            <@pul title=uiLabelMap.WebtoolsPerformanceTests>
                <@pli><a href="<@pageUrl>EntityPerformanceTest</@pageUrl>">${uiLabelMap.WebtoolsEntityEngine}</a></@pli>
            </@pul>
            </li>
            <#if security.hasPermission("SERVER_STATS_VIEW", request)>
            <li>
              <@pul title=uiLabelMap.WebtoolsServerHitStatisticsTools>
                    <@pli><a href="<@pageUrl>StatsSinceStart</@pageUrl>">${uiLabelMap.WebtoolsStatsSinceServerStart}</a></@pli>
              </@pul>
            </li>
            </#if>
            <li>
            <@pul title=uiLabelMap.WebtoolsCertsX509>
                <@pli><a href="<@pageUrl>myCertificates</@pageUrl>">${uiLabelMap.WebtoolsMyCertificates}</a></@pli>
            </@pul>
            </li>
        </@grid>
    </@section>
    </#if>
