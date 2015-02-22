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
<@renderScreenletBegin id="" title=uiLabelMap.WebtoolsMainPage />
  <div class="">
    <#if !userLogin?has_content>
      <div>${uiLabelMap.WebtoolsForSomethingInteresting}.</div>
      <br />
      <div>${uiLabelMap.WebtoolsNoteAntRunInstall}</div>
      <br />
      <div><a href="<@ofbizUrl>checkLogin</@ofbizUrl>" class="button">${uiLabelMap.CommonLogin}</a></div>
    </#if>
    <@grid>
    <#if userLogin?has_content>
      <li><ul class="pricing-table">
        <li class="title">${uiLabelMap.WebtoolsCacheDebugTools}</li>
        <#--<li class="description">lorem ipsum</li>-->
        <li class="bullet-item"><a href="<@ofbizUrl>FindUtilCache</@ofbizUrl>">${uiLabelMap.WebtoolsCacheMaintenance}</a></li>
        <li class="bullet-item"><a href="<@ofbizUrl>LogConfiguration</@ofbizUrl>">${uiLabelMap.WebtoolsAdjustDebuggingLevels}</a></li>
        <li class="bullet-item"><a href="<@ofbizUrl>LogView</@ofbizUrl>">${uiLabelMap.WebtoolsViewLog}</a></li>
        <li class="bullet-item"><a href="<@ofbizUrl>ViewComponents</@ofbizUrl>">${uiLabelMap.WebtoolsViewComponents}</a></li>
      </ul></li>
        <#if security.hasPermission("ARTIFACT_INFO_VIEW", session)>
          <li>
          <ul class="pricing-table">
              <li class="title">${uiLabelMap.WebtoolsGeneralArtifactInfoTools}</li>  
              <li class="bullet-item"><a href="<@ofbizUrl>ViewComponents</@ofbizUrl>" target="_blank">${uiLabelMap.WebtoolsArtifactInfo}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>entityref</@ofbizUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceInteractiveVersion}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>ServiceList</@ofbizUrl>">${uiLabelMap.WebtoolsServiceReference}</a></li>
          </ul></li>
        </#if>
    
        <#if security.hasPermission("LABEL_MANAGER_VIEW", session)>
          <li>
          <ul class="pricing-table">
              <li class="title">${uiLabelMap.WebtoolsLabelManager}</li>         
              <li class="bullet-item"><a href="<@ofbizUrl>SearchLabels</@ofbizUrl>">${uiLabelMap.WebtoolsLabelManager}</a></li>
          </ul></li>
        </#if>
        <#if security.hasPermission("ENTITY_MAINT", session)>
        <li>
        <ul class="pricing-table">
          <li class="title">${uiLabelMap.WebtoolsEntityEngineTools}</li>
          <li class="bullet-item"><a href="<@ofbizUrl>entitymaint</@ofbizUrl>">${uiLabelMap.WebtoolsEntityDataMaintenance}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>entityref</@ofbizUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceInteractiveVersion}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>entityref?forstatic=true</@ofbizUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReference} - ${uiLabelMap.WebtoolsEntityReferenceStaticVersion}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>entityrefReport</@ofbizUrl>" target="_blank">${uiLabelMap.WebtoolsEntityReferencePdf}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>EntitySQLProcessor</@ofbizUrl>">${uiLabelMap.PageTitleEntitySQLProcessor}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>EntitySyncStatus</@ofbizUrl>">${uiLabelMap.WebtoolsEntitySyncStatus}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>view/ModelInduceFromDb</@ofbizUrl>" target="_blank">${uiLabelMap.WebtoolsInduceModelXMLFromDatabase}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>EntityEoModelBundle</@ofbizUrl>">${uiLabelMap.WebtoolsExportEntityEoModelBundle}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>view/checkdb</@ofbizUrl>">${uiLabelMap.WebtoolsCheckUpdateDatabase}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>ConnectionPoolStatus</@ofbizUrl>">${uiLabelMap.ConnectionPoolStatus}</a></li>
          <#-- want to leave these out because they are only working so-so, and cause people more problems that they solve, IMHO
            <li class="bullet-item"><a href="<@ofbizUrl>view/EditEntity</@ofbizUrl>"  target="_blank">Edit Entity Definitions</a></li>
            <li class="bullet-item"><a href="<@ofbizUrl>ModelWriter</@ofbizUrl>" target="_blank">Generate Entity Model XML (all in one)</a></li>
            <li class="bullet-item"><a href="<@ofbizUrl>ModelWriter?savetofile=true</@ofbizUrl>" target="_blank">Save Entity Model XML to Files</a></li>
          -->
          <#-- not working right now anyway
            <li class="bullet-item"><a href="<@ofbizUrl>ModelGroupWriter</@ofbizUrl>" target="_blank">Generate Entity Group XML</a></li>
            <li class="bullet-item"><a href="<@ofbizUrl>ModelGroupWriter?savetofile=true</@ofbizUrl>" target="_blank">Save Entity Group XML to File</a></li>
          -->
          <#--
            <li class="bullet-item"><a href="<@ofbizUrl>view/tablesMySql</@ofbizUrl>">MySQL Table Creation SQL</a></li>
            <li class="bullet-item"><a href="<@ofbizUrl>view/dataMySql</@ofbizUrl>">MySQL Auto Data SQL</a></li>
          -->
          </ul></li>
          <li>
          <ul class="pricing-table">
              <li class="title">${uiLabelMap.WebtoolsEntityXMLTools}</li>
              <li class="bullet-item"><a href="<@ofbizUrl>xmldsdump</@ofbizUrl>">${uiLabelMap.PageTitleEntityExport}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>EntityExportAll</@ofbizUrl>">${uiLabelMap.PageTitleEntityExportAll}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>EntityImport</@ofbizUrl>">${uiLabelMap.PageTitleEntityImport}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>EntityImportDir</@ofbizUrl>">${uiLabelMap.PageTitleEntityImportDir}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>EntityImportReaders</@ofbizUrl>">${uiLabelMap.PageTitleEntityImportReaders}</a></li>
          </ul></li>
        </#if>
        <#if security.hasPermission("SERVICE_MAINT", session)>
        <li>
            <ul class="pricing-table">
              <li class="title">${uiLabelMap.WebtoolsServiceEngineTools}</li>
              <li class="bullet-item"><a href="<@ofbizUrl>ServiceList</@ofbizUrl>">${uiLabelMap.WebtoolsServiceReference}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>scheduleJob</@ofbizUrl>">${uiLabelMap.PageTitleScheduleJob}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>runService</@ofbizUrl>">${uiLabelMap.PageTitleRunService}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>FindJob</@ofbizUrl>">${uiLabelMap.PageTitleJobList}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>threadList</@ofbizUrl>">${uiLabelMap.PageTitleThreadList}</a></li>
              <li class="bullet-item"><a href="<@ofbizUrl>ServiceLog</@ofbizUrl>">${uiLabelMap.WebtoolsServiceLog}</a></li>
            </ul></li>
        </#if>
        
        <#if security.hasPermission("DATAFILE_MAINT", session)>
        <li>
          <ul class="pricing-table">
            <li class="title">${uiLabelMap.WebtoolsDataFileTools}</li>
            <li class="bullet-item"><a href="<@ofbizUrl>viewdatafile</@ofbizUrl>">${uiLabelMap.WebtoolsWorkWithDataFiles}</a></li>
          </ul></li>
        </#if>
        <li>
        <ul class="pricing-table">
        <li class="title">${uiLabelMap.WebtoolsMiscSetupTools}</li>
        <#if security.hasPermission("PORTALPAGE_ADMIN", session)>
          <li class="bullet-item"><a href="<@ofbizUrl>FindPortalPage</@ofbizUrl>">${uiLabelMap.WebtoolsAdminPortalPage}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>FindGeo</@ofbizUrl>">${uiLabelMap.WebtoolsGeoManagement}</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>WebtoolsLayoutDemo</@ofbizUrl>">${uiLabelMap.WebtoolsLayoutDemo}</a></li>
        </#if>
        <#if security.hasPermission("ENUM_STATUS_MAINT", session)>
          <#--
          <li class="bullet-item"><a href="<@ofbizUrl>EditEnumerationTypes</@ofbizUrl>">Edit Enumerations</a></li>
          <li class="bullet-item"><a href="<@ofbizUrl>EditStatusTypes</@ofbizUrl>">Edit Status Options</a></li>
          -->
        </#if>
        </ul></li>
        <li>
        <ul class="pricing-table">
        <li class="title">${uiLabelMap.WebtoolsPerformanceTests}</li>
        <li class="bullet-item"><a href="<@ofbizUrl>EntityPerformanceTest</@ofbizUrl>">${uiLabelMap.WebtoolsEntityEngine}</a></li>
        </ul></li>
        <#if security.hasPermission("SERVER_STATS_VIEW", session)>
          <li>
           <ul class="pricing-table">
          <li class="title">${uiLabelMap.WebtoolsServerHitStatisticsTools}</li>
          <li class="bullet-item"><a href="<@ofbizUrl>StatsSinceStart</@ofbizUrl>">${uiLabelMap.WebtoolsStatsSinceServerStart}</a></li>
          </ul></li>
        </#if>
        <li>
        <ul class="pricing-table">
            <li class="title">${uiLabelMap.WebtoolsCertsX509}</li>
            <li class="bullet-item"><a href="<@ofbizUrl>myCertificates</@ofbizUrl>">${uiLabelMap.WebtoolsMyCertificates}</a></li>
        </ul></li>
      </ul></li>
    </#if>
    </@grid>
  </div>
</div>
