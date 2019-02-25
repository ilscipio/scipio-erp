<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#--Dispatcher Name: ${dispatcherName!uiLabelMap.CommonNA} -->

<#assign url='ServiceList'>
<#assign popupUrl='serviceEcaDetail'>

<#-- Selected Service is available -->
<#if selectedServiceMap??>

  <#if showWsdl?? && showWsdl == true>
    <@section title=rawLabel('WebtoolsServiceWSDL')+" - "+rawLabel('WebtoolsService')+" "+raw(selectedServiceMap.serviceName)>
        <@code type="html">${selectedServiceMap.wsdl}</@code>
        <br />
        <a href="<@pageUrl>${url}?sel_service_name=${selectedServiceMap.serviceName}</@pageUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonBack}</a>
    </@section>

  <#else>
    <@section title=rawLabel('WebtoolsService')+" "+raw(selectedServiceMap.serviceName)>
        <@menu type="button">
          <@menuitem type="link" href=makePageUrl(url) text=uiLabelMap.CommonListAll class="+${styles.action_run_sys!} ${styles.action_find!}" />
          <@menuitem type="link" href=makePageUrl("scheduleJob?SERVICE_NAME=${raw(selectedServiceMap.serviceName)}") text=uiLabelMap.WebtoolsSchedule class="+${styles.action_nav!} ${styles.action_configure!}" />
          <@menuitem type="link" href=makePageUrl("setSyncServiceParameters?SERVICE_NAME=${raw(selectedServiceMap.serviceName)}&POOL_NAME=pool&_RUN_SYNC_=Y") text=uiLabelMap.PageTitleRunService class="+${styles.action_nav!} ${styles.action_begin!}" />
        </@menu>

        <#if selectedServiceMap.deprecatedUseInstead?has_content>
          <@section title=rawLabel('WebtoolsWarningLogLevel')?upper_case+": "+rawLabel('WebtoolsDeprecated')>
            <@field type="display" label=uiLabelMap.WebtoolsDeprecatedUseInstead><#if raw(selectedServiceMap.deprecatedUseInstead)?lower_case == "none">${uiLabelMap.CommonNone}<#else><#rt/>
                <a href="<@pageUrl>ServiceList?sel_service_name=${selectedServiceMap.deprecatedUseInstead}</@pageUrl>">${selectedServiceMap.deprecatedUseInstead}</a></#if></@field><#lt/>
            <@field type="display" label=uiLabelMap.CommonSince>${selectedServiceMap.deprecatedSince!}</@field>
            <@field type="display" label=uiLabelMap.CommonReason>${selectedServiceMap.deprecatedReason!}</@field>
          </@section>
        </#if>

    <#-- Show a little form for exportServiceEoModelBundle -->
    <@row>
        <@cell columns=6>
          <form name="exportServiceEoModelBundle" method="post" action="<@pageUrl>exportServiceEoModelBundle</@pageUrl>" class="basic-form">
            <input type="hidden" name="sel_service_name" value="${selectedServiceMap.serviceName}"/>
            <input type="hidden" name="serviceName" value="${selectedServiceMap.serviceName}"/>
            Save eomodeld to Local Path: <input type="text" name="eomodeldFullPath" value="${parameters.eomodeldFullPath!}" size="60"/>
            <input type="submit" name="submitButton" value="Export" class="${styles.link_run_sys!} ${styles.action_export!}"/>
          </form>
        </@cell>
      </@row>
    <@row>
        <@cell>
        <@table type="fields">
        <@thead>
          <@tr>
                <@th>${uiLabelMap.WebtoolsParameterName}</@th>
                <@th>${uiLabelMap.CommonDescription}</@th>
                <@th>${uiLabelMap.WebtoolsParameterName}</@th>
                <@th>${uiLabelMap.CommonDescription}</@th>
            </@tr>
        </@thead>
          <@tr>
            <@td>${uiLabelMap.WebtoolsServiceName}</@td>
            <@td>${selectedServiceMap.serviceName}</@td>
            <@td>${uiLabelMap.WebtoolsEngineName}</@td>
            <@td><a href="<@pageUrl>${url}?constraint=engine_name@${selectedServiceMap.engineName}</@pageUrl>">${selectedServiceMap.engineName}</a></@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.CommonDescription}</@td>
            <@td>${selectedServiceMap.description}</@td>
            <@td>${uiLabelMap.WebtoolsInvoke}</@td>
            <@td>${selectedServiceMap.invoke}</@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.WebtoolsExportable}</@td>
            <@td>${selectedServiceMap.export}<#if selectedServiceMap.exportBool == "true">&nbsp;(<a href="<@pageUrl>${url}?sel_service_name=${selectedServiceMap.serviceName}&amp;show_wsdl=true</@pageUrl>" class="+${styles.link_nav_inline!}" >WSDL</a>)</#if></@td>
            <@td>${uiLabelMap.WebtoolsLocation}</@td>
            <@td><a href="<@pageUrl>${url}?constraint=location@${selectedServiceMap.location}</@pageUrl>">${selectedServiceMap.location}</a></@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.WebtoolsDefinitionLocation}</@td>
            <@td><a href="<@pageUrl>${url}?constraint=definitionLocation@${selectedServiceMap.definitionLocation}</@pageUrl>">${selectedServiceMap.definitionLocation}</a></@td>
            <@td>${uiLabelMap.WebtoolsDefaultEntityName}</@td>
            <@td><a href="<@pageUrl>${url}?constraint=default_entity_name@${selectedServiceMap.defaultEntityName}</@pageUrl>">${selectedServiceMap.defaultEntityName}</a></@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.WebtoolsArtifactInfo}</@td>
            <@td><a href="<@pageUrl>ArtifactInfo?name=${selectedServiceMap.serviceName}&amp;type=service</@pageUrl>">${uiLabelMap.WebtoolsArtifactInfo}</a></@td>
            <@td>${uiLabelMap.WebtoolsRequireNewTransaction}</@td>
            <@td>${selectedServiceMap.requireNewTransaction}</@td>
          </@tr>
          <@tr>
            <@td colspan="2">&nbsp;</@td>
            <@td>${uiLabelMap.WebtoolsUseTransaction}</@td>
            <@td>${selectedServiceMap.useTrans}</@td>
          </@tr>
          <@tr>
            <@td colspan="2">&nbsp;</@td>
            <@td>${uiLabelMap.WebtoolsMaxRetries}</@td>
            <@td>${selectedServiceMap.maxRetry}</@td>
          </@tr>
        </@table>
    </@cell>
</@row>

<@row>
    <@cell>
        <@heading>${uiLabelMap.SecurityGroups}</@heading>
      <#if selectedServiceMap.permissionGroups != 'NA'>
        <@table type="data-list">
         <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.WebtoolsNameOrRole}</@th>
            <@th>${uiLabelMap.WebtoolsPermissionType}</@th>
            <@th>${uiLabelMap.WebtoolsAction}</@th>
          </@tr>
         </@thead> 
          <#list selectedServiceMap.permissionGroups as permGrp>
            <@tr>
              <@td>${permGrp.nameOrRole!uiLabelMap.CommonNA}</@td>
              <@td>${permGrp.permType!uiLabelMap.CommonNA}</@td>
              <@td>${permGrp.action!uiLabelMap.CommonNA}</@td>
            </@tr>
          </#list>
        </@table>
      <#else>
           ${selectedServiceMap.permissionGroups}
      </#if>
    </@cell>
</@row>

<@row>
   <@cell>
        <@heading>${uiLabelMap.WebtoolsImplementedServices}</@heading>

        <#if selectedServiceMap.implServices == 'NA'>
          ${selectedServiceMap.implServices}
        <#elseif selectedServiceMap.implServices?has_content>
          <#list selectedServiceMap.implServices as implSrv>
            <a href="<@pageUrl>${url}?sel_service_name=${implSrv.getService()}</@pageUrl>">${implSrv.getService()}</a><br />
          </#list>
        </#if>
    </@cell>
</@row>

    <#-- If service has ECA's -->
    <#if ecaMapList?? && ecaMapList?has_content>
      <#-- add the javascript for modalpopup's -->
      <@script>
          function detailsPopup(viewName){
              var lookupWinSettings = 'top=50,left=50,width=600,height=300,scrollbars=auto,status=no,resizable=no,dependent=yes,alwaysRaised=yes';
              var params = '';
              var lookupWin = window.open(viewName, params, lookupWinSettings);
              if(lookupWin.opener == null) lookupWin.opener = self;
              lookupWin.focus();
          }
      </@script>
<@row>
   <@cell>
          <@heading>${uiLabelMap.WebtoolsServiceECA}</@heading>
        <@table type="data-complex">
          <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.WebtoolsEventName}</@th>
            <#if ecaMapList.runOnError??>
              <@th>${uiLabelMap.WebtoolsRunOnError}</@th>
            </#if>
            <#if ecaMapList.runOnFailure??>
              <@th>${uiLabelMap.WebtoolsRunOnFailure}</@th>
            </#if>
            <@th>${uiLabelMap.WebtoolsActions}</@th>
            <@th>${uiLabelMap.WebtoolsConditions}</@th>
            <@th>${uiLabelMap.WebtoolsSet}</@th>
          </@tr>
          </@thead>
          <#list ecaMapList as ecaMap>
            <@tr>
              <@td>${ecaMap.eventName!}</@td>
              <#if ecaMap.runOnError??>
                <@td>${ecaMap.runOnError}</@td>
              </#if>
              <#if ecaMap.runOnFailure??>
                <@td>${ecaMap.runOnFailure}</@td>
              </#if>
              <#if ecaMap.actions?has_content>
                <@td>
                  <#list ecaMap.actions as action>
                    <@table type="fields">
                      <@tr>
                        <@td colspan="2"><a href="<@pageUrl>${url}?sel_service_name=${action.serviceName}</@pageUrl>">${action.serviceName!uiLabelMap.CommonNA}</a></@td>
                      </@tr>
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsSecasIgnoreError}</b> ${action.ignoreError!uiLabelMap.CommonNA}</@td>
                        <@td>${uiLabelMap.WebtoolsSecasIgnoreFailure}</b> ${action.ignoreFailure!uiLabelMap.CommonNA}</@td>
                      </@tr>
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsSecasPersist}</b> ${action.persist!uiLabelMap.CommonNA}</@td>
                        <@td>${uiLabelMap.WebtoolsSecasResultMapName}</b> ${action.resultMapName!uiLabelMap.CommonNA}</@td>
                      </@tr>
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsSecasResultToContext}</b> ${action.resultToContext!uiLabelMap.CommonNA}</@td>
                        <@td>${uiLabelMap.WebtoolsSecasResultToResult}</b> ${action.resultToResult!uiLabelMap.CommonNA}</@td>
                      </@tr>
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsSecasServiceMode}</b> ${action.serviceMode!uiLabelMap.CommonNA}</@td>
                        <@td colspan="2">&nbsp;</@td>
                      </@tr>
                    </@table>
                  </#list>
                </@td>
              </#if>
              <#if ecaMap.conditions?has_content>
                <@td>
                  <#list ecaMap.conditions as condition>
                    <@table type="fields">
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsCompareType}</b> ${condition.compareType!uiLabelMap.CommonNA}</@td>
                        <@td>
                          <b>${uiLabelMap.WebtoolsConditionService}</b>
                          <#if condition.conditionService?has_content>
                            <a href="<@pageUrl>${url}?sel_service_name=${condition.conditionService}</@pageUrl>">${condition.conditionService!uiLabelMap.CommonNA}</a>
                          <#else>
                            ${condition.conditionService!uiLabelMap.CommonNA}
                          </#if>
                        </@td>
                        <@td>${uiLabelMap.WebtoolsFormat}</b> ${condition.format!uiLabelMap.CommonNA}</@td>
                      </@tr>
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsIsService}</b> ${condition.isService!uiLabelMap.CommonNA}</@td>
                        <@td>${uiLabelMap.WebtoolsIsConstant}</b> ${condition.isConstant!uiLabelMap.CommonNA}</@td>
                        <@td>${uiLabelMap.WebtoolsOperator}</b> ${condition.operator!uiLabelMap.CommonNA}</@td>
                      </@tr>
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsLHSMapName}</b> ${condition.lhsMapName!uiLabelMap.CommonNA}</@td>
                        <@td>${uiLabelMap.WebtoolsLHSValueName}</b> ${condition.lhsValueName!uiLabelMap.CommonNA}</@td>
                        <@td>&nbsp;</@td>
                      </@tr>
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsRHSMapName}</b> ${condition.rhsMapName!uiLabelMap.CommonNA}</@td>
                        <@td>${uiLabelMap.WebtoolsRHSValueName}</b> ${condition.rhsValueName!uiLabelMap.CommonNA}</@td>
                        <@td>&nbsp;</@td>
                      </@tr>
                    </@table><br />
                  </#list>
                </@td>
              </#if>
              <#if ecaMap.sets?has_content>
                <@td>
                  <#list ecaMap.sets as set>
                    <@table type="fields">
                      <@tr>
                        <@td>${uiLabelMap.WebtoolsFieldName}</b> ${set.fieldName!uiLabelMap.CommonNA}</@td>
                        <@td>&nbsp;</@td>
                      </@tr>
                      <@tr>
                        <#if set.envName?has_content>
                          <@td>${uiLabelMap.WebtoolsEnvName}</b> ${set.envName}</@td>
                          <@td>&nbsp;</@td>
                        </#if>
                      </@tr>
                      <@tr>
                        <#if set.value?has_content>
                          <@td>${uiLabelMap.CommonValue}</b> ${set.value}</@td>
                          <@td>&nbsp;</@td>
                        </#if>
                      </@tr>
                      <@tr>
                        <#if set.format?has_content>
                          <@td>${uiLabelMap.WebtoolsFormat}</b> ${set.format}</@td>
                          <@td>&nbsp;</@td>
                        </#if>
                      </@tr>
                    </@table><br />
                  </#list>
                </@td>
              </#if>
            </@tr>
            <@tr type="util"><@td colspan='5'><hr/></@td></@tr>
          </#list>
        </@table>
    </@cell>
</@row>
    </#if>
    <#-- End if service has ECA's -->

    <#list selectedServiceMap.allParamsList! as paramList>
      <style type="text/css">
        .param-table tr td {
          width: 12.5%;
          vertical-align: top;
        }
      </style>
    <@row>
       <@cell>
          <@heading>${paramList.title}</@heading>

        <#if paramList.paramList?? && paramList.paramList?has_content>
          <@table type="data-list" class="+param-table">
            <@thead>
              <@tr class="header-row">
                <@th>${uiLabelMap.WebtoolsParameterName}</@th>
                <@th>${uiLabelMap.CommonDescription}</@th>
                <@th>${uiLabelMap.WebtoolsOptional}</@th>
                <@th>${uiLabelMap.CommonType}</@th>
                <#-- <@th>Default Value</@th> -->
                <@th>${uiLabelMap.WebtoolsMode}</@th>
                <@th>${uiLabelMap.WebtoolsIsSetInternally}</@th>
                <@th>${uiLabelMap.WebtoolsEntityName}</@th>
                <@th>${uiLabelMap.WebtoolsFieldName}</@th>
              </@tr>
            </@thead>
              <#list paramList.paramList as modelParam>
                <@tr>
                  <@td>${modelParam.name!}</@td>
                  <@td>${modelParam.description!}</@td>
                  <@td>${modelParam.optional!}</@td>
                  <@td>${modelParam.type!}</@td>
                  <#-- <@td>[${modelParam.defaultValue!}]</@td> -->
                  <@td>${modelParam.mode!}</@td>
                  <@td>${modelParam.internal!}</@td>
                  <@td>
                    <#if modelParam.entityName??>
                      <a href="<@pageUrl>${url}?constraint=default_entity_name@${modelParam.entityName}</@pageUrl>">${modelParam.entityName!}</a>
                    </#if>
                  </@td>
                  <@td>${modelParam.fieldName!}</@td>
                </@tr>
              </#list>
          </@table>
        <#else>
            <@commonMsg type="result-norecord">${uiLabelMap.WebtoolsNoParametersDefined}</@commonMsg>
        </#if>
        </@cell>
    </@row>
    </#list>

    </@section>
  </#if>
  
<#-- No Service selected , we list all-->
<#elseif servicesList?? && servicesList?has_content>

  <#-- Show alphabetical index -->
  <#if serviceNamesAlphaList?? && serviceNamesAlphaList?has_content>
      <@nav type="magellan">
        <@mli arrival="Service_all"><a href="<@pageUrl>${url}</@pageUrl>">${uiLabelMap.CommonAll}</a></@mli>
        <#assign isfirst=true>
        <#list serviceNamesAlphaList as alpha>
          <#-- Use this to jump to location
          <a href="#Service_${alpha}">${alpha}</a>
           -->
          <@mli arrival="Service_${alpha}"><a href="<@pageUrl>${url}?constraint=alpha@${alpha}</@pageUrl>">${alpha}</a></@mli>
          <#assign isfirst=false>
        </#list>
      </@nav>
  </#if>

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
      <@menuitem type="link" href=makePageUrl("${url}") text=uiLabelMap.CommonListAll class="+${styles.action_run_sys!} ${styles.action_find!}" />
    </@menu>
  </#macro>
  <@section menuContent=menuContent>
      <#--
      ${uiLabelMap.WebtoolsServicesListFor} ${dispatcherName!uiLabelMap.CommonNA} (${servicesFoundCount} ${uiLabelMap.CommonFound})-->
      <@table type="data-list" autoAltRows=true scrollable=true>
        <@thead>
        <@tr class="header-row">
          <@th id="Service_all">${uiLabelMap.WebtoolsServiceName}</@th>
          <@th>${uiLabelMap.WebtoolsEngineName}</@th>
          <@th>${uiLabelMap.WebtoolsDefaultEntityName}</@th>
          <@th>${uiLabelMap.WebtoolsInvoke}</@th>
          <@th>${uiLabelMap.WebtoolsLocation}</@th>
          <@th>${uiLabelMap.WebtoolsDefinitionLocation}</@th>
        </@tr>
        </@thead>
        <#assign lastChar = "">
        <#list servicesList as service>
          <#-- SCIPIO: TODO: deprecation warning
            <td><#if service.deprecated?has_content><strike></#if><a href='<@pageUrl>${url}?sel_service_name=${service.serviceName}</@pageUrl>'>${service.serviceName}</a><#if service.deprecated?has_content></strike> @deprecated</#if></td>
          -->
          <#assign firstChar = service.serviceName?substring(0, 1)>
          <#if firstChar != lastChar>
            <#assign anchorId = "Service_${firstChar}">
            <#assign anchorAttribs = {"data-magellan-destination":"Service_${firstChar}"}>
          <#else>
            <#assign anchorId = "">
            <#assign anchorAttribs = {}>
          </#if>
          <@tr>
            <@td id=anchorId attribs=anchorAttribs><#if service.deprecated?has_content><strike></#if><a href="<@pageUrl>${url}?sel_service_name=${service.serviceName}</@pageUrl>">${service.serviceName}</a><#if service.deprecated?has_content></strike> (${uiLabelMap.WebtoolsDeprecated})</#if></@td>
            <@td><a href="<@pageUrl>${url}?constraint=engine_name@${service.engineName!uiLabelMap.CommonNA}</@pageUrl>">${service.engineName}</a></@td>
            <@td><a href="<@pageUrl>${url}?constraint=default_entity_name@${service.defaultEntityName!uiLabelMap.CommonNA}</@pageUrl>">${service.defaultEntityName}</a></@td>
            <@td>${service.invoke}</@td>
            <@td><a href="<@pageUrl>${url}?constraint=location@${service.location!uiLabelMap.CommonNA}</@pageUrl>">${service.location}</a></@td>
            <@td><a href="<@pageUrl>${url}?constraint=definitionLocation@${service.definitionLocation}</@pageUrl>">${service.definitionLocation}</a></@td>
          </@tr>
          <#assign lastChar = firstChar>
        </#list>
      </@table>
  </@section>
<#else>
  <@commonMsg type="result-norecord">${uiLabelMap.WebtoolsNoServicesFound}.</@commonMsg>
</#if>
