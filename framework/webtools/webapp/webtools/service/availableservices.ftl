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

<#--Dispatcher Name: ${dispatcherName?default(uiLabelMap.CommonNA)} -->

<#assign url='ServiceList'>
<#assign popupUrl='serviceEcaDetail'>

<#-- Selected Service is available -->
<#if selectedServiceMap??>
  <#if showWsdl?? && showWsdl = true>
    <div class="screenlet">
      <div class="screenlet-title-bar">
        <h3>${uiLabelMap.WebtoolsServiceWSDL} - ${uiLabelMap.WebtoolsService} ${selectedServiceMap.serviceName}</h3>
      </div>
      <div class="screenlet-body" align="center">
        <form><textarea rows="20" cols="85" name="wsdloutput">${selectedServiceMap.wsdl}</textarea></form>
        <br />
        <a href='<@ofbizUrl>${url}?sel_service_name=${selectedServiceMap.serviceName}</@ofbizUrl>' class='smallSubmit'>${uiLabelMap.CommonBack}</a>
      </div>
    </div>
  <#else>
    <div class="screenlet">
      <div class="screenlet-title-bar">
        <ul>
          <li class="h3">${uiLabelMap.WebtoolsService} ${selectedServiceMap.serviceName}</li>
          <li><a href='<@ofbizUrl>${url}</@ofbizUrl>'>${uiLabelMap.CommonListAll}</a></li>
          <li><a href='<@ofbizUrl>/scheduleJob?SERVICE_NAME=${selectedServiceMap.serviceName}</@ofbizUrl>'>${uiLabelMap.WebtoolsSchedule}</a></li>
          <li><a href='<@ofbizUrl>/setSyncServiceParameters?SERVICE_NAME=${selectedServiceMap.serviceName}&amp;POOL_NAME=pool&amp;_RUN_SYNC_=Y</@ofbizUrl>'>${uiLabelMap.PageTitleRunService}</a></li>
        </ul>
        <br class="clear"/>
      </div>
      <div class="screenlet-body">
        <table class="basic-table" cellspacing='0'>
          <tr>
            <td class=>${uiLabelMap.WebtoolsServiceName}</td>
            <td>${selectedServiceMap.serviceName}</td>
            <td class=>${uiLabelMap.WebtoolsEngineName}</td>
            <td><a href='<@ofbizUrl>${url}?constraint=engine_name@${selectedServiceMap.engineName}</@ofbizUrl>'>${selectedServiceMap.engineName}</a></td>
          </tr>
          <tr>
            <td class=>${uiLabelMap.CommonDescription}</td>
            <td>${selectedServiceMap.description}</td>
            <td class=>${uiLabelMap.WebtoolsInvoke}</td>
            <td>${selectedServiceMap.invoke}</td>
          </tr>
          <tr>
            <td class=>${uiLabelMap.WebtoolsExportable}</td>
            <td>${selectedServiceMap.export}<#if selectedServiceMap.exportBool = "true">&nbsp;(<a href='<@ofbizUrl>${url}?sel_service_name=${selectedServiceMap.serviceName}&amp;show_wsdl=true</@ofbizUrl>'>${uiLabelMap.WebtoolsShowShowWSDL}</a>)</#if></td>
            <td class=>${uiLabelMap.WebtoolsLocation}</td>
            <td><a href='<@ofbizUrl>${url}?constraint=location@${selectedServiceMap.location}</@ofbizUrl>'>${selectedServiceMap.location}</a></td>
          </tr>
          <tr>
            <td class=>${uiLabelMap.WebtoolsDefinitionLocation}</td>
            <td><a href='<@ofbizUrl>${url}?constraint=definitionLocation@${selectedServiceMap.definitionLocation}</@ofbizUrl>'>${selectedServiceMap.definitionLocation}</a></td>
            <td class=>${uiLabelMap.WebtoolsDefaultEntityName}</td>
            <td><a href='<@ofbizUrl>${url}?constraint=default_entity_name@${selectedServiceMap.defaultEntityName}</@ofbizUrl>'>${selectedServiceMap.defaultEntityName}</a></td>
          </tr>
          <tr>
            <td class=>${uiLabelMap.WebtoolsArtifactInfo}</td>
            <td><a href='<@ofbizUrl>ArtifactInfo?name=${selectedServiceMap.serviceName}&amp;type=service</@ofbizUrl>'>${uiLabelMap.WebtoolsArtifactInfo}</a></td>
            <td class=>${uiLabelMap.WebtoolsRequireNewTransaction}</td>
            <td>${selectedServiceMap.requireNewTransaction}</td>
          </tr>
          <tr>
            <td colspan="2">&nbsp;</td>
            <td class=>${uiLabelMap.WebtoolsUseTransaction}</td>
            <td>${selectedServiceMap.useTrans}</td>
          </tr>
          <tr>
            <td colspan="2">&nbsp;</td>
            <td class=>${uiLabelMap.WebtoolsMaxRetries}</td>
            <td>${selectedServiceMap.maxRetry}</td>
          </tr>
        </table>
      </div>
    </div>

    <div class="screenlet">
      <div class="screenlet-title-bar">
        <h3>${uiLabelMap.SecurityGroups}</h3>
      </div>
      <#if selectedServiceMap.permissionGroups != 'NA'>
        <table class="basic-table" cellspacing="0">
         <thead>
          <tr class="header-row">
            <th>${uiLabelMap.WebtoolsNameOrRole}</th>
            <th>${uiLabelMap.WebtoolsPermissionType}</th>
            <th>${uiLabelMap.WebtoolsAction}</th>
          </tr>
         </thead> 
          <#list selectedServiceMap.permissionGroups as permGrp>
            <tr>
              <td>${permGrp.nameOrRole?default(uiLabelMap.CommonNA)}</td>
              <td>${permGrp.permType?default(uiLabelMap.CommonNA)}</td>
              <td>${permGrp.action?default(uiLabelMap.CommonNA)}</td>
            </tr>
          </#list>
        </table>
      <#else>
        <div class="screenlet-body">
          <b>${selectedServiceMap.permissionGroups}</b>
        </div>
      </#if>
    </div>

    <div class="screenlet">
      <div class="screenlet-title-bar">
        <h3>${uiLabelMap.WebtoolsImplementedServices}</h3>
      </div>
      <div class="screenlet-body">
        <#if selectedServiceMap.implServices == 'NA'>
          <b>${selectedServiceMap.implServices}</b>
        <#elseif selectedServiceMap.implServices?has_content>
          <#list selectedServiceMap.implServices as implSrv>
            <a href='<@ofbizUrl>${url}?sel_service_name=${implSrv.getService()}</@ofbizUrl>'>${implSrv.getService()}</a><br />
          </#list>
        </#if>
      </div>
    </div>

    <#-- If service has ECA's -->
    <#if ecaMapList?? && ecaMapList?has_content>
      <#-- add the javascript for modalpopup's -->
      <script language='javascript' type='text/javascript'>
          function detailsPopup(viewName){
              var lookupWinSettings = 'top=50,left=50,width=600,height=300,scrollbars=auto,status=no,resizable=no,dependent=yes,alwaysRaised=yes';
              var params = '';
              var lookupWin = window.open(viewName, params, lookupWinSettings);
              if(lookupWin.opener == null) lookupWin.opener = self;
              lookupWin.focus();
          }
      </script>
      <div class="screenlet">
        <div class="screenlet-title-bar">
          <h3>${uiLabelMap.WebtoolsServiceECA}</h3>
        </div>
        <table class="basic-table" cellspacing="0">
          <thead>
          <tr class="header-row">
            <th>${uiLabelMap.WebtoolsEventName}</th>
            <#if ecaMapList.runOnError??>
              <th>${uiLabelMap.WebtoolsRunOnError}</th>
            </#if>
            <#if ecaMapList.runOnFailure??>
              <th>${uiLabelMap.WebtoolsRunOnFailure}</th>
            </#if>
            <th>${uiLabelMap.WebtoolsActions}</th>
            <th>${uiLabelMap.WebtoolsConditions}</th>
            <th>${uiLabelMap.WebtoolsSet}</th>
          </tr>
          </thead>
          <#list ecaMapList as ecaMap>
            <tr>
              <td>${ecaMap.eventName!}</td>
              <#if ecaMap.runOnError??>
                <td>${ecaMap.runOnError}</div></td>
              </#if>
              <#if ecaMap.runOnFailure??>
                <td>${ecaMap.runOnFailure}</div></td>
              </#if>
              <#if ecaMap.actions?has_content>
                <td>
                  <#list ecaMap.actions as action>
                    <table class="basic-table" cellspacing='0'>
                      <tr>
                        <td colspan="2"><a href='<@ofbizUrl>${url}?sel_service_name=${action.serviceName}</@ofbizUrl>'>${action.serviceName?default(uiLabelMap.CommonNA)}</a></td>
                      </tr>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsSecasIgnoreError}</b> ${action.ignoreError?default(uiLabelMap.CommonNA)}</td>
                        <td><b>${uiLabelMap.WebtoolsSecasIgnoreFailure}</b> ${action.ignoreFailure?default(uiLabelMap.CommonNA)}</td>
                      </tr>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsSecasPersist}</b> ${action.persist?default(uiLabelMap.CommonNA)}</td>
                        <td><b>${uiLabelMap.WebtoolsSecasResultMapName}</b> ${action.resultMapName?default(uiLabelMap.CommonNA)}</td>
                      </tr>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsSecasResultToContext}</b> ${action.resultToContext?default(uiLabelMap.CommonNA)}</td>
                        <td><b>${uiLabelMap.WebtoolsSecasResultToResult}</b> ${action.resultToResult?default(uiLabelMap.CommonNA)}</td>
                      </tr>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsSecasServiceMode}</b> ${action.serviceMode?default(uiLabelMap.CommonNA)}</td>
                        <td colspan="2">&nbsp;</td>
                      </tr>
                    </table>
                  </#list>
                </td>
              </#if>
              <#if ecaMap.conditions?has_content>
                <td>
                  <#list ecaMap.conditions as condition>
                    <table class='basic-table' cellspacing='0'>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsCompareType}</b> ${condition.compareType?default(uiLabelMap.CommonNA)}</td>
                        <td>
                          <b>${uiLabelMap.WebtoolsConditionService}</b>
                          <#if condition.conditionService?has_content>
                            <a href='<@ofbizUrl>${url}?sel_service_name=${condition.conditionService}</@ofbizUrl>'>${condition.conditionService?default(uiLabelMap.CommonNA)}</a>
                          <#else>
                            ${condition.conditionService?default(uiLabelMap.CommonNA)}
                          </#if>
                        </td>
                        <td><b>${uiLabelMap.WebtoolsFormat}</b> ${condition.format?default(uiLabelMap.CommonNA)}</td>
                      </tr>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsIsService}</b> ${condition.isService?default(uiLabelMap.CommonNA)}</td>
                        <td><b>${uiLabelMap.WebtoolsIsConstant}</b> ${condition.isConstant?default(uiLabelMap.CommonNA)}</td>
                        <td><b>${uiLabelMap.WebtoolsOperator}</b> ${condition.operator?default(uiLabelMap.CommonNA)}</td>
                      </tr>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsLHSMapName}</b> ${condition.lhsMapName?default(uiLabelMap.CommonNA)}</td>
                        <td><b>${uiLabelMap.WebtoolsLHSValueName}</b> ${condition.lhsValueName?default(uiLabelMap.CommonNA)}</td>
                        <td>&nbsp;</td>
                      </tr>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsRHSMapName}</b> ${condition.rhsMapName?default(uiLabelMap.CommonNA)}</td>
                        <td><b>${uiLabelMap.WebtoolsRHSValueName}</b> ${condition.rhsValueName?default(uiLabelMap.CommonNA)}</td>
                        <td>&nbsp;</td>
                      </tr>
                    </table><br />
                  </#list>
                </td>
              </#if>
              <#if ecaMap.sets?has_content>
                <td>
                  <#list ecaMap.sets as set>
                    <table class='basic-table' cellspacing='0'>
                      <tr>
                        <td><b>${uiLabelMap.WebtoolsFieldName}</b> ${set.fieldName?default(uiLabelMap.CommonNA)}</td>
                        <td colspan="2">&nbsp;</td>
                      </tr>
                      <tr>
                        <#if set.envName?has_content>
                          <td><b>${uiLabelMap.WebtoolsEnvName}</b> ${set.envName}</td>
                          <td colspan="2">&nbsp;</td>
                        </#if>
                      </tr>
                      <tr>
                        <#if set.value?has_content>
                          <td><b>${uiLabelMap.CommonValue}</b> ${set.value}</td>
                          <td colspan="2">&nbsp;</td>
                        </#if>
                      </tr>
                      <tr>
                        <#if set.format?has_content>
                          <td><b>${uiLabelMap.WebtoolsFormat}</b> ${set.format}</td>
                          <td colspan="2">&nbsp;</td>
                        </#if>
                      </tr>
                    </table><br />
                  </#list>
                </td>
              </#if>
            </tr>
            <tr><td colspan='5'><hr/></td></tr>
          </#list>
        </table>
      </div>
    </#if>
    <#-- End if service has ECA's -->

    <#list selectedServiceMap.allParamsList! as paramList>
      <style type="text/css">
        .param-table tr td {
          width: 12.5%;
          vertical-align: top;
        }
      </style>
      <div class="screenlet">
        <div class="screenlet-title-bar">
          <h3>${paramList.title}</h3>
        </div>
        <#if paramList.paramList?? && paramList.paramList?has_content>
          <table class="basic-table param-table" cellspacing="0">
            <thead>
              <tr class="header-row">
                <th>${uiLabelMap.WebtoolsParameterName}</th>
                <th>${uiLabelMap.CommonDescription}</th>
                <th>${uiLabelMap.WebtoolsOptional}</th>
                <th>${uiLabelMap.CommonType}</th>
                <#-- <th>Default Value</th> -->
                <th>${uiLabelMap.WebtoolsMode}</th>
                <th>${uiLabelMap.WebtoolsIsSetInternally}</th>
                <th>${uiLabelMap.WebtoolsEntityName}</th>
                <th>${uiLabelMap.WebtoolsFieldName}</th>
              </tr>
            </thead>
              <#list paramList.paramList as modelParam>
                <tr>
                  <td>${modelParam.name!}</td>
                  <td>${modelParam.description!}</td>
                  <td>${modelParam.optional!}</td>
                  <td>${modelParam.type!}</td>
                  <#-- <td>[${modelParam.defaultValue!}]</td> -->
                  <td>${modelParam.mode!}</td>
                  <td>${modelParam.internal!}</td>
                  <td>
                    <#if modelParam.entityName??>
                      <a href='<@ofbizUrl>${url}?constraint=default_entity_name@${modelParam.entityName}</@ofbizUrl>'>${modelParam.entityName!}</a>
                    </#if>
                  </td>
                  <td>${modelParam.fieldName!}</td>
                </tr>
              </#list>
          </table>
        <#else>
          <div class="screenlet-body">
            ${uiLabelMap.WebtoolsNoParametersDefined}
          </div>
        </#if>
      </div>
    </#list>

    <#-- Show a little form for exportServiceEoModelBundle -->
    <div class="screenlet-body">
      <form name="exportServiceEoModelBundle" method="post" action="<@ofbizUrl>exportServiceEoModelBundle</@ofbizUrl>" class="basic-form">
        <input type="hidden" name="sel_service_name" value="${selectedServiceMap.serviceName}"/>
        <input type="hidden" name="serviceName" value="${selectedServiceMap.serviceName}"/>
        Save eomodeld to Local Path: <input type="text" name="eomodeldFullPath" value="${parameters.eomodeldFullPath!}" size="60"/>
        <input type="submit" name="submitButton" value="Export"/>
      </form>
    </div>
  </#if>
<#-- No Service selected , we list all-->
<#elseif servicesList?? && servicesList?has_content>

  <#-- Show alphabetical index -->
  <#if serviceNamesAlphaList?? && serviceNamesAlphaList?has_content>
      <div class="button-bar">
        <#assign isfirst=true>
        <#list serviceNamesAlphaList as alpha>
          <a href='<@ofbizUrl>${url}?constraint=alpha@${alpha}</@ofbizUrl>'>${alpha}</a>
          <#assign isfirst=false>
        </#list>
      </div>
  </#if>

  <div class="screenlet">
    <div class="screenlet-body">
      <label>${uiLabelMap.WebtoolsServicesListFor} ${dispatcherName?default(uiLabelMap.CommonNA)} (${servicesFoundCount} ${uiLabelMap.CommonFound})</label>
      <table class="basic-table hover-bar" cellspacing="0">
        <thead>
        <tr class="header-row">
          <th>${uiLabelMap.WebtoolsServiceName}</th>
          <th>${uiLabelMap.WebtoolsEngineName}</th>
          <th>${uiLabelMap.WebtoolsDefaultEntityName}</th>
          <th>${uiLabelMap.WebtoolsInvoke}</th>
          <th>${uiLabelMap.WebtoolsLocation}</th>
          <th>${uiLabelMap.WebtoolsDefinitionLocation}</th>
        </tr>
        </thead>
        <#assign alt_row = false>
        <#list servicesList as service>
          <tr<#if alt_row> class="alternate-row"</#if>>
            <td><a href='<@ofbizUrl>${url}?sel_service_name=${service.serviceName}</@ofbizUrl>'>${service.serviceName}</a></td>
            <td><a href='<@ofbizUrl>${url}?constraint=engine_name@${service.engineName?default(uiLabelMap.CommonNA)}</@ofbizUrl>'>${service.engineName}</a></td>
            <td><a href='<@ofbizUrl>${url}?constraint=default_entity_name@${service.defaultEntityName?default(uiLabelMap.CommonNA)}</@ofbizUrl>'>${service.defaultEntityName}</a></td>
            <td>${service.invoke}</td>
            <td><a href='<@ofbizUrl>${url}?constraint=location@${service.location?default(uiLabelMap.CommonNA)}</@ofbizUrl>'>${service.location}</a></td>
            <td><a href='<@ofbizUrl>${url}?constraint=definitionLocation@${service.definitionLocation}</@ofbizUrl>'>${service.definitionLocation}</a></td>
          </tr>
          <#assign alt_row = !alt_row>
        </#list>
      </table>
    </div>
  </div>
<#else>
  ${uiLabelMap.WebtoolsNoServicesFound}.
  <a href='<@ofbizUrl>${url}</@ofbizUrl>' class="smallSubmit">${uiLabelMap.CommonListAll}</a>
</#if>
