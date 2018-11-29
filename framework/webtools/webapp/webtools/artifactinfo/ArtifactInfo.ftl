<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- ToDo: Refactor - these artifacts seem like a copy and paste job to me. The only thing changing is really just the title... -->

<#-- SCIPIO: simplified list -->
<#if recentArtifactInfoList?has_content>
  <@panel title="Recently Viewed Artifacts:">
    <ol><#list recentArtifactInfoList as recentArtifactInfo>
        <li>${recentArtifactInfo.type}: <@displayArtifactInfoLink type=recentArtifactInfo.type uniqueId=recentArtifactInfo.uniqueId displayName=recentArtifactInfo.displayName/></li>
    </#list></ol>
  </@panel>
</#if>

<#if !artifactInfo??>

    <#-- add form here to specify artifact info name. -->
    <@section>
      <form name="ArtifactInfoByName" method="post" action="<@ofbizUrl>ArtifactInfo</@ofbizUrl>" class="basic-form">
        Search Names/Locations: <input type="text" name="name" value="${parameters.name!}" size="40"/>
        <select name="type">
          <option></option>
          <option>entity</option>
          <option>service</option>
          <option>form</option>
          <option>screen</option>
          <option>request</option>
          <option>view</option>
        </select>
        <input type="hidden" name="findType" value="search"/>
        <input type="submit" name="submitButton" value="Find" class="${styles.link_run_sys!} ${styles.action_find!}"/>
      </form>
    </@section>
    <@section>
      <form name="ArtifactInfoByNameAndType" method="post" action="<@ofbizUrl>ArtifactInfo</@ofbizUrl>" class="basic-form">
        <div>Name: <input type="text" name="name" value="${parameters.name!}" size="40"/></div>
        <div>Location: <input type="text" name="location" value="${parameters.location!}" size="60"/></div>
        <div>Type:
          <select name="type">
            <option>entity</option>
            <option>service</option>
            <option>form</option>
            <option>screen</option>
            <option>request</option>
            <option>view</option>
          </select>
          <input type="submit" name="submitButton" value="Lookup" class="${styles.link_run_sys!} ${styles.action_find!}"/>
        </div>
      </form>
    </@section>

    <#-- add set of ArtifactInfo if there is not a single one identified, with link to each -->
    <#if artifactInfoSet?has_content>
    <@section>
        <@heading>Multiple Artifacts Found:</@heading>
        <ol>
        <ol><#list artifactInfoSet as curArtifactInfo>
            <li>${curArtifactInfo.getDisplayType()}: <@displayArtifactInfo artifactInfo=curArtifactInfo/></li>
        </#list></ol>
        <ol>
    </@section>
    </#if>

<#else>

    <@heading>${uiLabelMap.WebtoolsArtifactInfo} (${artifactInfo.getDisplayType()}): ${artifactInfo.getDisplayName()}</@heading>
    <#if artifactInfo.getLocationURL()??>
        <@section title="Definition">Defined in: <a href="${artifactInfo.getLocationURL()}">${artifactInfo.getLocationURL()}</a></@section>
    </#if>

    <#if artifactInfo.getType() == "entity">
        <#if artifactInfo.modelEntity.getFieldsUnmodifiable()?has_content>
            <@section title="Entity Fields">
                <a href="<@ofbizUrl>FindGeneric?entityName=${artifactInfo.modelEntity.getEntityName()}&amp;find=true&amp;VIEW_SIZE=${getPropertyValue("webtools", "webtools.record.paginate.defaultViewSize")!50}&amp;VIEW_INDEX=0</@ofbizUrl>">All Entity Data</a>
                <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}">
                <#list artifactInfo.modelEntity.getFieldsUnmodifiable() as modelField>
                    <@tr><@td>${modelField.getName()}<#if modelField.getIsPk()>*</#if></@td><@td>${modelField.getType()}</@td><@td>${modelField.getDescription()!}</@td></@tr>
                </#list>
                </@table>
            </@section>
        </#if>
        
        <#if artifactInfo.getEntitiesRelatedOne()?has_content>
            <@section title="Entities Related (One)">
                <ol><#list artifactInfo.getEntitiesRelatedOne()! as entityArtifactInfo>
                    <@displayEntityArtifactInfo entityArtifactInfo=entityArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getEntitiesRelatedMany()?has_content>
            <@section title="Entities Related (Many)">
                <ol><#list artifactInfo.getEntitiesRelatedMany()! as entityArtifactInfo>
                    <@displayEntityArtifactInfo entityArtifactInfo=entityArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getServicesUsingEntity()?has_content>
            <@section title="Services Using This Entit">
                <ol><#list artifactInfo.getServicesUsingEntity()! as serviceArtifactInfo>
                    <@displayServiceArtifactInfo serviceArtifactInfo=serviceArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>
        
        <#if artifactInfo.getFormsUsingEntity()?has_content>
            <@section title="Forms Using This Entity">
                <ol><#list artifactInfo.getFormsUsingEntity()! as formWidgetArtifactInfo>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=formWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>
    
        <#if artifactInfo.getScreensUsingEntity()?has_content>
            <@section title="Screens Using This Entity">
                <ol><#list artifactInfo.getScreensUsingEntity()! as screenWidgetArtifactInfo>
                    <@displayScreenWidgetArtifactInfo screenWidgetArtifactInfo=screenWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

    <#elseif artifactInfo.getType() == "service"/>
        <@section title="Service Info">
            Description: ${artifactInfo.modelService.description}<br/>
            Run (${artifactInfo.modelService.engineName}): ${artifactInfo.modelService.location} :: ${artifactInfo.modelService.invoke}<br/>
            Impl Location: <a href="${artifactInfo.getImplementationLocationURL()!}">${artifactInfo.getImplementationLocationURL()!}</a>
        </@section>
        
        <#if artifactInfo.modelService.getAllParamNames()?has_content>
            <@section title="Service Parameters">
                <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}">
                    <@thead><@tr><@td>Name</@td><@td>Type</@td><@td>Optional</@td><@td>Mode</@td><@td>Entity.field</@td></@tr></@thead>
                  <#list artifactInfo.modelService.getAllParamNames() as paramName>
                    <#assign modelParam = artifactInfo.modelService.getParam(paramName)/>
                    <@tr><@td>${modelParam.getName()}<#if modelParam.getInternal()> (internal)</#if></@td><@td>${modelParam.getType()}</@td><@td><#if modelParam.isOptional()>optional<#else>required</#if></@td><@td>${modelParam.getMode()}</@td><@td>${modelParam.getEntityName()!}.${modelParam.getFieldName()!}</@td></@tr>
                </#list>
                </@table>
            </@section>
        </#if>
        
        <#if artifactInfo.getEntitiesUsedByService()?has_content>
            <@section title="Entities Used By This Service">
                <ol><#list artifactInfo.getEntitiesUsedByService()! as entityArtifactInfo>
                    <@displayEntityArtifactInfo entityArtifactInfo=entityArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getServicesCallingService()?has_content>
            <@section title="Services Calling This Service">
                <ol><#list artifactInfo.getServicesCallingService()! as serviceArtifactInfo>
                    <@displayServiceArtifactInfo serviceArtifactInfo=serviceArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getServicesCalledByService()?has_content>
            <@section title="Services Called By This Service">
                <ol><#list artifactInfo.getServicesCalledByService()! as serviceArtifactInfo>
                    <@displayServiceArtifactInfo serviceArtifactInfo=serviceArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getServiceEcaRulesTriggeredByService()?has_content>
            <@section title="Service ECA Rules Triggered By This Service">
                <ol><#list artifactInfo.getServiceEcaRulesTriggeredByService()! as serviceEcaArtifactInfo>
                    <@displayServiceEcaArtifactInfo serviceEcaArtifactInfo=serviceEcaArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getServiceEcaRulesCallingService()?has_content>
            <@section title="Service ECA Rules Calling This Service">
                <ol><#list artifactInfo.getServiceEcaRulesCallingService()! as serviceEcaArtifactInfo>
                    <@displayServiceEcaArtifactInfo serviceEcaArtifactInfo=serviceEcaArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getFormsCallingService()?has_content>
            <@section title="Forms Calling This Service">
                <ol><#list artifactInfo.getFormsCallingService()! as formWidgetArtifactInfo>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=formWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getFormsBasedOnService()?has_content>
            <@section title="Forms Based On This Service">
                <ol><#list artifactInfo.getFormsBasedOnService()! as formWidgetArtifactInfo>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=formWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getScreensCallingService()?has_content>
            <@section title="Screens Calling This Service">
                <ol><#list artifactInfo.getScreensCallingService()! as screenWidgetArtifactInfo>
                    <@displayScreenWidgetArtifactInfo screenWidgetArtifactInfo=screenWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getRequestsWithEventCallingService()?has_content>
            <@section title="Requests with Events That Call This Service">
                <ol><#list artifactInfo.getRequestsWithEventCallingService()! as controllerRequestArtifactInfo>
                    <@displayControllerRequestArtifactInfo controllerRequestArtifactInfo=controllerRequestArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

    <#elseif artifactInfo.getType() == "form"/>
        <#if artifactInfo.getFormThisFormExtends()?has_content>
            <@section title="Form Extended by This Form">
                <#if artifactInfo.getFormThisFormExtends()??>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=artifactInfo.getFormThisFormExtends()/>
                </#if>
            </@section>
        </#if>

        <#if artifactInfo.getEntitiesUsedInForm()?has_content>
            <@section title="Entities Used in This Form">
                <ol><#list artifactInfo.getEntitiesUsedInForm()! as entityArtifactInfo>
                    <@displayEntityArtifactInfo entityArtifactInfo=entityArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getServicesUsedInForm()?has_content>
            <@section title="Services Used in This Form">
                <ol><#list artifactInfo.getServicesUsedInForm()! as serviceArtifactInfo>
                    <@displayServiceArtifactInfo serviceArtifactInfo=serviceArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getFormsExtendingThisForm()?has_content>
            <@section title="Forms Extending This Form">
                <ol><#list artifactInfo.getFormsExtendingThisForm()! as formWidgetArtifactInfo>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=formWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getScreensIncludingThisForm()?has_content>
            <@section title="Screens Including This Form">
                <ol><#list artifactInfo.getScreensIncludingThisForm()! as screenWidgetArtifactInfo>
                    <@displayScreenWidgetArtifactInfo screenWidgetArtifactInfo=screenWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getRequestsLinkedToInForm()?has_content>
            <@section title="Controller Requests That Are Linked to in This Form">
                <ol><#list artifactInfo.getRequestsLinkedToInForm()! as controllerRequestArtifactInfo>
                    <@displayControllerRequestArtifactInfo controllerRequestArtifactInfo=controllerRequestArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>
        
        <#if artifactInfo.getRequestsTargetedByForm()?has_content>
            <@section title="Controller Requests That Are Targeted By This Form">
                <ol><#list artifactInfo.getRequestsTargetedByForm()! as controllerRequestArtifactInfo>
                    <@displayControllerRequestArtifactInfo controllerRequestArtifactInfo=controllerRequestArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

    <#elseif artifactInfo.getType() == "screen"/>
    
        <#if artifactInfo.getEntitiesUsedInScreen()?has_content>
            <@section title="Entities Used in This Screen">
                <ol><#list artifactInfo.getEntitiesUsedInScreen()! as entityArtifactInfo>
                    <@displayEntityArtifactInfo entityArtifactInfo=entityArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getServicesUsedInScreen()?has_content>
            <@section title="Services Used in This Screen">
                <ol><#list artifactInfo.getServicesUsedInScreen()! as serviceArtifactInfo>
                    <@displayServiceArtifactInfo serviceArtifactInfo=serviceArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getFormsIncludedInScreen()?has_content>
            <@section title="Forms Included in This Screen">
                <ol><#list artifactInfo.getFormsIncludedInScreen()! as formWidgetArtifactInfo>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=formWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getScreensIncludedInScreen()?has_content>
            <@section title="Screens Include in This Screen">
                <ol><#list artifactInfo.getScreensIncludedInScreen()! as screenWidgetArtifactInfo>
                    <@displayScreenWidgetArtifactInfo screenWidgetArtifactInfo=screenWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getScreensIncludingThisScreen()?has_content>
            <@section title="Screens Including This Screen">
                <ol><#list artifactInfo.getScreensIncludingThisScreen()! as screenWidgetArtifactInfo>
                    <@displayScreenWidgetArtifactInfo screenWidgetArtifactInfo=screenWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getRequestsLinkedToInScreen()?has_content>
            <@section title="Controller Requests That Are Linked to in This Screen">
                <ol><#list artifactInfo.getRequestsLinkedToInScreen()! as controllerRequestArtifactInfo>
                    <@displayControllerRequestArtifactInfo controllerRequestArtifactInfo=controllerRequestArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getViewsReferringToScreen()?has_content>
            <@section title="Controller Views Referring to This Screen">
                <ol><#list artifactInfo.getViewsReferringToScreen()! as controllerViewArtifactInfo>
                    <@displayControllerViewArtifactInfo controllerViewArtifactInfo=controllerViewArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

    <#elseif artifactInfo.getType() == "request"/>
        <#if artifactInfo.getServiceCalledByRequestEvent()??>
            <@section title="Service Called by Request Event">
              <ol><@displayServiceArtifactInfo serviceArtifactInfo=artifactInfo.getServiceCalledByRequestEvent()/></ol>
            </@section>
        </#if>

        <#if artifactInfo.getFormInfosReferringToRequest()?has_content>
            <@section title="Forms Referring to This Request">
                <ol><#list artifactInfo.getFormInfosReferringToRequest()! as formWidgetArtifactInfo>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=formWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>
        
        <#if artifactInfo.getFormInfosTargetingRequest()?has_content>
            <@section title="Forms Targeting This Request">
                <ol><#list artifactInfo.getFormInfosTargetingRequest()! as formWidgetArtifactInfo>
                    <@displayFormWidgetArtifactInfo formWidgetArtifactInfo=formWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>
        
        <#if artifactInfo.getScreenInfosReferringToRequest()?has_content>
            <@section title="Screens Referring to This Request">
                <ol><#list artifactInfo.getScreenInfosReferringToRequest()! as screenWidgetArtifactInfo>
                    <@displayScreenWidgetArtifactInfo screenWidgetArtifactInfo=screenWidgetArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getRequestsThatAreResponsesToThisRequest()?has_content>
            <@section title="Requests That Are Responses to This Request">
                <ol><#list artifactInfo.getRequestsThatAreResponsesToThisRequest()! as controllerRequestArtifactInfo>
                    <@displayControllerRequestArtifactInfo controllerRequestArtifactInfo=controllerRequestArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getRequestsThatThisRequestIsResponsTo()?has_content>        
            <@section title="Requests That This Request is a Responses To">
                <ol><#list artifactInfo.getRequestsThatThisRequestIsResponsTo()! as controllerRequestArtifactInfo>
                    <@displayControllerRequestArtifactInfo controllerRequestArtifactInfo=controllerRequestArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>
        
        <#if artifactInfo.getViewsThatAreResponsesToThisRequest()?has_content>
            <@section title="Controller Views That Are Responses to This Request">
                <ol><#list artifactInfo.getViewsThatAreResponsesToThisRequest()! as controllerViewArtifactInfo>
                    <@displayControllerViewArtifactInfo controllerViewArtifactInfo=controllerViewArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

    <#elseif artifactInfo.getType() == "view"/>
        <#if artifactInfo.getRequestsThatThisViewIsResponseTo()?has_content>
            <@section title="Requests That This View is a Responses To">
                <ol><#list artifactInfo.getRequestsThatThisViewIsResponseTo()! as controllerRequestArtifactInfo>
                    <@displayControllerRequestArtifactInfo controllerRequestArtifactInfo=controllerRequestArtifactInfo/>
                </#list></ol>
            </@section>
        </#if>

        <#if artifactInfo.getScreenCalledByThisView()??>
            <@section title="Screen Called by This View">
                <@displayScreenWidgetArtifactInfo screenWidgetArtifactInfo=artifactInfo.getScreenCalledByThisView()/>
            </@section>
        </#if>

    </#if>
</#if>

<#-- ==================== MACROS ===================== -->
<#macro displayEntityArtifactInfo entityArtifactInfo>
    <li><@displayArtifactInfo artifactInfo=entityArtifactInfo/></li>
</#macro>

<#macro displayServiceArtifactInfo serviceArtifactInfo>
    <li><@displayArtifactInfo artifactInfo=serviceArtifactInfo/></li>
</#macro>

<#macro displayServiceEcaArtifactInfo serviceEcaArtifactInfo>
  <li>
    <h4>Service ECA Rule: ${serviceEcaArtifactInfo.getDisplayPrefixedName()}</h4>
    <#if serviceEcaArtifactInfo.serviceEcaRule.getEcaConditionList()?has_content>
        <h4>ECA Rule Conditions</h4>
        <ol>
        <#list serviceEcaArtifactInfo.serviceEcaRule.getEcaConditionList() as ecaCondition>
            <div>${ecaCondition.getShortDisplayDescription(true)}</div>
        </#list>
        </ol>
    </#if>
    <#if serviceEcaArtifactInfo.serviceEcaRule.getEcaActionList()?has_content>
        <h4>ECA Rule Actions</h4>
        <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}">
        <#list serviceEcaArtifactInfo.serviceEcaRule.getEcaActionList() as ecaAction>
            <@tr>
                <@td><a href="<@ofbizUrl>ArtifactInfo?type=${artifactInfo.getType()}&amp;uniqueId=${ecaAction.getServiceName()}</@ofbizUrl>">${ecaAction.getServiceName()}</a></@td>
                <@td>${ecaAction.getServiceMode()}<#if ecaAction.isPersist()>-persisted</#if></@td>
            </@tr>
        </#list>
        </@table>
    </#if>

    <#-- leaving this out, will show service links for actions
    <#if serviceEcaArtifactInfo.getServicesCalledByServiceEcaActions()?has_content>
        <h4>Services Called By Service ECA Actions</h4>
        <ol><#list serviceEcaArtifactInfo.getServicesCalledByServiceEcaActions() as serviceArtifactInfo>
            <@displayServiceArtifactInfo serviceArtifactInfo=serviceArtifactInfo/>
        </#list></ol>
    </#if>
    -->
    <#if serviceEcaArtifactInfo.getServicesTriggeringServiceEca()?has_content>
        <h4>Services Triggering Service ECA</h4>
        <ol><#list serviceEcaArtifactInfo.getServicesTriggeringServiceEca() as serviceArtifactInfo>
            <@displayServiceArtifactInfo serviceArtifactInfo=serviceArtifactInfo/>
        </#list></ol>
    </#if>
  </li>
</#macro>

<#macro displayFormWidgetArtifactInfo formWidgetArtifactInfo>
    <li><@displayArtifactInfo artifactInfo=formWidgetArtifactInfo/></li>
</#macro>

<#macro displayScreenWidgetArtifactInfo screenWidgetArtifactInfo>
    <li><@displayArtifactInfo artifactInfo=screenWidgetArtifactInfo/></li>
</#macro>

<#macro displayControllerRequestArtifactInfo controllerRequestArtifactInfo>
    <li><@displayArtifactInfo artifactInfo=controllerRequestArtifactInfo/></li>
</#macro>

<#macro displayControllerViewArtifactInfo controllerViewArtifactInfo>
    <li><@displayArtifactInfo artifactInfo=controllerViewArtifactInfo/></li>
</#macro>

<#macro displayArtifactInfo artifactInfo>
    <@displayArtifactInfoLink type=artifactInfo.getType() uniqueId=artifactInfo.getUniqueId() displayName=artifactInfo.getDisplayName()/>
</#macro>

<#macro displayArtifactInfoLink type uniqueId displayName>
<a href="<@ofbizUrl>ArtifactInfo?type=${type}&amp;uniqueId=${uniqueId?url('ISO-8859-1')}</@ofbizUrl>">${displayName}</a>
</#macro>
