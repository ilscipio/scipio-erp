<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- ToDo: Refactor - these artifacts seem like a copy and paste job to me. The only thing changing is really just the title... -->

<@menu type="button">
    <@menuitem type="link" href=makePageUrl("ArtifactInfo") text=uiLabelMap.CommonFind class="+${styles.action_nav!} ${styles.action_find!}"/>
    <@menuitem type="link" href=makePageUrl("ArtifactInfo?reloadArtifacts=Y") text=uiLabelMap.CommonReload class="+${styles.action_run_session!} ${styles.action_reload!}"/>
    <@menuitem type="generic">
      <@modal id="artifacts_recent" label="Recently Viewed" linkClass="+${styles.menu_button_item_link!} ${styles.action_run_local!} ${styles.action_show!}">
        <#-- SCIPIO: simplified list and moved under modal -->
        <#if recentArtifactInfoList?has_content>
          <@panel title="Recently Viewed Artifacts">
            <ol><#list recentArtifactInfoList as recentArtifactInfo>
                <li>${recentArtifactInfo.type}: <@displayArtifactInfoLink type=recentArtifactInfo.type uniqueId=recentArtifactInfo.uniqueId displayName=recentArtifactInfo.displayName/></li>
            </#list></ol>
          </@panel>
        </#if>
      </@modal>
    </@menuitem>
</@menu>

<#if !artifactInfo??>

    <#assign artifactTypes = ["entity", "service", "form", "screen", "request", "view"]>

    <#-- add form here to specify artifact info name. -->
    <@section title="Search Names/Locations">
      <form name="ArtifactInfoByName" method="post" action="<@pageUrl>ArtifactInfo</@pageUrl>">
        <@field type="text" name="name" value=(parameters.name!) size="40" label="Names/Locations"/>
        <@field type="select" name="type" label="Type">
          <@field type="option" value=""/>
          <#list artifactTypes as artifactType>
            <@field type="option" value=artifactType>${artifactType}</@field>
          </#list>
        </@field>
        <@field type="hidden" name="findType" value="search"/>
        <@field type="submit" name="submitButton" value="Find" class="${styles.link_run_sys!} ${styles.action_find!}"/>
      </form>
    </@section>
    <@section title="Search By Name and Type">
      <form name="ArtifactInfoByNameAndType" method="post" action="<@pageUrl>ArtifactInfo</@pageUrl>">
          <@field type="text" name="name" value=(parameters.name!) size="40" label="Name"/>
          <@field type="text" name="location" value=(parameters.location!) size="60" label="Location"/>
          <@field type="select" name="type" label="Type">
            <#list artifactTypes as artifactType>
              <@field type="option" value=artifactType>${artifactType}</@field>
            </#list>
          </@field>
          <@field type="submit" name="submitButton" value="Lookup" class="${styles.link_run_sys!} ${styles.action_find!}"/>
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

    <#-- SCIPIO -->
    <#macro artifactInfoFileLink id type location text class="">
        <form name="${id}_form" id="${id}_form" method="post" action="<@pageUrl>ArtifactInfo</@pageUrl>" style="display:inline;">
            <input type="hidden" name="name" value="${location}"/>
            <input type="hidden" name="type" value="${type}"/>
            <input type="hidden" name="findType" value="search"/>
            <@field inline=true type="submit" submitType="link" href="javascript:$('#${escapeVal(id, 'js')}_form').submit(); void(0);"
                class="+${styles.action_run_sys!} ${styles.action_find!}" text=text/>
        </form>
    </#macro>

    <@heading>${uiLabelMap.WebtoolsArtifactInfo} (${artifactInfo.getDisplayType()}): ${artifactInfo.getDisplayName()}</@heading>
    <#if artifactInfo.getLocationURL()??>
        <#-- SCIPIO: Fixed href with new form + use relative locations -->
        <@section title="Definition">Defined in: 
            <@artifactInfoFileLink id="ArtifactInfoFile" type=artifactInfo.type location=("/"+artifactInfo.getRelativeLocation())
                class="+${styles.action_run_sys!} ${styles.action_find!}" text=artifactInfo.getRelativeLocation()/>
        </@section>
    </#if>

    <#if artifactInfo.getType() == "entity">
        <#if artifactInfo.modelEntity.getFieldsUnmodifiable()?has_content>
            <@section title="Entity Fields">
                <a href="<@pageUrl>FindGeneric?entityName=${artifactInfo.modelEntity.getEntityName()}&amp;find=true&amp;VIEW_SIZE=${getPropertyValue("webtools", "webtools.record.paginate.defaultViewSize")!50}&amp;VIEW_INDEX=0</@pageUrl>">All Entity Data</a>
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
            <#-- SCIPIO: Fixed link; added fallback for java -->
            <#assign servImplLoc = raw((artifactInfo.getImplementationLocation())!)>
            Impl Location: 
            <#if servImplLoc?has_content>
              <@artifactInfoFileLink id="ArtifactInfoServImplFile" type=artifactInfo.type location=servImplLoc
                  class="+${styles.action_run_sys!} ${styles.action_find!}" text=servImplLoc/>
            <#else>
              ${uiLabelMap.CommonNA}
            </#if>
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
                <@td><a href="<@pageUrl>ArtifactInfo?type=${artifactInfo.getType()}&amp;uniqueId=${ecaAction.getServiceName()}</@pageUrl>">${ecaAction.getServiceName()}</a></@td>
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
<#-- SCIPIO: This hardcoded encoding was probably due to stock hadn't configured the default yet...
<a href="<@pageUrl>ArtifactInfo?type=${type}&amp;uniqueId=${uniqueId?url('ISO-8859-1')}</@pageUrl>">${displayName}</a>-->
<a href="<@pageUrl>ArtifactInfo?type=${type}&amp;uniqueId=${raw(uniqueId)?url}</@pageUrl>">${displayName}</a>
</#macro>
