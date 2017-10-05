<#include "component://setup/webapp/setup/common/common.ftl">

<#-- DEPRECATED: form merged into store page - unmaintained, reference only -->

<#assign defaultParams = {
    "visualThemeSetId": defaultVisualThemeSetId!,
    "webSiteId": defaultWebSiteId!
}>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":webSite!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id=submitFormId action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["webSiteId", "partyId", "productStoreId"]/>
        <@field type="hidden" name="isCreateWebsite" value=(webSite??)?string("N","Y")/>

        <#--
        <actions><set field="webSiteId" from-field="webSite.webSiteId"/></actions>
        <field use-when="webSite==null&amp;&amp;webSiteId==null" name="webSiteId" required-field="true"><text default-value="ScipioWebStore"/></field>
        -->
      <#if webSite??>
        <@field type="display" label=uiLabelMap.FormFieldTitle_webSiteId><#rt/>
            <@setupExtAppLink uri="/catalog/control/EditWebSite?webSiteId=${rawString(params.webSiteId!)}" text=(params.webSiteId!)/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="webSiteId" value=(params.webSiteId!)/> 
      <#else>
        <@field type="input" name="webSiteId" label=uiLabelMap.FormFieldTitle_webSiteId value=(params.webSiteId!) placeholder=(defaultInitialWebSiteId!)/>
      </#if>
        
        <@field type="input" name="siteName" label=uiLabelMap.FormFieldTitle_siteName value=(params.siteName!) required=true size="30" maxlength="60"/>
        <@field type="hidden" name="visualThemeSetId" value=(fixedParams.visualThemeSetId!)/>
        <@field type="hidden" name="partyId" value=(partyId!)/>
        
        <@field type="hidden" name="httpHost" value=(params.httpHost!)/>
        <@field type="hidden" name="httpPort" value=(params.httpPort!)/>
        <@field type="hidden" name="httpsHost" value=(params.httpsHost!)/>
        <@field type="hidden" name="httpsPort" value=(params.httpsPort!)/>
        <@field type="hidden" name="enableHttps" value=(params.enableHttps!)/>
        <@field type="hidden" name="standardContentPrefix" value=(params.standardContentPrefix!)/>
        <@field type="hidden" name="secureContentPrefix" value=(params.secureContentPrefix!)/>
        <@field type="hidden" name="cookieDomain" value=(params.cookieDomain!)/>
        <@field type="hidden" name="productStoreId" value=(params.productStoreId!)/>
        <@field type="hidden" name="allowProductStoreChange" value=(params.allowProductStoreChange!)/>
    </@form>

