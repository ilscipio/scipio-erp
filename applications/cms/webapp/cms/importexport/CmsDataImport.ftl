<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- SCIPIO: based on component://webtools/webapp/webtools/entity/EntityImport.ftl -->
<#assign eiActionUri = "importCmsData">
<#assign eiAllowServerLocs = security.hasPermission("ENTITY_MAINT", request)>
<#assign eiUnsafeFieldOpt = false>
<#assign eiInfoMsg>
  ${uiLabelMap.WebtoolsXMLImportInfo}
  ${uiLabelMap.CommonNote}: ${uiLabelMap.CmsDataImportAdminLinkDesc} (<a href="<@serverUrl escapeAs='html' uri='/admin/control/EntityImport' extLoginKey=true/>">${uiLabelMap.PageTitleEntityImport}</a>, 
    <a href="<@serverUrl uri='/admin/control/EntityImportDir' extLoginKey=true escapeAs='html'/>">${uiLabelMap.PageTitleEntityImportDir}</a>)
</#assign>
<#assign eiShowMsgs = false>
<#include "component://webtools/webapp/webtools/entity/EntityImport.ftl">
