<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.WebtoolsEntityXMLRepresentation>
      <p>
      <#if xmlDoc?has_content>
        <#if (hasTmplTestPerm!true) == true><#-- SCIPIO -->
          <#-- SCIPIO: NOTE: this escapeVal prevents the aggressive screen html and uses the ftl ?html instead, so the replaces can work again (historical bug?) -->
          ${escapeVal(Static["org.ofbiz.base.util.UtilXml"].writeXmlDocument(xmlDoc), 'html')?replace("\n", "<br/>")?replace("    ", "&nbsp;&nbsp;&nbsp;&nbsp;")}
        <#else>
          <@alert type="error">${uiLabelMap.WebtoolsPermissionError}</@alert>
        </#if>
      </#if>
      </p>
</@section>

