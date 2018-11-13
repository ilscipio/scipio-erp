<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section title=uiLabelMap.WebtoolsEntityXMLRepresentation>
      <p>
      <#if xmlDoc?has_content>
        ${Static["org.ofbiz.base.util.UtilXml"].writeXmlDocument(xmlDoc)?replace("\n", "<br />")?replace("    ", "&nbsp;&nbsp;&nbsp;&nbsp;")}
      </#if>
      </p>
</@section>

