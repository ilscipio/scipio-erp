<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
<#if layoutSettings.styleSheets?has_content>
  <#--layoutSettings.styleSheets is a list of style sheets -->
  <#list layoutSettings.styleSheets as styleSheet>
    <?xml-stylesheet type="text/xsl" href="<@ofbizContentUrl>${styleSheet}</@ofbizContentUrl>"?>
  </#list>
</#if>
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format"
    font-family="${(layoutSettings.defaultFontFamily)!"Helvetica, sans-serif"}"
    font-size="${(layoutSettings.defaultFontSize)!"12pt"}">
  <fo:layout-master-set>
<#if layoutSettings.pageMasters?has_content>
  <#--layoutSettings.pageMasters is a list of fo page master element ftl templates -->
  <#list layoutSettings.pageMasters as pageMaster>
    <#include pageMaster/>
  </#list>
<#else>
  <#include "component://common/webcommon/includes/fo/pm-11x17.fo.ftl"/>
  <#include "component://common/webcommon/includes/fo/pm-iso216.fo.ftl"/>
  <#include "component://common/webcommon/includes/fo/pm-legal.fo.ftl"/>  
  <#include "component://common/webcommon/includes/fo/pm-letter.fo.ftl"/>
</#if>
  </fo:layout-master-set>
  <#assign masterReference = (layoutSettings.masterReference)!"letter-portrait"/>
  <fo:page-sequence master-reference="${masterReference}">
</#escape>
