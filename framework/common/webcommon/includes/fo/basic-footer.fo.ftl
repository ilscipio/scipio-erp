<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
        <fo:static-content flow-name="xsl-region-after" font-size="${(layoutSettings.footerFontSize)!"8pt"}">
            <fo:block text-align="center" border-top="thin solid black">Copyright (c) 2001-${nowTimestamp?string("yyyy")} The Apache Software Foundation</fo:block>
            <fo:block text-align="center">${uiLabelMap.CommonPage} <fo:page-number/></fo:block>
        </fo:static-content>
</#escape>
