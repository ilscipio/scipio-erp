<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<html>
    <head>
        <title>${uiLabelMap.WebtoolsEntityReferenceChart}</title>
    </head>
    <frameset cols="30%,70%">
        <frame src="${encodeUrlList}" name="entityListFrame"/>
        <frame src="${encodeUrlMain}" name="entityFrame"/>
    </frameset>
    <noframes>
        <@heading>${uiLabelMap.CommonFrameAlert1}</@heading>
        <p>${uiLabelMap.CommonFrameAlert2}</p>
        <br />
    </noframes>
</html>
