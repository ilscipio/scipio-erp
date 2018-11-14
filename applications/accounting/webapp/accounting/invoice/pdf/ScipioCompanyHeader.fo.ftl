<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
<fo:block-container height="45mm" margin-bottom="5mm" display-align="center">
    <fo:block>
        <#if logoImageUrl?has_content><fo:external-graphic src="<@ofbizContentUrl>${logoImageUrl}</@ofbizContentUrl>" overflow="hidden" height="45mm" content-height="scale-to-fit" content-width="scale-to-fit" width="100%" scaling="uniform"/></#if>
    </fo:block>
</fo:block-container>
</#escape>
