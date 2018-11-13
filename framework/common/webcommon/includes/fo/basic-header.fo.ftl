<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#escape x as x?xml>
        <fo:static-content flow-name="xsl-region-before">
          <fo:block font-size="${(layoutSettings.headerFontSize)!"14pt"}" text-align="center" margin-bottom="14pt">
            ${(layoutSettings.documentTitle)!}
          </fo:block>
        </fo:static-content>
</#escape>
