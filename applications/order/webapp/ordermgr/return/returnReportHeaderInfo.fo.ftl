<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#escape x as x?xml>
          <fo:table><fo:table-column column-width="0.3in"/><fo:table-body><fo:table-row><fo:table-cell>
            <fo:table font-size="10pt">
            <fo:table-column column-width="1in"/>
            <fo:table-column column-width="1in"/>
            <fo:table-column column-width="1in"/>
            <fo:table-body>

            <fo:table-row>
              <fo:table-cell number-columns-spanned="3">
                <fo:block space-after="2mm" font-size="14pt" font-weight="bold" text-align="right">${uiLabelMap.OrderReturnSummary}</fo:block>
              </fo:table-cell>
            </fo:table-row>

            <fo:table-row>
              <fo:table-cell text-align="center" border-style="solid" border-width="0.2pt">
                <fo:block padding="1mm" font-weight="bold">${uiLabelMap.CommonDate}</fo:block>
              </fo:table-cell>
              <fo:table-cell text-align="center" border-style="solid" border-width="0.2pt">
                <fo:block padding="1mm" font-weight="bold">${uiLabelMap.OrderReturnId}</fo:block>
              </fo:table-cell>
              <fo:table-cell text-align="center" border-style="solid" border-width="0.2pt">
                <fo:block padding="1mm" font-weight="bold">${uiLabelMap.CommonStatus}</fo:block>
              </fo:table-cell>
            </fo:table-row>

            <fo:table-row>
              <fo:table-cell text-align="center" border-style="solid" border-width="0.2pt">
                <fo:block padding="1mm">${returnHeader.entryDate?string("yyyy-MM-dd")}</fo:block>
              </fo:table-cell>
              <fo:table-cell text-align="center" border-style="solid" border-width="0.2pt">
                <fo:block padding="1mm">${returnId}</fo:block>
              </fo:table-cell>
              <fo:table-cell text-align="center" border-style="solid" border-width="0.2pt">
                <fo:block padding="1mm">${currentStatus.get("description",locale)}</fo:block>
              </fo:table-cell>
            </fo:table-row>

          </fo:table-body></fo:table>
        </fo:table-cell></fo:table-row></fo:table-body></fo:table>
</#escape>

