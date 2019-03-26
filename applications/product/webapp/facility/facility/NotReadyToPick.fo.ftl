<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
        <fo:layout-master-set>
            <fo:simple-page-master master-name="main" page-height="11in" page-width="8.5in"
                    margin-top="0.5in" margin-bottom="1in" margin-left=".5in" margin-right="1in">
                <fo:region-body margin-top="1in"/>
                <fo:region-before extent="1in"/>
                <fo:region-after extent="1in"/>
            </fo:simple-page-master>
        </fo:layout-master-set>
        
        <fo:page-sequence master-reference="main">
            <fo:flow flow-name="xsl-region-body" font-family="Helvetica">
                <#include "component://order/webapp/ordermgr/order/companyHeader.fo.ftl"/>
                <fo:table border-width="1pt" border-style="solid">
                    <fo:table-column column-width="250pt"/>
                    <fo:table-body>
                        <fo:table-row>
                            <fo:table-cell>
                                <fo:block>${uiLabelMap.ProductOrderNotReadyForPickNeedStockMove}</fo:block>
                            </fo:table-cell>
                        </fo:table-row>
                    </fo:table-body>
                </fo:table>
           </fo:flow>
        </fo:page-sequence>
     </fo:root>
</#escape>