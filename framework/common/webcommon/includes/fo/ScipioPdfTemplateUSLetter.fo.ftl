<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#escape x as x?xml>
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format"
    <#-- inheritance -->
    <#if defaultFontFamily?has_content>font-family="${defaultFontFamily}"</#if>
>
    <fo:layout-master-set>
        <#-- Regular -->
        <fo:simple-page-master master-name="main-page-first"
               page-width="8.5in" page-height="11in"
               margin-top="0.1in" margin-bottom="0.4in"
               margin-left="0.6in" margin-right="0.4in">
              <fo:region-body   margin-top="4.2in" margin-bottom="0.4in" space-after="0.1in"/>
              <fo:region-before extent="1.2in" margin-top="0in"/>
              <fo:region-after  extent="0.4in" />
              <fo:region-start  extent="0in"/>
              <fo:region-end    extent="0in"/>
        </fo:simple-page-master>

        <fo:simple-page-master master-name="main-page-subsequent"
                page-width="8.5in" page-height="11in"
                margin-top="0.4in" margin-bottom="0.4in"
                margin-left="0.6in" margin-right="0.4in">
              <fo:region-body margin-top="1.5in" margin-bottom="0.4in"/>
              <!--<fo:region-before extent="1.2in"/>-->
              <fo:region-after  extent="0.4in" />
              <fo:region-start  extent="0in"/>
              <fo:region-end    extent="0in"/>
        </fo:simple-page-master>

        <#-- Landscape view -->
        <fo:simple-page-master master-name="main-page-landscape-first"
                page-width="11in" page-height="8.5in"
                margin-top="0.4in" margin-bottom="0.4in"
                margin-left="0.6in" margin-right="0.4in">
            <fo:region-body margin-top="1.2in" margin-bottom="0.4in" space-after="0.1in"/>
            <fo:region-before extent="1.2in"/>
            <fo:region-after extent="0.4in"/>
            <fo:region-end extent="0in"/>
        </fo:simple-page-master>

        <fo:simple-page-master master-name="main-page-landscape-subsequent"
                page-width="11in" page-height="8.5in"
                margin-top="0.4in" margin-bottom="0.4in"
                margin-left="0.6in" margin-right="0.4in">
            <fo:region-body margin-top="1.2in" margin-bottom="0.4in" space-after="0.1in"/>
            <!--<fo:region-before extent="1.2in"/>-->
            <fo:region-after extent="0.4in"/>
            <fo:region-end extent="0in"/>
        </fo:simple-page-master>

        <#-- Sequence -->
        <fo:page-sequence-master master-name="page-sequence">
            <fo:repeatable-page-master-alternatives>
                <fo:conditional-page-master-reference page-position="first" master-reference="${pageLayoutName!"main-page"}-first"/>
                <fo:conditional-page-master-reference page-position="rest" master-reference="${pageLayoutName!"main-page"}-subsequent"/>
            </fo:repeatable-page-master-alternatives>
        </fo:page-sequence-master>
    </fo:layout-master-set>




    <#-- ************* -->
    <#-- Page Content -->
    <#-- ************* -->
    <fo:page-sequence master-reference="page-sequence">

        <#-- Header -->
        <#-- The elements it it are positioned using a table composed by one row
             composed by two cells (each 50% of the total table that is 100% of the page):
             in the left side cell the "topLeft" template is included
             in the right side cell the "topRight" template is included
        -->
        <fo:static-content flow-name="xsl-region-before">
            <fo:table table-layout="fixed" width="100%" >
                <fo:table-column column-number="1" padding-right="0.4in"/><#-- invalid attr: padding-left="5mm" padding-right="5mm" -->
                <fo:table-column column-number="2" column-width="3.2in"/><#-- invalid attr: padding-left="5mm" padding-right="5mm" -->
                <fo:table-body>
                    <fo:table-row>
                        <fo:table-cell><fo:block>${sections.render("topLeft")}</fo:block></fo:table-cell>
                        <fo:table-cell><fo:block>${sections.render("topRight")}</fo:block></fo:table-cell>
                    </fo:table-row>
                </fo:table-body>
            </fo:table>
        </fo:static-content>

        <#-- the footer -->
        <fo:static-content flow-name="xsl-region-after">
            <fo:block>
                ${sections.render("footer")}
            </fo:block>
        </fo:static-content>

        <#-- page number -->
        <fo:static-content flow-name="xsl-region-end">
            <fo:block font-size="8pt" text-align="center">
                <#-- ${uiLabelMap.CommonPage} <fo:page-number/> ${uiLabelMap.CommonOf}  --><fo:page-number/>/<fo:page-number-citation ref-id="theEnd"/>
            </fo:block>
        </fo:static-content>

        <#-- the body -->
        <fo:flow flow-name="xsl-region-body">
            ${sections.render("body")}
            <fo:block id="theEnd"/>  <#-- marks the end of the pages and used to identify page-number at the end -->
        </fo:flow>
    </fo:page-sequence>
</fo:root>
</#escape>