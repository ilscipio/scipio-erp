<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#-- FOP may require a library called JIMI to print certain graphical formats such as GIFs.  Jimi is a Sun library which cannot
be included in OFBIZ due to licensing incompatibility, but you can download it yourself at: http://java.sun.com/products/jimi/
and rename the ZIP file that comes with it as jimi-xxx.jar, then copy it into the same directory as fop.jar, which at this time
is ${ofbiz.home}/framework/webapp/lib/ -->

<#-- SCIPIO: WARN: 2018-10-17: This FTL can currently only be called if a HttpServletRequest is available, due to accessToken;
    the accessToken here is scoped to the request with weak keys; it is lost as soon as the rendering is over. -->
<#if !viewShipAccessToken?? && request??>
  <#assign viewShipAccessToken = Static["org.ofbiz.shipment.shipment.ShipmentEvents"].getShipmentViewRequestAccessTokenString(request)>
</#if>

<#escape x as x?xml>
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
    <fo:layout-master-set>
        <#-- these margins are arbitrary, please redefine as you see fit -->
        <fo:simple-page-master master-name="main-page"
            page-width="17in" page-height="20in"
            margin-top="1in" margin-bottom="1in"
            margin-left="1in" margin-right="1in">
          <fo:region-body margin-top="1in" margin-bottom="0.5in"/>  <#-- main body -->
        </fo:simple-page-master>
  </fo:layout-master-set>

  <fo:page-sequence master-reference="main-page">
       <fo:flow flow-name="xsl-region-body">
       <#assign segments = Static["org.ofbiz.base.util.UtilHttp"].parseMultiFormData(parameters)>
       <#list segments as segment>
         <fo:block break-before="page"> <#-- this tells fop to put a page break before this content TODO: content-type must be dynamic -->
           <fo:external-graphic content-type="content-type:image/gif" width="669px" height="724px" src="<@ofbizUrl>viewShipmentLabel?shipmentId=${segment.shipmentId}&amp;shipmentRouteSegmentId=${segment.shipmentRouteSegmentId}&amp;shipmentPackageSeqId=${segment.shipmentPackageSeqId}<#t/>
             <#lt/>&amp;accessToken=${viewShipAccessToken!}</@ofbizUrl>"></fo:external-graphic><#-- SCIPIO: 2018-10-17: access token -->
         </fo:block>
      </#list>
      <#if segments.size() == 0>
        <fo:block>No Shipping Labels Selected</fo:block>
      </#if>
      </fo:flow>
  </fo:page-sequence>
</fo:root>
</#escape>
