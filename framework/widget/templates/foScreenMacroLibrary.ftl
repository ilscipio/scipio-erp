<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#macro getFoStyle style>
    <#local foStyles = {
        "tabletext":"border-left=\"solid black\" border-right=\"solid black\" padding-left=\"2pt\" padding-top=\"2pt\"",
        "tabletextright":"border-left=\"solid black\" border-right=\"solid black\" padding-left=\"2pt\" padding-top=\"2pt\" text-align=\"right\"",
        "tableheadverysmall":"column-width=\"0.3in\"",
        "tableheadsmall":"column-width=\"0.5in\"",
        "tableheadmedium":"column-width=\"1.5in\"",
        "tableheadwide":"column-width=\"3in\"",
        "head1":"font-size=\"12\" font-weight=\"bold\"",
        "head2":"font-weight=\"bold\"",
        "head3":"font-weight=\"bold\" font-style=\"italic\"",
        "h1":"font-size=\"12\" font-weight=\"bold\"",
        "h2":"font-weight=\"bold\"",
        "h3":"font-weight=\"bold\" font-style=\"italic\"",
        "error":"color=\"red\""}/>
    <#list style?split(' ') as styleItem>
        <#local foStyle = foStyles[styleItem]!""/>
        ${foStyle!""}
    </#list>
</#macro>

<#escape x as x?xml>

<#macro renderScreenBegin extraArgs...>
<?xml version="1.0" encoding="UTF-8"?>
</#macro>

<#macro renderScreenEnd extraArgs...>
</#macro>

<#macro renderSectionBegin boundaryComment extraArgs...>
</#macro>

<#macro renderSectionEnd boundaryComment extraArgs...>
</#macro>
<#macro renderContainerBegin id style autoUpdateLink autoUpdateInterval extraArgs...><fo:block <#if style?has_content><@getFoStyle style/></#if>></#macro>
<#macro renderContainerEnd extraArgs...></fo:block></#macro>
<#macro renderContentBegin editRequest enableEditValue editContainerStyle extraArgs...></#macro>
<#macro renderContentBody extraArgs...></#macro>
<#macro renderContentEnd extraArgs...></#macro>
<#macro renderSubContentBegin extraArgs...></#macro>
<#macro renderSubContentBody extraArgs...></#macro>
<#macro renderSubContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...></#macro>

<#macro renderHorizontalSeparator id style extraArgs...><fo:block><fo:leader leader-length="100%" leader-pattern="rule" rule-style="solid" rule-thickness="0.1mm" color="black"/></fo:block></#macro>
<#macro renderLabel text="" id="" style="" extraArgs...><#if text?has_content><fo:block <#if style?has_content><@getFoStyle style/></#if> <#if id?has_content> id="${id}"</#if>>${text}</fo:block></#if></#macro>
<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name linkUrl text imgStr extraArgs...></#macro>
<#macro renderImage src id style wid hgt border alt urlString extraArgs...><fo:block><fo:external-graphic id="${id}" src="${src}" content-width="${wid}" content-height="${hgt}" scaling="non-uniform"/></fo:block></#macro>

<#macro renderContentFrame extraArgs...></#macro>
<#macro renderScreenletBegin id title collapsible saveCollapsed collapsibleAreaId expandToolTip collapseToolTip fullUrlString padded menuString showMore collapsed javaScriptEnabled menuRole="" titleStyle="" extraArgs...></#macro>
<#macro renderScreenletSubWidget extraArgs...></#macro>
<#macro renderScreenletEnd extraArgs...></#macro>

<#macro renderScreenletPaginateMenu lowIndex actualPageSize ofLabel listSize paginateLastStyle lastLinkUrl paginateLastLabel paginateNextStyle nextLinkUrl paginateNextLabel paginatePreviousStyle paginatePreviousLabel previousLinkUrl paginateFirstStyle paginateFirstLabel firstLinkUrl extraArgs...></#macro>

<#macro renderColumnContainerBegin id style extraArgs...>
  <fo:table table-layout="fixed" width="100%"<#if id?has_content> id="${id}"</#if><#if style?has_content> <@getFoStyle style/></#if>>
    <fo:table-body>
      <fo:table-row>
</#macro>

<#macro renderColumnContainerEnd extraArgs...>
      </fo:table-row>
    </fo:table-body>
  </fo:table>
</#macro>

<#macro renderColumnBegin id style extraArgs...>
        <fo:table-cell<#if id?has_content> id="${id}"</#if><#if style?has_content> <@getFoStyle style/></#if>><fo:block>
</#macro>

<#macro renderColumnEnd extraArgs...>
        </fo:block></fo:table-cell>
</#macro>

</#escape>
