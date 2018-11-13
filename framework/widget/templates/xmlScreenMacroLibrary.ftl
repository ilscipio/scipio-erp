<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#macro renderScreenBegin extraArgs...>
<?xml version="1.0" encoding="UTF-8"?>
<export>
</#macro>

<#macro renderScreenEnd extraArgs...>
</export>
</#macro>

<#macro renderSectionBegin boundaryComment extraArgs...>
</#macro>

<#macro renderSectionEnd boundaryComment extraArgs...>
</#macro>

<#macro renderContainerBegin id style autoUpdateLink autoUpdateInterval extraArgs...>
</#macro>
<#macro renderContainerEnd extraArgs...></#macro>
<#macro renderContentBegin editRequest enableEditValue editContainerStyle extraArgs...></#macro>
<#macro renderContentBody extraArgs...></#macro>
<#macro renderContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...>
</#macro>
<#macro renderSubContentBegin editContainerStyle editRequest enableEditValue extraArgs...></#macro>
<#macro renderSubContentBody extraArgs...></#macro>
<#macro renderSubContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...>
</#macro>

<#macro renderHorizontalSeparator id style extraArgs...></#macro>
<#macro renderLabel text id style extraArgs...></#macro>
<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name linkUrl text imgStr extraArgs...>
</#macro>
<#macro renderImage src id style wid hgt border alt urlString extraArgs...>
</#macro>

<#macro renderContentFrame fullUrl width height border extraArgs...></#macro>
<#macro renderScreenletBegin id title collapsible saveCollapsed collapsibleAreaId expandToolTip collapseToolTip fullUrlString padded menuString showMore collapsed javaScriptEnabled menuRole="" titleStyle="" extraArgs...>
</#macro>
<#macro renderScreenletSubWidget extraArgs...></#macro>
<#macro renderScreenletEnd extraArgs...></#macro>
<#macro renderScreenletPaginateMenu lowIndex actualPageSize ofLabel listSize paginateLastStyle lastLinkUrl paginateLastLabel paginateNextStyle nextLinkUrl paginateNextLabel paginatePreviousStyle paginatePreviousLabel previousLinkUrl paginateFirstStyle paginateFirstLabel firstLinkUrl extraArgs...>
</#macro>