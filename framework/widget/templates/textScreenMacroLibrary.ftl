<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

<#macro renderScreenBegin extraArgs...>
</#macro>

<#macro renderScreenEnd extraArgs...>
</#macro>

<#macro renderSectionBegin boundaryComment extraArgs...>
</#macro>

<#macro renderSectionEnd boundaryComment extraArgs...>
</#macro>

<#macro renderContainerBegin id style autoUpdateLink autoUpdateInterval extraArgs...></#macro>
<#macro renderContainerEnd extraArgs...></#macro>
<#macro renderContentBegin editRequest enableEditValue editContainerStyle extraArgs...></#macro>
<#macro renderContentBody extraArgs...></#macro>
<#macro renderContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...></#macro>
<#macro renderSubContentBegin editContainerStyle editRequest enableEditValue extraArgs...></#macro>
<#macro renderSubContentBody extraArgs...></#macro>
<#macro renderSubContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...></#macro>

<#macro renderHorizontalSeparator id style extraArgs...></#macro>
<#macro renderLabel text="" id="" style="" extraArgs...>
    <#if text??>
        ${text}<#lt/>
    </#if>
</#macro>
<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name linkUrl text imgStr extraArgs...></#macro>
<#macro renderImage src id style wid hgt border alt urlString extraArgs...></#macro>

<#macro renderContentFrame fullUrl width height border extraArgs...></#macro>
<#macro renderScreenletBegin id title collapsible saveCollapsed collapsibleAreaId expandToolTip collapseToolTip fullUrlString padded menuString showMore collapsed javaScriptEnabled menuRole="" titleStyle="" extraArgs...></#macro>
<#macro renderScreenletSubWidget extraArgs...></#macro>
<#macro renderScreenletEnd extraArgs...></#macro>

<#macro renderScreenletPaginateMenu lowIndex actualPageSize ofLabel listSize paginateLastStyle lastLinkUrl paginateLastLabel paginateNextStyle nextLinkUrl paginateNextLabel paginatePreviousStyle paginatePreviousLabel previousLinkUrl paginateFirstStyle paginateFirstLabel firstLinkUrl extraArgs...></#macro>

<#macro renderColumnContainerBegin id style extraArgs...></#macro>
<#macro renderColumnContainerEnd extraArgs...></#macro>
<#macro renderColumnBegin id style extraArgs...></#macro>
<#macro renderColumnEnd extraArgs...></#macro>
