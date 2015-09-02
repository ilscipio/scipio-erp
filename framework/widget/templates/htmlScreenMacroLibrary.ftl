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
<#-- 
Cato: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, etc. however it is only from the initial context,
not "current" context (too intrusive in current renderer design). still relies on macro params.
-->
<#macro renderScreenBegin>
<#if locale??><#local docLangAttr = locale.toString()?replace("_", "-")></#if>
<#local langDir = "ltr">
<#if docLangAttr?? && "ar.iw"?contains(docLangAttr?substring(0, 2))>
    <#local langDir = "rtl">
</#if>
<!DOCTYPE html>
<!--[if IE 9]><html class="lt-ie10" lang="${docLangAttr!"en"}" <#if langDir??>dir="${langDir}"</#if>> <![endif]-->
<html class="no-js" lang="${doctLangAttr!"en"}"<#if langDir?has_content> dir="${langDir!}"</#if>>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</#macro>

<#macro renderScreenEnd>
</#macro>

<#macro renderSectionBegin boundaryComment>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#macro renderSectionEnd boundaryComment>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#macro renderContainerBegin id style autoUpdateLink autoUpdateInterval>
<#if autoUpdateLink?has_content>
<script type="text/javascript">ajaxUpdateAreaPeriodic('${id}', '${autoUpdateLink}', '', '${autoUpdateInterval}');</script>
</#if>
<div <#if style?has_content> class="${style}"</#if> <#if id?has_content>id="${id!}"</#if>>
</#macro>

<#macro renderContainerEnd></div></#macro>

<#macro renderContentBegin editRequest enableEditValue editContainerStyle><#if editRequest?has_content && enableEditValue == "true"><div class=${editContainerStyle}></#if></#macro>

<#macro renderContentBody></#macro>

<#macro renderContentEnd urlString editMode editContainerStyle editRequest enableEditValue>

<#if editRequest?exists && enableEditValue == "true">
<#if urlString?exists><a href="${urlString}">${editMode}</a><#rt/></#if>
<#if editContainerStyle?exists></div><#rt/></#if>
</#if>
</#macro>

<#macro renderSubContentBegin editContainerStyle editRequest enableEditValue><#if editRequest?exists && enableEditValue == "true"><div class="${editContainerStyle}"></#if></#macro>

<#macro renderSubContentBody></#macro>

<#macro renderSubContentEnd urlString editMode editContainerStyle editRequest enableEditValue>
<#if editRequest?exists && enableEditValue == "true">
<#if urlString?exists><a href="${urlString}">${editMode}</a><#rt/></#if>
<#if editContainerStyle?exists></div><#rt/></#if>
</#if>
</#macro>

<#macro renderHorizontalSeparator id style><hr<#if id?has_content> id="${id}"</#if><#if style?has_content> class="${style}"</#if>/></#macro>

<#macro renderLabel text id style>
  <#if text?has_content>
    <#-- If a label widget has one of the h1-h6 styles, then it is considered block level element.
         Otherwise it is considered an inline element. -->
    <#-- Cato: also support "p" style to indicate paragraph, headings and special error classes -->
    <#local idText = ""/>
    <#if id?has_content><#local idText = " id=\"${id}\""/></#if>
    <#if style?has_content>
      <#-- Cato: also support both style and class split by ":" -->
      <#local styleParts = style?split(":")>
      <#if (styleParts?size <= 1)>
        <#local elemType = style?lower_case>
        <#local class = true>
      <#else>
        <#local elemType = styleParts?first?lower_case>
        <#local class = style?substring(elemType?length + 1)>
      </#if>
      <#local res = elemType?matches(r'(heading|h)(\+)?(\d*)')>
      <#if res>
        <#if res?groups[2]?has_content>
          <#if res?groups[3]?has_content>
            <@heading relLevel=res?groups[3]?number class=class id=id>${text}</@heading>
          <#else>
            <@heading class=class id=id>${text}</@heading>
          </#if>
        <#else>
          <#if res?groups[3]?has_content>
            <@heading level=res?groups[3]?number class=class id=id>${text}</@heading>
          <#else>
            <@heading class=class id=id>${text}</@heading>
          </#if>
        </#if>
      <#elseif elemType=="p">
        <p${idText}<#if class?is_string && class?has_content> class="${class}"</#if>>${text}</p>
      <#elseif elemType=="span">
        <span${idText}<#if class?is_string && class?has_content> class="${class}"</#if>>${text}</span>
      <#-- specific permission error class -->
      <#elseif elemType=="perm-error-msg">
        <@errorMsg type="permission" class=class id=id>${text}</@errorMsg>
      <#-- more general security error class -->
      <#elseif elemType=="security-error-msg">
        <@errorMsg type="security" class=class id=id>${text}</@errorMsg>
      <#-- general error class -->
      <#elseif elemType=="error-msg">
        <@errorMsg type="error" class=class id=id>${text}</@errorMsg>
      <#-- result message class: sometimes messages like "no product found" are not an error but expected possible result -->
      <#elseif elemType=="result-msg">
        <@resultMsg class=class id=id>${text}</@resultMsg>
      <#elseif elemType=="message">
        <@alert type="info" class=class id=id>${text}</@alert>
      <#else>
        <span${idText} class="${style}">${text}</span>
      </#if>
    <#else>
      <span${idText}>${text}</span>
    </#if>
  </#if>
</#macro>

<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name height width linkUrl text imgStr>
    <#if "ajax-window" != linkType>
        <#if "hidden-form" == linkType>
            <form method="post" action="${actionUrl}" <#if targetWindow?has_content>target="${targetWindow}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${uniqueItemName}"><#rt/>
                <#list parameterList as parameter>
                <input name="${parameter.name}" value="${parameter.value}" type="hidden"/><#rt/>
                </#list>
            </form><#rt/>
        </#if>
        <a 
            <#if id?has_content>id="${id}"</#if> 
            <#if style?has_content>class="${style}"</#if> 
            <#if name?has_content>name="${name}"</#if> 
            <#if targetWindow?has_content>target="${targetWindow}"</#if> 
            href="<#if "hidden-form"==linkType>javascript:document.${uniqueItemName}.submit()<#else>${linkUrl}</#if>"><#rt/>
            <#if imgStr?has_content>${imgStr}</#if><#if text?has_content>${text}</#if>
        </a>
    <#else>
        <div id="${uniqueItemName}"></div>
        <a href="javascript:void(0);" id="${uniqueItemName}_link" 
        <#if style?has_content>class="${style}"</#if>>
        <#if text?has_content>${text}</#if></a>
        <script type="text/javascript">
            function getRequestData () {
                var data =  {
                    <#list parameterList as parameter>
                        "${parameter.name}": "${parameter.value}",
                    </#list>
                    "presentation": "layer"
                };
        
                return data;
            }
            jQuery("#${uniqueItemName}_link").click( function () {
                jQuery("#${uniqueItemName}").dialog("open");
            });
            jQuery("#${uniqueItemName}").dialog({
                 autoOpen: false,
                 <#if text?has_content>title: "${text}",</#if>
                 height: ${height},
                 width: ${width},
                 modal: true,
                 open: function() {
                         jQuery.ajax({
                             url: "${target}",
                             type: "POST",
                             data: getRequestData(),
                             success: function(data) {jQuery("#${uniqueItemName}").html(data);}
                         });
                 }
            });
        </script>
    </#if>
</#macro>

<#macro renderImage src id style wid hgt border alt urlString>
<#if src?has_content>
<img <#if id?has_content>id="${id}"</#if><#if style?has_content> class="${style}"</#if><#if wid?has_content> width="${wid}"</#if><#if hgt?has_content> height="${hgt}"</#if><#if border?has_content> border="${border}"</#if> alt="<#if alt?has_content>${alt}</#if>" src="${urlString}" />
</#if>
</#macro>

<#macro renderContentFrame fullUrl width height border><iframe src="${fullUrl}" width="${width}" height="${height}" <#if border?has_content>border="${border}"</#if> /></#macro>

<#-- Cato:
     fromWidgets: hint of whether called by renderer or ftl macros
     hasContent: hint to say there will be content, workaround for styling -->
<#macro renderScreenletBegin id="" title="" classes="" collapsible=false saveCollapsed=true collapsibleAreaId="" expandToolTip=true collapseToolTip=true fullUrlString="" padded=false menuString="" showMore=true collapsed=false javaScriptEnabled=true fromWidgets=true menuClass="" menuId="" menuRole="" requireMenu=false forceEmptyMenu=false hasContent=true titleStyle="" titleContainerStyle="" autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel=2>

<#-- level logic begin -->
    <#-- note: request obj only available because of macro renderer initial context mod -->
    <#local sLevel = getCurrentSectionLevel()>
    <#local prevSectionLevel = sLevel>
    <#local dummy = setCurrentSectionLevel(sLevel+1)>
<#-- level logic end -->

<#-- title-style parsing begin -->
    <#-- titleContainerStyle can be inlined as prefix in titleStyle, separated by ;, as workaround 
         full string can look like:
           e.g. titleStyle="div:containerClass;h4:titleClass", titleStyle="div;h5"-->
    <#local titleStyleParts = titleStyle?split(";")>
    <#if (titleStyleParts?size > 1)>
      <#local titleContainerStyle = titleStyleParts?first>
      <#local titleStyle = titleStyleParts?last>
    </#if>
    
    <#local titleContainerElemType = "">
    <#local titleContainerClass = "">
    <#if titleContainerStyle?has_content>
      <#local titleContainerStyleLower = titleContainerStyle?lower_case>
      <#if titleContainerStyleLower?starts_with("div:")>
        <#local titleContainerClass = titleContainerStyle?substring("div:"?length)>
      <#elseif titleContainerStyleLower == "div">
        <#local titleContainerClass = "">
      <#else>
        <#local titleContainerClass = titleContainerStyle>
      </#if>
    </#if>
    
    <#local titleElemType = "">
    <#local titleClass = true>
    <#if titleStyle?has_content>
      <#local titleStyleParts = titleStyle?split(":")>
      <#if (titleStyleParts?size <= 1)>
        <#-- here titleStyle is either an elem or class, can't tell yet -->
        <#local titleElemType = titleStyle?lower_case>
        <#local titleClass = titleStyle>
      <#else>
        <#local titleElemType = titleStyleParts?first?lower_case>
        <#local titleClass = titleStyle?substring(titleElemType?length + 1)>
      </#if>
    
      <#local res = titleElemType?matches(r'(heading|h)(\+)?(\d*)')>
      <#if res>
        <#if res?groups[2]?has_content>
          <#if res?groups[3]?has_content>
            <#local relHeadingLevel = res?groups[3]?number>
          </#if>
        <#else>
          <#if res?groups[3]?has_content>
            <#-- overrides headingLevel (so style from screen affects heading calc) -->
            <#local headingLevel = res?groups[3]?number>
          </#if>
        </#if>
        <#if (titleStyleParts?size <= 1)>
          <#local titleClass = true>
        </#if>
        <#local titleElemType = "">
      <#elseif ['div','span','p']?seq_contains(titleElemType)>
        <#if (titleStyleParts?size <= 1)>
          <#local titleClass = true>
        </#if>
      <#else>
        <#local titleElemType = "">
        <#-- if invalid type found, use the full string as class, in case ":" char is important somehow -->
        <#local titleClass = titleStyle>
      </#if>
    </#if>
<#-- title-style parsing end -->

<#-- auto-heading-level logic begin -->
    <#local explicitHeadingLevel = false>
    <#local updatedHeadingLevel = false> <#-- just so consistent -->
    <#local prevHeadingLevel = "">
    <#if autoHeadingLevel>
        <#local prevHeadingLevel = getCurrentHeadingLevel(false)>
        <#if headingLevel?has_content>
            <#local hLevel = headingLevel>
            <#local explicitHeadingLevel = true>
        <#elseif prevHeadingLevel?has_content>
            <#local hLevel = prevHeadingLevel>
        <#else>
            <#local hLevel = defaultHeadingLevel>
        </#if>
        <#if relHeadingLevel?has_content>
          <#local hLevel = hLevel + relHeadingLevel>
        </#if>
        <#if title?has_content>
            <#local dummy = setCurrentHeadingLevel(hLevel + 1)>
            <#local updatedHeadingLevel = true>
        <#elseif explicitHeadingLevel>
            <#-- set here but don't increase if none title -->
            <#local dummy = setCurrentHeadingLevel(hLevel)>
            <#local updatedHeadingLevel = true>
        </#if>
    <#else>
        <#if headingLevel?has_content>
            <#local hLevel = headingLevel>
            <#local explicitHeadingLevel = true>
        <#else>
            <#local hLevel = defaultHeadingLevel>
        </#if>
        <#if relHeadingLevel?has_content>
          <#local hLevel = hLevel + relHeadingLevel>
        </#if>
    </#if>
    <#global renderScreenletValuesStack = pushStack(renderScreenletValuesStack!, {"autoHeadingLevel":autoHeadingLevel, "updatedHeadingLevel":updatedHeadingLevel, "prevHeadingLevel":prevHeadingLevel, "prevSectionLevel":prevSectionLevel})>
<#-- auto-heading-level logic end -->

<#-- Cato: menuString is not wrapped in UL when it's received here from macro renderer... 
     note: with recent patch, menuString passed by renderer is rendered by macro renderer. -->
<#local menuString = menuString?trim>
<#local hasMenu = (menuString?has_content || requireMenu || forceEmptyMenu)>
<#local contentFlagClasses> section-level-${sLevel} heading-level-${hLevel}<#if title?has_content> has-title<#else> no-title</#if><#if hasMenu> has-menu<#else> no-menu</#if><#if hasContent> has-content<#else> no-content</#if></#local>
<div class="section-screenlet${contentFlagClasses}<#if collapsed> toggleField</#if>">
<#if collapsed><p class="alert legend">[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] ${title!}</p></#if>
<div class="${styles.grid_row!}"<#if id?has_content> id="${id}"</#if>><#rt/>
<div class="<#if classes?has_content>${classes}<#else>${styles.grid_large!}12</#if> ${styles.grid_cell!} section-screenlet-container${contentFlagClasses}">

<#if showMore>
<#if title?has_content>
  <#if titleContainerStyle?has_content>
    <#if titleContainerElemType?has_content>
      <#local tcElem = titleContainerElemType>
    <#else>
      <#local tcElem = "div">
    </#if>
    <${tcElem} class="heading-level-${hLevel}<#if titleContainerClass?has_content> ${titleContainerClass}</#if>">
  </#if>
  
  <#if titleElemType?has_content>
    <#local tElem = titleElemType>
  <#else>
    <#-- standard case. -->
    <#if (hLevel < 1)>
      <#local tElem = "h1">
    <#elseif (hLevel > 6)>
      <#local tElem = "h6">
      <#--<#local tElem = "div">-->
    <#else>
      <#local tElem = "h${hLevel}">
    </#if>
  </#if>
  <#local titleClasses = makeClassesArg(titleClass, "")>
      <${tElem} class="heading-level-${hLevel}<#if titleClasses?has_content> ${titleClasses}</#if>">${title}</${tElem}>
  
  <#if titleContainerStyle?has_content>
    </${tcElem}>
  </#if>
</#if>    
    
    <#--
<#if collapsible>
<li class="<#rt/>
<#if collapsed>
collapsed"><a <#if javaScriptEnabled>onclick="javascript:toggleScreenlet(this, '${collapsibleAreaId}', '${saveCollapsed?string}', '${expandToolTip}', '${collapseToolTip}');"<#else>href="${fullUrlString}"</#if><#if expandToolTip?has_content> title="${expandToolTip}"</#if>
<#else>
expanded"><a <#if javaScriptEnabled>onclick="javascript:toggleScreenlet(this, '${collapsibleAreaId}', '${saveCollapsed?string}', '${expandToolTip}', '${collapseToolTip}');"<#else>href="${fullUrlString}"</#if><#if collapseToolTip?has_content> title="${collapseToolTip}"</#if>
</#if>
>&nbsp;</a></li>
</#if>
 -->
 
<#if hasMenu>
  <#-- temporarily (?) unnecessary; all use styles.button_group and hacks moved
  <#local screenletPaginateMenu = (menuRole == "paginate-menu") && widgetRender>
  <#local screenletNavMenu = (menuRole == "nav-menu") && widgetRender>
  <#local ftlNavMenu = (menuRole == "nav-menu") && !widgetRender>
  -->
  
  <#if !menuClass?has_content>
    <#local menuClass = "${styles.menu_section!}"> <#-- ${styles.button_force!} -->
  <#elseif menuClass == "none">
    <#local menuClass = "">
  </#if>

  <#-- note: menuString shouldn't contain <ul because li may be added here, but check anyway -->
  <#if !menuString?starts_with("<ul")><ul<#if menuId?has_content> id="${menuId}"<#elseif id?has_content> id="${id}_menu"</#if><#if menuClass?has_content> class="${menuClass}"</#if>></#if>
  <#if !forceEmptyMenu>
    ${menuString}
  </#if>
  <#if !menuString?ends_with("</ul>")></ul></#if>
</#if>

</#if>
    <#-- note: may need to keep this div free of foundation grid classes -->
    <div<#if collapsibleAreaId?has_content> id="${collapsibleAreaId}"</#if> class="section-screenlet-content${contentFlagClasses}"><#rt>
</#macro>

<#macro renderScreenletSubWidget></#macro>

<#macro renderScreenletEnd>
<#-- auto-heading-level logic begin -->
    <#local stackValues = readStack(renderScreenletValuesStack!)>
    <#global renderScreenletValuesStack = popStack(renderScreenletValuesStack!)>
    
    <#local autoHeadingLevel = stackValues.autoHeadingLevel>
    <#local updatedHeadingLevel = stackValues.updatedHeadingLevel>
    <#local prevHeadingLevel = stackValues.prevHeadingLevel>
    
<#-- level logic begin -->
    <#local sLevel = stackValues.prevSectionLevel>
    <#local dummy = setCurrentSectionLevel(sLevel)>
<#-- level logic end -->

    <#if autoHeadingLevel && updatedHeadingLevel>
        <#local dummy = setCurrentHeadingLevel(prevHeadingLevel)>
    </#if>
    
<#-- auto-heading-level logic end -->
    <#lt></div></div></div></div>
</#macro>

<#macro renderScreenletPaginateMenu lowIndex actualPageSize ofLabel listSize paginateLastStyle lastLinkUrl paginateLastLabel paginateNextStyle nextLinkUrl paginateNextLabel paginatePreviousStyle paginatePreviousLabel previousLinkUrl paginateFirstStyle paginateFirstLabel firstLinkUrl>
    <li class="${paginateFirstStyle?default("nav-first")}<#if !firstLinkUrl?has_content> disabled</#if>"><#if firstLinkUrl?has_content><a href="${firstLinkUrl}" class="${styles.menu_section_itemlink!}">${paginateFirstLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_itemlink!}">${paginateFirstLabel}</a></#if></li>
    <li class="${paginatePreviousStyle?default("nav-previous")}<#if !previousLinkUrl?has_content> disabled</#if>"><#if previousLinkUrl?has_content><a href="${previousLinkUrl}" class="${styles.menu_section_itemlink!}">${paginatePreviousLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_itemlink!}">${paginatePreviousLabel}</a></#if></li>
    <#if (listSize?number > 0)><li><span class="text-entry">${lowIndex?number + 1} - ${lowIndex?number + actualPageSize?number} ${ofLabel} ${listSize}</span></li><#rt/></#if>
    <li class="${paginateNextStyle}<#if !nextLinkUrl?has_content> disabled</#if>"><#if nextLinkUrl?has_content><a href="${nextLinkUrl}" class="${styles.menu_section_itemlink!}">${paginateNextLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_itemlink!}">${paginateNextLabel}</a></#if></li>
    <li class="${paginateLastStyle}<#if !lastLinkUrl?has_content> disabled</#if>"><#if lastLinkUrl?has_content><a href="${lastLinkUrl}" class="${styles.menu_section_itemlink!}">${paginateLastLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_itemlink!}">${paginateLastLabel}</a></#if></li>
</#macro>

<#macro renderPortalPageBegin originalPortalPageId portalPageId confMode="false" addColumnLabel="Add column" addColumnHint="Add a new column to this portal" columnCount=1>
  <#global portalPageGridUsed = 0>
  <#--
  <#if confMode == "true">
    <a class="${styles.button_default!}" href="javascript:document.addColumn_${portalPageId}.submit()" title="${addColumnHint}">${addColumnLabel}</a> <b>PortalPageId: ${portalPageId}</b>
    <form method="post" action="addPortalPageColumn" name="addColumn_${portalPageId}">
      <input name="portalPageId" value="${portalPageId}" type="hidden"/>
    </form>
  </#if>
  -->
  <div class="${styles.grid_row!}">
    <div class="${styles.grid_large!}12 ${styles.grid_cell!}">  
      <div class="${styles.grid_row!}">
        <#-- for now, make each column a huge cell 
             (can't use grid any other way without rewriting portal page render order?) -->
</#macro>

<#macro renderPortalPageEnd>
      </div>
    </div>
  </div>
</#macro>

<#function getPortalPageWidthPercent widthHint>
  <#if widthHint?has_content>
    <#local res = widthHint?matches(r"^((\d|\d\d|100)(\.\d+)?)%$")>
    <#if res>
        <#local widthPercent = res?groups[1]?number>
        <#return widthPercent>
    </#if>
  </#if>
  <#return "">  
</#function>

<#-- returns portal column size in grid units (grid column size), closest int value possible 
     note: is possible not all of gridSize is used depend on fractions and ints.
     let user worry about it (and caller can use foundation 'end'). -->
<#function calcPortalPageColumnGridSize columnCount columnIndex gridSize gridUsed widthHint>
  <#local columnRemain = (columnCount - columnIndex)>
  <#local gridRemain = (gridSize - gridUsed)>
  <#-- ofbiz portal column width is specified in pixels or %
       if %, create a rounding to nearest grid int value
       ignore pixels since we can't use it, treat it and everything else as auto -->
  <#local widthPercent = getPortalPageWidthPercent(widthHint)>
  <#if widthPercent?has_content>
    <#local columnSize = ((widthPercent * gridSize) / 100)?round>
    <#if (columnSize <= gridRemain)>
      <#return columnSize>
    <#else>
      <#return gridRemain>
    </#if>
  <#else>
    <#-- here, floor so will never go over grid -->
    <#local columnSize = (gridRemain / columnRemain)?floor>
    <#return columnSize>
  </#if>
</#function>


<#macro renderPortalPageColumnBegin originalPortalPageId portalPageId columnSeqId confMode="false" width="auto" delColumnLabel="Delete column" delColumnHint="Delete this column" addPortletLabel="Add portlet" addPortletHint="Add a new portlet to this column" colWidthLabel="Col. width:" setColumnSizeHint="Set column size" columnCount=1 columnIndex=0>
  <#local gridSize = 12>
  <#local firstColumn = (columnIndex <= 0)>
  <#local lastColumn = (columnIndex >= (columnCount - 1))>
  
  <#-- Cato: calc and adapt width to possible grid widths + record grid used so far in global var -->
  <#local columnSize = calcPortalPageColumnGridSize(columnCount, columnIndex, gridSize, portalPageGridUsed, width)>
  <#global portalPageGridUsed = portalPageGridUsed + columnSize>
  
  <#local columnKey = portalPageId+columnSeqId>
  <#--
  <#local columnKeyFields = '<input name="portalPageId" value="' + portalPageId + '" type="hidden"/><input name="columnSeqId" value="' + columnSeqId + '" type="hidden"/>'>
  <script type="text/javascript">
    if (typeof SORTABLE_COLUMN_LIST != "undefined") {
      if (SORTABLE_COLUMN_LIST == null) {
        SORTABLE_COLUMN_LIST = "#portalColumn_${columnSeqId}";
      } else {
        SORTABLE_COLUMN_LIST += ", #portalColumn_${columnSeqId}";
      }
    }
  </script>
  <td class="portal-column<#if confMode == "true">-config</#if> connectedSortable" style="vertical-align: top; <#if width?has_content> width:${width};</#if>" id="portalColumn_${columnSeqId}">
  -->
    
    <#-- if it's the last column and didn't fill full gridSize, add 'end' class so doesn't float right and look weird -->
    <#local endClassStr = "">
    <#if lastColumn && (portalPageGridUsed < gridSize)>
        <#local endClassStr = " ${styles.grid_end!}">
    </#if>
    
    <#-- Column: columnCount: ${columnCount}, columnIndex: ${columnIndex}, portalPageGridUsed: ${portalPageGridUsed}, width: ${width} --> 
    <div class="${styles.grid_large!}${columnSize} ${styles.grid_cell!}${endClassStr}">
    <#if confMode == "true">
      <div class="portal-column-config-title-bar">
        <ul>
          <li>
            <form method="post" action="deletePortalPageColumn" name="delColumn_${columnKey}">
              ${columnKeyFields}
            </form>
            <a class="${styles.button_default!}" href="javascript:document.delColumn_${columnKey}.submit()" title="${delColumnHint}">${delColumnLabel}</a>
          </li>
          <li>
            <form method="post" action="addPortlet" name="addPortlet_${columnKey}">
              ${columnKeyFields}
            </form>
            <a class="${styles.button_default!}" href="javascript:document.addPortlet_${columnKey}.submit()" title="${addPortletHint}">${addPortletLabel}</a>
          </li>
          <li>
            <form method="post" action="editPortalPageColumnWidth" name="setColumnSize_${columnKey}">
              ${columnKeyFields}
            </form>
            <a class="${styles.button_default!}" href="javascript:document.setColumnSize_${columnKey}.submit()" title="${setColumnSizeHint}">${colWidthLabel}: ${width}</a>
          </li>
        </ul>
      </div>
      <br class="clear"/>
    </#if>
</#macro>

<#macro renderPortalPageColumnEnd>
    </div>
</#macro>

<#macro renderPortalPagePortletBegin originalPortalPageId portalPageId portalPortletId portletSeqId prevPortletId="" prevPortletSeqId="" nextPortletId="" nextPortletSeqId="" columnSeqId="" prevColumnSeqId="" nextColumnSeqId="" confMode="false" delPortletHint="Remove this portlet" editAttribute="false" editAttributeHint="Edit portlet parameters" width="auto" columnCount=1 columnIndex=0>
  <#local portletKey = portalPageId+portalPortletId+portletSeqId>
  <#local portletKeyFields = '<input name="portalPageId" value="' + portalPageId + '" type="hidden"/><input name="portalPortletId" value="' + portalPortletId + '" type="hidden"/><input name="portletSeqId" value="' + portletSeqId  + '" type="hidden"/>'>
  
  <div id="PP_${portletKey}" name="portalPortlet" class="noClass" portalPageId="${portalPageId}" portalPortletId="${portalPortletId}" columnSeqId="${columnSeqId}" portletSeqId="${portletSeqId}">
    <#if confMode == "true">
      <div class="portlet-config" id="PPCFG_${portletKey}">
        <div class="portlet-config-title-bar">
          <ul>
            <li class="title">Portlet : [${portalPortletId}]</li>
            <li class="remove">
              <form method="post" action="deletePortalPagePortlet" name="delPortlet_${portletKey}">
                ${portletKeyFields}
              </form>
              <a href="javascript:document.delPortlet_${portletKey}.submit()" title="${delPortletHint}">&nbsp;&nbsp;&nbsp;</a>
            </li>
            <#if editAttribute == "true">
              <li class="edit">
                <form method="post" action="editPortalPortletAttributes" name="editPortlet_${portletKey}">
                  ${portletKeyFields}
                </form>
                <a href="javascript:document.editPortlet_${portletKey}.submit()" title="${editAttributeHint}">&nbsp;&nbsp;&nbsp;</a>
              </li>
            </#if>
            <#if prevColumnSeqId?has_content>
              <li class="move-left">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletLeft_${portletKey}">
                  <input name="o_portalPageId" value="${portalPageId}" type="hidden"/>
                  <input name="o_portalPortletId" value="${portalPortletId}" type="hidden"/>
                  <input name="o_portletSeqId" value="${portletSeqId}" type="hidden"/>
                  <input name="destinationColumn" value="${prevColumnSeqId}" type="hidden"/>
                  <input name="mode" value="DRAGDROPBOTTOM" type="hidden"/>
                </form>
                <a href="javascript:document.movePortletLeft_${portletKey}.submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
            <#if nextColumnSeqId?has_content>
              <li class="move-right">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletRight_${portletKey}">
                  <input name="o_portalPageId" value="${portalPageId}" type="hidden"/>
                  <input name="o_portalPortletId" value="${portalPortletId}" type="hidden"/>
                  <input name="o_portletSeqId" value="${portletSeqId}" type="hidden"/>
                  <input name="destinationColumn" value="${nextColumnSeqId}" type="hidden"/>
                  <input name="mode" value="DRAGDROPBOTTOM" type="hidden"/>
                </form>
                <a href="javascript:document.movePortletRight_${portletKey}.submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
            <#if prevPortletId?has_content>
              <li class="move-up">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletUp_${portletKey}">
                  <input name="o_portalPageId" value="${portalPageId}" type="hidden"/>
                  <input name="o_portalPortletId" value="${portalPortletId}" type="hidden"/>
                  <input name="o_portletSeqId" value="${portletSeqId}" type="hidden"/>
                  <input name="d_portalPageId" value="${portalPageId}" type="hidden"/>
                  <input name="d_portalPortletId" value="${prevPortletId}" type="hidden"/>
                  <input name="d_portletSeqId" value="${prevPortletSeqId}" type="hidden"/>
                  <input name="mode" value="DRAGDROPBEFORE" type="hidden"/>
                </form>
                <a href="javascript:document.movePortletUp_${portletKey}.submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
            <#if nextPortletId?has_content>
              <li class="move-down">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletDown_${portletKey}">
                  <input name="o_portalPageId" value="${portalPageId}" type="hidden"/>
                  <input name="o_portalPortletId" value="${portalPortletId}" type="hidden"/>
                  <input name="o_portletSeqId" value="${portletSeqId}" type="hidden"/>
                  <input name="d_portalPageId" value="${portalPageId}" type="hidden"/>
                  <input name="d_portalPortletId" value="${nextPortletId}" type="hidden"/>
                  <input name="d_portletSeqId" value="${nextPortletSeqId}" type="hidden"/>
                  <input name="mode" value="DRAGDROPAFTER" type="hidden"/>
                </form>
                <a href="javascript:document.movePortletDown_${portletKey}.submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
          </ul>
          <br class="clear"/>
        </div>
      </#if>
</#macro>

<#macro renderPortalPagePortletEnd confMode="false">
  </div>
  <#if confMode == "true">
    </div>
  </#if>
</#macro>

<#macro renderColumnContainerBegin id style>
  <table cellspacing="0"<#if id?has_content> id="${id}"</#if><#if style?has_content> class="${style}"</#if>>
  <tr>
</#macro>

<#macro renderColumnContainerEnd>
  </tr>
  </table>
</#macro>

<#macro renderColumnBegin id style>
  <td<#if id?has_content> id="${id}"</#if><#if style?has_content> class="${style}"</#if>>
</#macro>

<#macro renderColumnEnd>
  </td>
</#macro>

<#-- TODO: Macro for pagination -->
