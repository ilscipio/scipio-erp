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
<#include "htmlCommonMacroLibrary.ftl">
<#-- 
Cato: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, locale, and to some extent (since 2016-01-06), uiLabelMap.
WARN: no code run here or indirectly from here should assume full current context present. only use well-known generic vars.
-->
<#macro renderScreenBegin>
<#-- Cato: NOTE: HTML head open is now in cato template macros. 
     In OOTB ofbiz no context is passed here (locale, etc.) so did not belong here and cleaner if in cato macros. -->
<!DOCTYPE html>
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
    <@script>ajaxUpdateAreaPeriodic('${id}', '${autoUpdateLink}', '', '${autoUpdateInterval}');</@script>
  </#if>
  <#-- Cato: delegate to cato libs -->
  <@container open=true close=false class=style id=id />
</#macro>

<#macro renderContainerEnd>
  <@container close=true open=false />
</#macro>

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
  <@renderLabelCommon text=text id=id style=style />
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
        <@script>
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
        </@script>
    </#if>
</#macro>

<#macro renderImage src id style wid hgt border alt urlString>
<#if src?has_content>
<img <#if id?has_content>id="${id}"</#if><#if style?has_content> class="${style}"</#if><#if wid?has_content> width="${wid}"</#if><#if hgt?has_content> height="${hgt}"</#if><#if border?has_content> border="${border}"</#if> alt="<#if alt?has_content>${alt}</#if>" src="${urlString}" />
</#if>
</#macro>

<#macro renderContentFrame fullUrl width height border><iframe src="${fullUrl}" width="${width}" height="${height}" <#if border?has_content>border="${border}"</#if> /></#macro>

<#-- Cato: new params: menuRole, titleStyle -->
<#macro renderScreenletBegin id="" title="" collapsible=false saveCollapsed=true collapsibleAreaId="" expandToolTip=true collapseToolTip=true fullUrlString="" padded=false menuString="" showMore=true collapsed=false javaScriptEnabled=true menuRole="" titleStyle="">
    <#-- now delegates to Cato implementation. TODO? this call is still too closely based on this macro and its args; rework later -->
    <@section_core open=true close=false id=id title=title collapsible=collapsible saveCollapsed=saveCollapsed collapsibleAreaId=collapsibleAreaId expandToolTip=expandToolTip collapseToolTip=collapseToolTip fullUrlString=fullUrlString padded=padded menuContent=menuString 
        showMore=showMore collapsed=collapsed javaScriptEnabled=javaScriptEnabled fromScreenDef=true menuClass="" menuId="" menuLayout="" menuRole=menuRole requireMenu=false forceEmptyMenu=false hasContent=true titleStyle=titleStyle titleContainerStyle="" titleConsumeLevel=true 
        autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel="" />
</#macro>

<#macro renderScreenletSubWidget></#macro>

<#macro renderScreenletEnd>
    <@section_core close=true open=false />
</#macro>

<#macro renderScreenletPaginateMenu lowIndex actualPageSize ofLabel listSize paginateLastStyle lastLinkUrl paginateLastLabel paginateNextStyle nextLinkUrl paginateNextLabel paginatePreviousStyle paginatePreviousLabel previousLinkUrl paginateFirstStyle paginateFirstLabel firstLinkUrl>
    <li class="${paginateFirstStyle?default("nav-first")}<#if !firstLinkUrl?has_content> disabled</#if>"><#if firstLinkUrl?has_content><a href="${firstLinkUrl}" class="${styles.menu_section_item_link!}">${paginateFirstLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${paginateFirstLabel}</a></#if></li>
    <li class="${paginatePreviousStyle?default("nav-previous")}<#if !previousLinkUrl?has_content> disabled</#if>"><#if previousLinkUrl?has_content><a href="${previousLinkUrl}" class="${styles.menu_section_item_link!}">${paginatePreviousLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${paginatePreviousLabel}</a></#if></li>
    <#if (listSize?number > 0)><li><span class="text-entry">${lowIndex?number + 1} - ${lowIndex?number + actualPageSize?number} ${ofLabel} ${listSize}</span></li><#rt/></#if>
    <li class="${paginateNextStyle}<#if !nextLinkUrl?has_content> disabled</#if>"><#if nextLinkUrl?has_content><a href="${nextLinkUrl}" class="${styles.menu_section_item_link!}">${paginateNextLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${paginateNextLabel}</a></#if></li>
    <li class="${paginateLastStyle}<#if !lastLinkUrl?has_content> disabled</#if>"><#if lastLinkUrl?has_content><a href="${lastLinkUrl}" class="${styles.menu_section_item_link!}">${paginateLastLabel}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${paginateLastLabel}</a></#if></li>
</#macro>

<#macro renderPortalPageBegin originalPortalPageId portalPageId confMode="false" addColumnLabel="Add column" addColumnHint="Add a new column to this portal" columnCount=1>
  <#global portalPageGridUsed = 0>
  <#--
  <#if confMode == "true">
    <a class="${styles.link_action!}" href="javascript:document.addColumn_${portalPageId}.submit()" title="${addColumnHint}">${addColumnLabel}</a> <b>PortalPageId: ${portalPageId}</b>
    <form method="post" action="addPortalPageColumn" name="addColumn_${portalPageId}">
      <input name="portalPageId" value="${portalPageId}" type="hidden"/>
    </form>
  </#if>
  -->
  <@row open=true close=false />
    <@cell open=true close=false /> 
      <@row open=true close=false />
        <#-- for now, make each column a huge cell 
             (can't use grid any other way without rewriting portal page render order?) -->
</#macro>

<#macro renderPortalPageEnd>
      <@row close=true open=false />
    <@cell close=true open=false />
  <@row close=true open=false />
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
  <@script>
    if (typeof SORTABLE_COLUMN_LIST != "undefined") {
      if (SORTABLE_COLUMN_LIST == null) {
        SORTABLE_COLUMN_LIST = "#portalColumn_${columnSeqId}";
      } else {
        SORTABLE_COLUMN_LIST += ", #portalColumn_${columnSeqId}";
      }
    }
  </@script>
  <td class="portal-column<#if confMode == "true">-config</#if> connectedSortable" style="vertical-align: top; <#if width?has_content> width:${width};</#if>" id="portalColumn_${columnSeqId}">
  -->
    
    <#-- if it's the last column and didn't fill full gridSize, add 'end' class so doesn't float right and look weird -->
    <#local endClassStr = "">
    <#if lastColumn && (portalPageGridUsed < gridSize)>
        <#local endClassStr = " ${styles.grid_end!}">
    </#if>
    
    <#-- Column: columnCount: ${columnCount}, columnIndex: ${columnIndex}, portalPageGridUsed: ${portalPageGridUsed}, width: ${width} --> 
    <#local portalPageClasses = "${styles.grid_large!}${columnSize}${endClassStr}">
    <@cell open=true close=false class=portalPageClasses />
    <#if confMode == "true">
      <div class="portal-column-config-title-bar">
        <ul>
          <li>
            <form method="post" action="deletePortalPageColumn" name="delColumn_${columnKey}">
              ${columnKeyFields!}
            </form>
            <a class="${styles.link_action!}" href="javascript:document.delColumn_${columnKey}.submit()" title="${delColumnHint}">${delColumnLabel}</a>
          </li>
          <li>
            <form method="post" action="addPortlet" name="addPortlet_${columnKey}">
              ${columnKeyFields!}
            </form>
            <a class="${styles.link_action!}" href="javascript:document.addPortlet_${columnKey}.submit()" title="${addPortletHint}">${addPortletLabel}</a>
          </li>
          <li>
            <form method="post" action="editPortalPageColumnWidth" name="setColumnSize_${columnKey}">
              ${columnKeyFields!}
            </form>
            <a class="${styles.link_action!}" href="javascript:document.setColumnSize_${columnKey}.submit()" title="${setColumnSizeHint}">${colWidthLabel}: ${width}</a>
          </li>
        </ul>
      </div>
      <br class="clear"/>
    </#if>
</#macro>

<#macro renderPortalPageColumnEnd>
    <@cell close=true open=false /> 
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
