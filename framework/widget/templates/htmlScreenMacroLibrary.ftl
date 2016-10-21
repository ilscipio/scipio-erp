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
SCIPIO: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, locale, and to some extent (since 2016-01-06), uiLabelMap.
WARN: no code run here or indirectly from here should assume full current context present. only use well-known generic vars.

NOTE: 2016-10-05: Widget early HTML encoding is now DISABLED for all HTML macros.
    As a result all macros here must take care to html-escape as well as js-escape values.
    Use escapeVal/escapeFullUrl for this.
-->
<#macro renderScreenBegin extraArgs...>
<#-- SCIPIO: NOTE: HTML head open is now in scipio template macros. 
     In OOTB ofbiz no context is passed here (locale, etc.) so did not belong here and cleaner if in scipio macros. -->
<!DOCTYPE html>
</#macro>

<#macro renderScreenEnd extraArgs...>
</#macro>

<#macro renderSectionBegin boundaryComment extraArgs...>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#macro renderSectionEnd boundaryComment extraArgs...>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#macro renderContainerBegin id style autoUpdateLink autoUpdateInterval extraArgs...>
  <#if autoUpdateLink?has_content>
    <@script>ajaxUpdateAreaPeriodic('${escapeVal(id, 'js')}', '${escapeFullUrl(autoUpdateLink, 'js')}', '', '${autoUpdateInterval}');</@script>
  </#if>
  <#-- SCIPIO: now support a few more containers -->
  <#local elem = "">
  <#if ["div", "span", "p"]?seq_contains(style)>
    <#local elem = style>
    <#local style = "">
  <#elseif style?contains(":")>
    <#local parts = style?split(":")>
    <#local elem = parts[0]>
    <#local style = parts[1]>
  </#if>
  <#-- SCIPIO: delegate to scipio libs -->
  <@container open=true close=false class=style id=id elem=elem />
</#macro>

<#macro renderContainerEnd extraArgs...>
  <@container close=true open=false />
</#macro>

<#macro renderContentBegin editRequest enableEditValue editContainerStyle extraArgs...><#if editRequest?has_content && enableEditValue == "true"><div class="${escapeVal(editContainerStyle, 'html')}"></#if></#macro>

<#macro renderContentBody extraArgs...></#macro>

<#macro renderContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...>

<#if editRequest?exists && enableEditValue == "true">
<#if urlString?exists><a href="${escapeFullUrl(urlString, 'html')}">${escapeVal(editMode, 'htmlmarkup')}</a><#rt/></#if>
<#if editContainerStyle?exists></div><#rt/></#if>
</#if>
</#macro>

<#macro renderSubContentBegin editContainerStyle editRequest enableEditValue extraArgs...><#if editRequest?exists && enableEditValue == "true"><div class="${escapeVal(editContainerStyle, 'html')}"></#if></#macro>

<#macro renderSubContentBody extraArgs...></#macro>

<#macro renderSubContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...>
<#if editRequest?exists && enableEditValue == "true">
<#if urlString?exists><a href="${escapeFullUrl(urlString, 'html')}">${escapeVal(editMode, 'htmlmarkup')}</a><#rt/></#if>
<#if editContainerStyle?exists></div><#rt/></#if>
</#if>
</#macro>

<#macro renderHorizontalSeparator id style extraArgs...><hr<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>/></#macro>

<#macro renderLabel text id style extraArgs...>
  <@renderLabelCommon text=text id=id style=style />
</#macro>

<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name height width linkUrl text imgStr extraArgs...>
    <#if "ajax-window" != linkType>
        <#if "hidden-form" == linkType>
            <form method="post" action="${escapeFullUrl(actionUrl, 'html')}"<#if targetWindow?has_content> target="${escapeVal(targetWindow, 'html')}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${escapeVal(uniqueItemName, 'html')}"><#rt/>
                <#list parameterList as parameter>
                <input name="${escapeVal(parameter.name, 'html')}" value="${escapeVal(parameter.value, 'html')}" type="hidden"/><#rt/>
                </#list>
            </form><#rt/>
        </#if>
        <a 
            <#if id?has_content>id="${escapeVal(id, 'html')}"</#if> 
            <#if style?has_content>class="${escapeVal(style, 'html')}"</#if> 
            <#if name?has_content>name="${escapeVal(name, 'html')}"</#if> 
            <#if targetWindow?has_content>target="${escapeVal(targetWindow, 'html')}"</#if> 
            <#-- FIXME: dangerous lookup -->
            href="<#if "hidden-form"==linkType>javascript:document['${escapeVal(uniqueItemName, 'js-html')}'].submit()<#else>${escapeFullUrl(linkUrl, 'html')}</#if>"><#rt/>
            <#if imgStr?has_content>${imgStr}</#if><#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if>
        </a>
    <#else>
        <div id="${escapeVal(uniqueItemName, 'html')}"></div>
        <a href="javascript:void(0);" id="${escapeVal(uniqueItemName, 'html')}_link" 
        <#if style?has_content>class="${escapeVal(style, 'html')}"</#if>>
        <#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if></a>
        <@script>
            function getRequestData() {
                var data = {
                    <#list parameterList as parameter>
                        "${escapeVal(parameter.name, 'js')}": "${escapeVal(parameter.value, 'js')}",
                    </#list>
                    "presentation": "layer"
                };
        
                return data;
            }
            jQuery("#${escapeVal(uniqueItemName, 'js')}_link").click( function () {
                jQuery("#${escapeVal(uniqueItemName, 'js')}").dialog("open");
            });
            jQuery("#${escapeVal(uniqueItemName, 'js')}").dialog({
                 autoOpen: false,
                 <#if text?has_content>title: "${escapeVal(text, 'js')}",</#if>
                 height: ${height},
                 width: ${width},
                 modal: true,
                 open: function() {
                         jQuery.ajax({
                             url: "${escapeFullUrl(target, 'js')}",
                             type: "POST",
                             data: getRequestData(),
                             success: function(data) {jQuery("#${escapeVal(uniqueItemName, 'js')}").html(data);}
                         });
                 }
            });
        </@script>
    </#if>
</#macro>

<#macro renderImage src id style wid hgt border alt urlString extraArgs...>
<#if src?has_content>
<img<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if><#if wid?has_content> width="${wid}"</#if><#if hgt?has_content> height="${hgt}"</#if><#if border?has_content> border="${escapeVal(border, 'html')}"</#if> alt="<#if alt?has_content>${escapeVal(alt, 'html')}</#if>" src="${escapeFullUrl(urlString, 'html')}" />
</#if>
</#macro>

<#macro renderContentFrame fullUrl width height border extraArgs...><iframe src="${escapeFullUrl(fullUrl, 'html')}" width="${width}" height="${height}"<#if border?has_content> border="${escapeVal(border, 'html')}"</#if> /></#macro>

<#-- SCIPIO: new params: menuRole, titleStyle -->
<#macro renderScreenletBegin id="" title="" collapsible=false saveCollapsed=true collapsibleAreaId="" expandToolTip=true collapseToolTip=true fullUrlString="" padded=false menuString="" showMore=true collapsed=false javaScriptEnabled=true menuRole="" titleStyle="" extraArgs...>
    <#-- now delegates to Scipio implementation. -->
    <#-- NOTE (2016-09-09): We NO LONGER pass collapsibleAreaId - there is no explicit attribute in screen widgets, 
        and we should let @section_core assign a default so it is consistent system-wide: contentId=collapsibleAreaId -->
    <@section_core open=true close=false id=id title=title collapsible=collapsible saveCollapsed=saveCollapsed contentId="" expandToolTip=expandToolTip collapseToolTip=collapseToolTip fullUrlString=fullUrlString padded=padded menuContent=menuString 
        showMore=showMore collapsed=collapsed javaScriptEnabled=javaScriptEnabled fromScreenDef=true menuRole=menuRole requireMenu=false forceEmptyMenu=false hasContent=true titleStyle=titleStyle titleContainerStyle="" titleConsumeLevel=true 
        autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel="" />
</#macro>

<#macro renderScreenletSubWidget extraArgs...></#macro>

<#macro renderScreenletEnd extraArgs...>
    <@section_core close=true open=false />
</#macro>

<#macro renderScreenletPaginateMenu lowIndex actualPageSize ofLabel listSize paginateLastStyle lastLinkUrl paginateLastLabel paginateNextStyle nextLinkUrl paginateNextLabel paginatePreviousStyle paginatePreviousLabel previousLinkUrl paginateFirstStyle paginateFirstLabel firstLinkUrl extraArgs...>
    <li class="${escapeVal(paginateFirstStyle, 'html')}<#if !firstLinkUrl?has_content> disabled</#if>"><#if firstLinkUrl?has_content><a href="${escapeFullUrl(firstLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginateFirstLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginateFirstLabel, 'htmlmarkup')}</a></#if></li>
    <li class="${escapeVal(paginatePreviousStyle, 'html')}<#if !previousLinkUrl?has_content> disabled</#if>"><#if previousLinkUrl?has_content><a href="${escapeFullUrl(previousLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginatePreviousLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginatePreviousLabel, 'htmlmarkup')}</a></#if></li>
    <#if (listSize?number > 0)><li><span class="text-entry">${lowIndex?number + 1} - ${lowIndex?number + actualPageSize?number} ${escapeVal(ofLabel, 'htmlmarkup')} ${listSize}</span></li><#rt/></#if>
    <li class="${escapeVal(paginateNextStyle, 'html')}<#if !nextLinkUrl?has_content> disabled</#if>"><#if nextLinkUrl?has_content><a href="${escapeFullUrl(nextLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginateNextLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginateNextLabel, 'htmlmarkup')}</a></#if></li>
    <li class="${escapeVal(paginateLastStyle, 'html')}<#if !lastLinkUrl?has_content> disabled</#if>"><#if lastLinkUrl?has_content><a href="${escapeFullUrl(lastLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginateLastLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginateLastLabel, 'htmlmarkup')}</a></#if></li>
</#macro>

<#macro renderPortalPageBegin originalPortalPageId portalPageId confMode="false" addColumnLabel="Add column" addColumnHint="Add a new column to this portal" columnCount=1 extraArgs...>
  <#global portalPageGridUsed = 0>
  <#--
  <#if confMode == "true">
    <a class="${styles.link_run_sys!} ${styles.action_add!}" href="javascript:document['addColumn_${escapeVal(portalPageId, 'js-html')}'].submit()" title="${escapeVal(addColumnHint, 'html')}">${escapeVal(addColumnLabel, 'htmlmarkup')}</a> <b>PortalPageId: ${escapeVal(portalPageId, 'html')}</b>
    <form method="post" action="addPortalPageColumn" name="addColumn_${escapeVal(portalPageId, 'html')}">
      <input name="portalPageId" value="${escapeVal(portalPageId, 'html')}" type="hidden"/>
    </form>
  </#if>
  -->
  <@row open=true close=false />
    <@cell open=true close=false /> 
      <@row open=true close=false />
        <#-- for now, make each column a huge cell 
             (can't use grid any other way without rewriting portal page render order?) -->
</#macro>

<#macro renderPortalPageEnd extraArgs...>
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


<#macro renderPortalPageColumnBegin originalPortalPageId portalPageId columnSeqId confMode="false" width="auto" delColumnLabel="Delete column" delColumnHint="Delete this column" addPortletLabel="Add portlet" addPortletHint="Add a new portlet to this column" colWidthLabel="Col. width:" setColumnSizeHint="Set column size" columnCount=1 columnIndex=0 extraArgs...>
  <#local gridSize = 12>
  <#local firstColumn = (columnIndex <= 0)>
  <#local lastColumn = (columnIndex >= (columnCount - 1))>
  
  <#-- SCIPIO: calc and adapt width to possible grid widths + record grid used so far in global var -->
  <#local columnSize = calcPortalPageColumnGridSize(columnCount, columnIndex, gridSize, portalPageGridUsed, width)>
  <#global portalPageGridUsed = portalPageGridUsed + columnSize>
  
  <#local columnKey = portalPageId+columnSeqId>
  <#local columnKeyFields = "">
  <#--
  <#local columnKeyFields = '<input name="portalPageId" value="' + portalPageId + '" type="hidden"/><input name="columnSeqId" value="' + columnSeqId + '" type="hidden"/>'>
  <@script>
    if (typeof SORTABLE_COLUMN_LIST != "undefined") {
      if (SORTABLE_COLUMN_LIST == null) {
        SORTABLE_COLUMN_LIST = "#portalColumn_${escapeVal(columnSeqId, 'js')}";
      } else {
        SORTABLE_COLUMN_LIST += ", #portalColumn_${escapeVal(columnSeqId, 'js')}";
      }
    }
  </@script>
  <td class="portal-column<#if confMode == "true">-config</#if> connectedSortable" style="vertical-align: top; <#if width?has_content> width:${width};</#if>" id="portalColumn_${escapeVal(columnSeqId, 'html')}">
  -->
    
    <#-- if it's the last column and didn't fill full gridSize, add 'end' class so doesn't float right and look weird -->
    <#local endClassStr = "">
    <#if lastColumn && (portalPageGridUsed < gridSize)>
        <#local endClassStr = " ${styles.grid_end!}">
    </#if>
    
    <#-- Column: columnCount: ${columnCount}, columnIndex: ${columnIndex}, portalPageGridUsed: ${portalPageGridUsed}, width: ${width} --> 
    <#local portalPageClasses = "${styles.grid_large!}${columnSize}${rawString(endClassStr)}">
    <@cell open=true close=false class=portalPageClasses />
    <#if confMode == "true">
      <div class="portal-column-config-title-bar">
        <ul>
          <li>
            <form method="post" action="deletePortalPageColumn" name="delColumn_${escapeVal(columnKey, 'html')}">
              ${columnKeyFields}
            </form>
            <a class="${styles.link_run_sys!} ${styles.action_remove!}" href="javascript:document['delColumn_${escapeVal(columnKey, 'js-html')}'].submit()" title="${escapeVal(delColumnHint, 'html')}">${escapeVal(delColumnLabel, 'htmlmarkup')}</a>
          </li>
          <li>
            <form method="post" action="addPortlet" name="addPortlet_${escapeVal(columnKey, 'html')}">
              ${columnKeyFields}
            </form>
            <a class="${styles.link_run_sys!} ${styles.action_add!}" href="javascript:document['addPortlet_${escapeVal(columnKey, 'js-html')}'].submit()" title="${escapeVal(addPortletHint, 'html')}">${escapeVal(addPortletLabel, 'htmlmarkup')}</a>
          </li>
          <li>
            <form method="post" action="editPortalPageColumnWidth" name="setColumnSize_${escapeVal(columnKey, 'html')}">
              ${columnKeyFields}
            </form>
            <a class="${styles.link_run_sys!} ${styles.action_update!}" href="javascript:document['setColumnSize_${escapeVal(columnKey, 'js-html')}'].submit()" title="${escapeVal(setColumnSizeHint, 'html')}">${escapeVal(colWidthLabel, 'htmlmarkup')}: ${width}</a>
          </li>
        </ul>
      </div>
      <br class="clear"/>
    </#if>
</#macro>

<#macro renderPortalPageColumnEnd extraArgs...>
    <@cell close=true open=false /> 
</#macro>

<#macro renderPortalPagePortletBegin originalPortalPageId portalPageId portalPortletId portletSeqId prevPortletId="" prevPortletSeqId="" nextPortletId="" nextPortletSeqId="" columnSeqId="" prevColumnSeqId="" nextColumnSeqId="" confMode="false" delPortletHint="Remove this portlet" editAttribute="false" editAttributeHint="Edit portlet parameters" width="auto" columnCount=1 columnIndex=0 extraArgs...>
  <#local portletKey = portalPageId+portalPortletId+portletSeqId>
  <#local portletKeyFields = '<input name="portalPageId" value="' + portalPageId + '" type="hidden"/><input name="portalPortletId" value="' + portalPortletId + '" type="hidden"/><input name="portletSeqId" value="' + portletSeqId  + '" type="hidden"/>'>
  
  <div id="PP_${escapeVal(portletKey, 'html')}" name="portalPortlet" class="noClass" portalPageId="${escapeVal(portalPageId, 'html')}" portalPortletId="${escapeVal(portalPortletId, 'html')}" columnSeqId="${escapeVal(columnSeqId, 'html')}" portletSeqId="${escapeVal(portletSeqId, 'html')}">
    <#if confMode == "true">
      <div class="portlet-config" id="PPCFG_${escapeVal(portletKey, 'html')}">
        <div class="portlet-config-title-bar">
          <ul>
            <li class="title">Portlet : [${escapeVal(portalPortletId, 'html')}]</li>
            <li class="remove">
              <form method="post" action="deletePortalPagePortlet" name="delPortlet_${escapeVal(portletKey, 'html')}">
                ${portletKeyFields}
              </form>
              <a href="javascript:document['delPortlet_${escapeVal(portletKey, 'js-html')}'].submit()" title="${escapeVal(delPortletHint, 'html')}">&nbsp;&nbsp;&nbsp;</a>
            </li>
            <#if editAttribute == "true">
              <li class="edit">
                <form method="post" action="editPortalPortletAttributes" name="editPortlet_${escapeVal(portletKey, 'html')}">
                  ${portletKeyFields}
                </form>
                <a href="javascript:document['editPortlet_${escapeVal(portletKey, 'js-html')}'].submit()" title="${escapeVal(editAttributeHint, 'html')}">&nbsp;&nbsp;&nbsp;</a>
              </li>
            </#if>
            <#if prevColumnSeqId?has_content>
              <li class="move-left">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletLeft_${escapeVal(portletKey, 'html')}">
                  <input name="o_portalPageId" value="${escapeVal(portalPageId, 'html')}" type="hidden"/>
                  <input name="o_portalPortletId" value="${escapeVal(portalPortletId, 'html')}" type="hidden"/>
                  <input name="o_portletSeqId" value="${escapeVal(portletSeqId, 'html')}" type="hidden"/>
                  <input name="destinationColumn" value="${escapeVal(prevColumnSeqId, 'html')}" type="hidden"/>
                  <input name="mode" value="DRAGDROPBOTTOM" type="hidden"/>
                </form>
                <a href="javascript:document['movePortletLeft_${escapeVal(portletKey, 'js-html')}'].submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
            <#if nextColumnSeqId?has_content>
              <li class="move-right">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletRight_${escapeVal(portletKey, 'html')}">
                  <input name="o_portalPageId" value="${escapeVal(portalPageId, 'html')}" type="hidden"/>
                  <input name="o_portalPortletId" value="${escapeVal(portalPortletId, 'html')}" type="hidden"/>
                  <input name="o_portletSeqId" value="${escapeVal(portletSeqId, 'html')}" type="hidden"/>
                  <input name="destinationColumn" value="${escapeVal(nextColumnSeqId, 'html')}" type="hidden"/>
                  <input name="mode" value="DRAGDROPBOTTOM" type="hidden"/>
                </form>
                <a href="javascript:document['movePortletRight_${escapeVal(portletKey, 'js-html')}'].submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
            <#if prevPortletId?has_content>
              <li class="move-up">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletUp_${escapeVal(portletKey, 'html')}">
                  <input name="o_portalPageId" value="${escapeVal(portalPageId, 'html')}" type="hidden"/>
                  <input name="o_portalPortletId" value="${escapeVal(portalPortletId, 'html')}" type="hidden"/>
                  <input name="o_portletSeqId" value="${escapeVal(portletSeqId, 'html')}" type="hidden"/>
                  <input name="d_portalPageId" value="${escapeVal(portalPageId, 'html')}" type="hidden"/>
                  <input name="d_portalPortletId" value="${escapeVal(prevPortletId, 'html')}" type="hidden"/>
                  <input name="d_portletSeqId" value="${escapeVal(prevPortletSeqId, 'html')}" type="hidden"/>
                  <input name="mode" value="DRAGDROPBEFORE" type="hidden"/>
                </form>
                <a href="javascript:document['movePortletUp_${escapeVal(portletKey, 'js-html')}'].submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
            <#if nextPortletId?has_content>
              <li class="move-down">
                <form method="post" action="updatePortletSeqDragDrop" name="movePortletDown_${escapeVal(portletKey, 'html')}">
                  <input name="o_portalPageId" value="${escapeVal(portalPageId, 'html')}" type="hidden"/>
                  <input name="o_portalPortletId" value="${escapeVal(portalPortletId, 'html')}" type="hidden"/>
                  <input name="o_portletSeqId" value="${escapeVal(portletSeqId, 'html')}" type="hidden"/>
                  <input name="d_portalPageId" value="${escapeVal(portalPageId, 'html')}" type="hidden"/>
                  <input name="d_portalPortletId" value="${escapeVal(nextPortletId, 'html')}" type="hidden"/>
                  <input name="d_portletSeqId" value="${escapeVal(nextPortletSeqId, 'html')}" type="hidden"/>
                  <input name="mode" value="DRAGDROPAFTER" type="hidden"/>
                </form>
                <a href="javascript:document['movePortletDown_${escapeVal(portletKey, 'js-html')}'].submit()">&nbsp;&nbsp;&nbsp;</a></li>
            </#if>
          </ul>
          <br class="clear"/>
        </div>
      </#if>
</#macro>

<#macro renderPortalPagePortletEnd confMode="false" extraArgs...>
  </div>
  <#if confMode == "true">
    </div>
  </#if>
</#macro>

<#macro renderColumnContainerBegin id style extraArgs...>
  <table cellspacing="0"<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
  <tr>
</#macro>

<#macro renderColumnContainerEnd extraArgs...>
  </tr>
  </table>
</#macro>

<#macro renderColumnBegin id style extraArgs...>
  <td<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
</#macro>

<#macro renderColumnEnd extraArgs...>
  </td>
</#macro>

<#-- TODO: Macro for pagination -->
