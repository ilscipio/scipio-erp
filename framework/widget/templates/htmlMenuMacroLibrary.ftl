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

<#-- 
*************************************
* CATO: NEW MENU MACROS (ONE-SHOT) *
*************************************
These render a whole single item without splitting into begin/end sections, using data in hashes.
Must be enabled in catoWebapp.properties (enabled by default in Cato).

TODO/FIXME:
* I don't see a good way to separate markup in these yet... may be none...
  * Ideally the html generation of these and the cato macros should be based on a common library
  * maybe @menu, but this is a lot more hardcoded than @menu, and the data structure needs extra conversion...
* expand renderLink/renderImage, or part of them
* the java is inefficient and implementation maybe not final (see OneShotMacro.java) - depends
  on how much end up using and where
* Special style names too hardcoded
-->

<#-- Cato: One-shot macro full menu
  Data structure (indented means member of): 
  items: List of maps, each entry corresponding to old @renderMenuItemBegin arguments
    (item)
      "linkArgs": Link args corresponding to @renderLink args. In some cases, this will be missing, and linkStr (string) will be present instead. must test for both cases. This is because some cases don't render an actual link but text instead (misleading ofbiz var name).
        "imgArgs": Image args corresponding to @renderImage args, for menu entries that are images instead of text.
      "items": List of sub-menu items. Same format as parent items. Will be non-empty if parent item.containsNestedMenus is true. NOTE: there's no dedicated submenu (<ul>) open/close element or map. Implementation decides how to handle and what to call recursively.
        (item)
          "items": This goes on recursively for nested menus...

  Menu styles can be set via menu-container-style attribute. The rendering will differ if one of the following classes is set
    * menu-type-main
    * menu-type-sidebar
    * menu-type-button
    * etc.
-->
<#macro renderMenuFull boundaryComment="" id="" style="" title="" inlineEntries=false menuCtxRole="" items=[] htmlWrap=true>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
<#--<p><@objectAsScript lang="raw" object=items /></p>-->
  <#-- Extract menu types from style string, remove, and get global style -->
  <#local type = "">
  <#local class = style>
  <#local menuTypeStyles = getStyleNamesByPrefix(class, "menu-type-")>
  <#if menuTypeStyles?has_content>
    <#local class = removeStyleNames(class, menuTypeStyles)>
    <#-- Use only the first, we only support one -->
    <#local type = menuTypeStyles[0]?substring(10)>
  </#if>
  <#-- FIXME?: we don't fully support +/= syntax here, non-standard, treat empty as "+"...
      We may not even receive a +/= sign at all; see ModelMenu.java, may get stripped; should maybe be changed... -->
  <#if !class?starts_with("=")>
    <#local class = "+" + class>
  </#if>
  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || (!(styles["menu_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>
  <#local class = addClassArgDefault(class, styles["menu_" + styleName]!styles["menu_default"]!"")>
  <#-- Count menu and make sure has ID -->
  <#local menuIdNum = getRequestVar("catoMenuIdNum")!0>
  <#local menuIdNum = menuIdNum + 1 />
  <#local dummy = setRequestVar("catoMenuIdNum", menuIdNum)>
  <#if !id?has_content>
    <#local id = "menu_" + menuIdNum> <#-- FIXME? is this name too general? -->
  </#if>
  <#if htmlWrap?is_boolean && htmlWrap == false>
    <#local htmlWrap = "">
  <#elseif (htmlWrap?is_boolean && htmlWrap == true) || !htmlWrap?has_content>
    <#local htmlWrap = styles["menu_" + styleName + "_htmlwrap"]!styles["menu_default_htmlwrap"]!true>
    <#if htmlWrap?is_boolean>
      <#local htmlWrap = htmlWrap?string("ul", "")>
    </#if>
  </#if>
  <#-- Menu open -->
  <#if !inlineEntries>
    <#local extraMenuAttribs = {}>
    <#if type == "main">
        <li class="${styles.menu_main_wrap!}"><a href="#" class="${styles.menu_main_item_link!}"
            <#if styles.framework?has_content && styles.framework =="bootstrap"> data-toggle="dropdown"</#if>>${title!}<#if styles.framework?has_content && styles.framework =="bootstrap"> <i class="fa fa-fw fa-caret-down"></i></#if></a>
    <#elseif type == "sidebar">
        <nav class="${styles.nav_sidenav!""}">
            <#-- FIXME: this "navigation" variable is way too generic name! is it even still valid? -->
            <#if navigation?has_content><h2>${navigation!}</h2></#if>
    <#elseif type == "button-dropdown">
        <button href="#" data-dropdown="${id}" aria-controls="${id}" aria-expanded="false" class="${styles.menu_button_dropdown_mainbutton!}">${title}</button><br>
        <#local extraMenuAttribs = extraMenuAttribs + {"data-dropdown-content":"true", "aria-hidden":"true"}>
    <#else>
      <#-- all other cases -->
      <#-- WARN: stock ofbiz usually applied styles to a containing div, 
           not sure should keep that behavior or not, but might not consistent with foundation styles? -->
    </#if>
        <#if htmlWrap?has_content><${htmlWrap}<#if id?has_content> id="${id}"</#if><@compiledClassAttribStr class=class /><@elemAttribStr attribs=extraMenuAttribs />></#if>
  </#if>
   <#local dummy = pushRequestStack("renderMenuStack", {"type":type, "class":class,"id":id,"inlineEntires":inlineEntries})> <#-- pushing info to stack, so that this can be used by subsequently --> 
  
  <#-- Items (nested) --> 
  <#list items as item>
    <@renderMenuItemFull style=item.style toolTip=item.toolTip linkArgs=item.linkArgs!{} linkStr=item.linkStr!"" 
        containsNestedMenus=item.containsNestedMenus menuCtxRole=item.menuCtxRole items=item.items![] 
        menuType=type menuStyleName=styleName menuHtmlWrap=htmlWrap />
  </#list>

  <#-- Menu close -->
  <#local menuStack = popRequestStack("renderMenuStack")>
  <#if !inlineEntries>
    <#if type == "main">
            <#if htmlWrap?has_content></${htmlWrap}></#if>
        </li>
    <#elseif type == "sidebar">
            <#if htmlWrap?has_content></${htmlWrap}></#if>
        </nav>
    <#else>
        <#if htmlWrap?has_content></${htmlWrap}></#if>
    </#if>
  </#if>
  
  <#if !readRequestStack("renderMenuStack")??> <#-- if top-level menu -->
    <#local renderMenuHiddenFormContent = getRequestVar("renderMenuHiddenFormContent")!"">
    <#if renderMenuHiddenFormContent?has_content>
      ${renderMenuHiddenFormContent}
      <#-- NOTE: we don't have to worry about recursion here; will accumulate all forms from sub-menus as well;
           NOTE: for simplicity, don't use xxxRequestStack for now, probably not needed -->
      <#local dummy = setRequestVar("renderMenuHiddenFormContent", "")>
    </#if>
  </#if>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#-- Cato: Render full menu item. Separate macro required due to recursive nested menus. 
    NOTE: if linkArgs empty, there may still be content in linkStr (that was not traditionally passed through a macro call), which is not necessarily a link! -->
<#macro renderMenuItemFull style="" toolTip="" linkArgs={} linkStr="" containsNestedMenus=false menuCtxRole="" items=[] menuType="" menuStyleName="" menuHtmlWrap=true htmlWrap=true>
  <#-- TODO? maybe want to expand the renderLink and/or renderImage calls -->
  <#if linkArgs?has_content>
    <#local imgStr = "">
    <#if linkArgs.imgArgs?has_content>
      <#local imgArgs = linkArgs.imgArgs>
      <#local imgStr><@renderImage imgArgs.src imgArgs.id imgArgs.style imgArgs.width imgArgs.height imgArgs.border imgArgs.menuCtxRole /></#local>
    </#if>
    <#local linkStr><@renderLink linkArgs.linkUrl linkArgs.parameterList linkArgs.targetWindow linkArgs.uniqueItemName linkArgs.actionUrl linkArgs.linkType linkArgs.id linkArgs.style linkArgs.name linkArgs.height linkArgs.width linkArgs.text imgStr linkArgs.menuCtxRole /></#local>
  </#if>
  <#if htmlWrap?is_boolean && htmlWrap == false>
    <#local htmlWrap = "">
  <#elseif (htmlWrap?is_boolean && htmlWrap == true) || !htmlWrap?has_content>
    <#local htmlWrap = styles["menu_" + menuStyleName + "_item_htmlwrap"]!styles["menu_default_item_htmlwrap"]!true>
    <#if htmlWrap?is_boolean>
      <#local htmlWrap = htmlWrap?string("li", "")>
    </#if>
  </#if>
  <#if htmlWrap?has_content><${htmlWrap}<@compiledClassAttribStr class=style /><#if toolTip?has_content> title="${toolTip}"</#if>></#if><#if linkStr?has_content>${linkStr}</#if><#rt>
    <#if containsNestedMenus>
      <#if menuHtmlWrap?has_content><${menuHtmlWrap}></#if>
      <#list items as item>
        <@renderMenuItemFull style=item.style toolTip=item.toolTip linkArgs=item.linkArgs!{} linkStr=item.linkStr!"" 
            containsNestedMenus=item.containsNestedMenus menuCtxRole=item.menuCtxRole items=item.items![] 
            menuType=menuType menuStyleName=menuStyleName menuHtmlWrap=menuHtmlWrap />
      </#list>
      <#if menuHtmlWrap?has_content></${menuHtmlWrap}></#if>
    </#if>
  <#if htmlWrap?has_content></${htmlWrap}></#if><#lt>
</#macro>


<#-- 
*************************************
* CATO: TRANSITION MENU MACROS *
*************************************
-->

<#-- Cato: Delegating implementation of one shot menu - used as reference
<#macro renderMenuFull boundaryComment="" id="" style="" title="" inlineEntries=false menuCtxRole="" items=[]>
  <@renderMenuBegin boundaryComment=boundaryComment id=id style=style title=title inlineEntries=inlineEntries menuCtxRole=menuCtxRole />
  <#list items as item>
    <@renderMenuItemFull item.style item.toolTip item.linkArgs!{} item.linkStr!"" item.containsNestedMenus item.menuCtxRole item.items![]/>
  </#list>
  <@renderMenuEnd boundaryComment=boundaryComment style=style inlineEntries=inlineEntries menuCtxRole=menuCtxRole />
</#macro>

<#macro renderMenuItemFull style="" toolTip="" linkArgs={} linkStr="" containsNestedMenus=false menuCtxRole="" items=[]>
  <#if linkArgs?has_content>
    <#local imgStr = "">
    <#if linkArgs.imgArgs?has_content>
      <#local imgArgs = linkArgs.imgArgs>
      <#local imgStr><@renderImage imgArgs.src imgArgs.id imgArgs.style imgArgs.width imgArgs.height imgArgs.border imgArgs.menuCtxRole /></#local>
    </#if>
    <#local linkStr><@renderLink linkArgs.linkUrl linkArgs.parameterList linkArgs.targetWindow linkArgs.uniqueItemName linkArgs.actionUrl linkArgs.linkType linkArgs.id linkArgs.style linkArgs.name linkArgs.height linkArgs.width linkArgs.text imgStr linkArgs.menuCtxRole /></#local>
  </#if>

  <@renderMenuItemBegin style toolTip linkStr containsNestedMenus menuCtxRole />
    <#if containsNestedMenus>
      <#list items as item>
        <@renderMenuItemFull item.style item.toolTip item.linkArgs!{} item.linkStr!"" item.containsNestedMenus item.menuCtxRole item.items![] />
      </#list>
    </#if>
  <@renderMenuItemEnd containsNestedMenus menuCtxRole />
</#macro>
-->


<#-- 
*************************************
* CATO: TRADITIONAL MENU MACROS *
*************************************
Mostly deprecated and no longer need to maintain except where noted.
-->

<#-- 
Menu styles can be set via menu-container-style attribute. The rendering will differ if one of the following classes is set
    * menu-main
    * menu-sidebar
    * menu-button
    * menu-tab // ToDo
-->
<#-- Cato: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
<#macro renderMenuBegin boundaryComment="" id="" style="" title="" inlineEntries=false menuCtxRole="">
  <#local styleSet = splitStyleNamesToSet(style)>
  <#local remStyle = "">
<#if boundaryComment?has_content>
<!- ${boundaryComment} ->
</#if>
  <#local menuIdNum = getRequestVar("catoMenuIdNum")!0>
  <#local menuIdNum = menuIdNum + 1 />
  <#local dummy = setRequestVar("catoMenuIdNum", menuIdNum)>
  <#if !id?has_content>
    <#local id = "menu_" + menuIdNum>
  </#if>
  <#if !inlineEntries>
    <#local extraMenuAttribs = {}>
    <#if styleSet.contains("menu-main")>
      <#local remStyle = removeStyleNames(style, "menu-main")>
        <li class="${styles.menu_main_wrap!}"><a href="#" class="${styles.menu_main_item_link!}"
            <#if styles.framework?has_content && styles.framework =="bootstrap"> data-toggle="dropdown"</#if>>${title!}<#if styles.framework?has_content && styles.framework =="bootstrap"> <i class="fa fa-fw fa-caret-down"></i></#if></a>
      <#local classes = joinStyleNames(styles.menu_main!, remStyle)>
    <#elseif styleSet.contains("menu-sidebar")>
      <#local remStyle = removeStyleNames(style, "menu-sidebar")>
        <nav class="${styles.nav_sidenav!""}">
            <#if navigation?has_content><h2>${navigation!}</h2></#if>
      <#local classes = joinStyleNames(styles.menu_sidebar!, remStyle)>
    <#elseif styleSet.contains("menu-button")>
      <#local remStyle = removeStyleNames(style, "menu-button")>
      <#local classes = joinStyleNames(styles.menu_button!, remStyle)>
    <#elseif styleSet.contains("menu-button-dropdown")>
      <#local remStyle = removeStyleNames(style, "menu-button-dropdown")>
      <#local classes = joinStyleNames(styles.menu_button_dropdown!, remStyle)>
      <button href="#" data-dropdown="${id}" aria-controls="${id}" aria-expanded="false" class="${styles.menu_button_dropdown_mainbutton!}">${title}</button><br>
      <#local extraMenuAttribs = extraMenuAttribs + {"data-dropdown-content":"true", "aria-hidden":"true"}>
    <#elseif styleSet.contains("menu-tab")>    
      <#local remStyle = removeStyleNames(style, "menu-tab")>
      <#local classes = joinStyleNames(styles.menu_tab!, remStyle)>
    <#elseif styleSet.contains("button-bar")>
      <#- NOTE (2016-02-08): There should be no more "button-bar" style left in *Menus.xml... should all go through CommonButtonBarMenu (menu-button) or alternative base menu ->
      <#local remStyle = removeStyleNames(style, ["button-bar"])> <#-- ["button-bar", "no-clear"] ->
      <#- right now translating button-bar menu-container-style here to avoid modifying all menu styles
           note: in stock, button-bar usually accompanied by one of: button-style-2, tab-bar; also found: no-clear (removed above) ->
      <#- WARN: stock ofbiz usually applied styles to a containing div, 
           not sure should keep that behavior or not, but might not consistent with foundation styles? ->
      <#local classes = joinStyleNames(styles.menu_button!, remStyle)>
    <#else>
      <#- all other cases ->
      <#- WARN: stock ofbiz usually applied styles to a containing div, 
           not sure should keep that behavior or not, but might not consistent with foundation styles? ->
      <#local classes = joinStyleNames(styles.menu_default!, style)>
    </#if>
        <ul<#if id?has_content> id="${id}"</#if><#if classes?has_content> class="${classes}"</#if><@elemAttribStr attribs=extraMenuAttribs />>
            <#- Hardcoded alternative that will always display a Dashboard link on top of the sidebar
            <#local dashboardLink><a href="<@ofbizUrl>/main</@ofbizUrl>">${uiLabelMap.CommonDashboard!}</a></#local>
            <@renderMenuItemBegin style="${styles.menu_sidebar_itemdashboard!}" linkStr=dashboardLink! /><@renderMenuItemEnd/>->
  </#if>
   <#local dummy = pushRequestStack("renderMenuStack", {"style":style,"remStyle":remStyle,"id":id,"inlineEntires":inlineEntries})> <#- pushing info to stack, so that this can be used by subsequently -> 
</#macro>
-->

<#-- Cato: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
<#macro renderMenuEnd boundaryComment="" style="" inlineEntries=false menuCtxRole="">
  <#local styleSet = splitStyleNamesToSet(style)>
  <#local menu = popRequestStack("renderMenuStack")>
  <#if !inlineEntries>
    <#-        
    <#if isSubMenu>
            </ul>
    <#else>
        </ul>
        </li>
        <#global isSubMenu=true/>
    </#if>
    ->
    <#if styleSet.contains("menu-main")>
            </ul>
        </li>
    <#elseif styleSet.contains("menu-sidebar")>
            </ul>
        </nav>
    <#elseif styleSet.contains("menu-button")>
        </ul>
    <#elseif styleSet.contains("menu-tab")>
        </ul>
    <#elseif styleSet.contains("button-bar")>
        </ul>
    <#else>
        </ul>
    </#if>
  </#if>
  
  <#if !readRequestStack("renderMenuStack")??> <#- if top-level menu ->
    <#local renderMenuHiddenFormContent = getRequestVar("renderMenuHiddenFormContent")!"">
    <#if renderMenuHiddenFormContent?has_content>
      ${renderMenuHiddenFormContent}
      <#- note: we don't have to worry about recursion here; will accumulate all forms from sub-menus as well;
           note: for simplicity, don't use xxxRequestStack for now, probably not needed ->
      <#local dummy = setRequestVar("renderMenuHiddenFormContent", "")>
    </#if>
  </#if>
<#if boundaryComment?has_content>
<!- ${boundaryComment} ->
</#if>
</#macro>
-->

<#-- Cato: TODO: refactor? -->
<#macro renderImage src id style width height border menuCtxRole="">
<img src="${src}"<#if id?has_content> id="${id}"</#if><#if style?has_content> class="${style}"</#if><#if width?has_content> width="${width}"</#if><#if height?has_content> height="${height}"</#if><#if border?has_content> border="${border}"</#if> />
</#macro>

<#-- Cato: TODO: refactor? -->
<#macro renderLink linkUrl parameterList targetWindow uniqueItemName actionUrl linkType="" id="" style="" name="" height="" width="" text="" imgStr="" menuCtxRole="">
<#-- Cato: hack: for screenlet nav menus, always impose buttons if no style specified, 
     because can't centralize these menus easily anywhere else. -->
<#if menuCtxRole=="screenlet-nav-menu">
  <#if !style?has_content>
    <#local style = "${styles.menu_section_item_link!}">
  </#if>
</#if>
<#-- Cato: treat "none" keyword as requesting empty style, as workaround -->
<#if style == "none">
  <#local style = "">
</#if>

  <#if linkType?has_content && "hidden-form" == linkType>
    <#local hiddenFormContent>
<form method="post" action="${actionUrl}"<#if targetWindow?has_content> target="${targetWindow}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${uniqueItemName}" class="menu-widget-action-form"><#rt/>
    <#list parameterList as parameter>
<input name="${parameter.name}" value="${parameter.value}" type="hidden"/><#rt/>
    </#list>
</form><#rt/>
    </#local>
    <#local renderMenuHiddenFormContent = getRequestVar("renderMenuHiddenFormContent")!"">
    <#local dummy = setRequestVar("renderMenuHiddenFormContent", renderMenuHiddenFormContent+hiddenFormContent)>
  </#if>
<#if (linkType?has_content && "hidden-form" == linkType) || linkUrl?has_content>
<a<#if id?has_content> id="${id}"</#if><#if style?has_content> class="${style}"</#if><#if name?has_content> name="${name}"</#if><#if targetWindow?has_content> target="${targetWindow}"</#if> href="<#if "hidden-form"==linkType>javascript:document.${uniqueItemName}.submit()<#else>${linkUrl}</#if>"><#rt/>
</#if>
<#if imgStr?has_content>${imgStr}</#if><#if text?has_content>${text}</#if><#rt/>
<#if (linkType?has_content && "hidden-form" == linkType) || linkUrl?has_content></a><#rt/></#if>
</#macro>

<#-- Cato: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
<#macro renderMenuItemBegin style toolTip="" linkStr="" containsNestedMenus=false menuCtxRole="">
        <li<#if style?has_content> class="${style}"</#if><#if toolTip?has_content> title="${toolTip}"</#if>><#if linkStr?has_content>${linkStr}</#if><#if containsNestedMenus><ul></#if><#rt/>
</#macro>-->

<#-- Cato: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
<#macro renderMenuItemEnd containsNestedMenus=false menuCtxRole="">
<#if containsNestedMenus></ul></#if></li>
</#macro>-->
