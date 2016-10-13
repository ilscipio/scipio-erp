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
    Use escapePart/escapeFullUrl for this.
-->

<#-- 
*************************************
* SCIPIO: NEW MENU MACROS (ONE-SHOT) *
*************************************
These render a whole single item without splitting into begin/end sections, using data in hashes.
Must be enabled in scipioWebapp.properties (enabled by default in Scipio).

NOTE (2016-09-01): These macros are now expected to accept and silently ignore any extra parameters (using extraArgs... catch-all).

TODO/FIXME:
* Currently the new renderMenuFull partly shares code with @menu, but only markup macros;
  logic is duplicated. This is a compromise for now...
* Integrate renderLink with the @menu markup stuff...
* Sub-menu control is limited, but not really implemented anywhere else either.
* The java for one-shot macro is inefficient and implementation maybe not final (see OneShotMacro.java) - depends
  on how much end up using and where
-->

<#-- SCIPIO: One-shot macro full menu
  Data structure (indented means member of): 
  items: List of maps, each entry corresponding to old @renderMenuItemBegin arguments
    (item)
      "linkArgs": Link args corresponding to @renderLink args. In some cases, this will be missing, and linkStr (string) will be present instead. must test for both cases. This is because some cases don't render an actual link but text instead (misleading ofbiz var name).
        "imgArgs": Image args corresponding to @renderImage args, for menu entries that are images instead of text.
      "items": List of sub-menu items. Same format as parent items. Will be non-empty if parent item.containsNestedMenus is true. NOTE: there's no dedicated submenu (<ul>) open/close element or map. Implementation decides how to handle and what to call recursively.
        (item)
          "items": This goes on recursively for nested menus...

  Menu types can be set via menu-container-style attribute by specifying a "virtual" style name having the prefix "menu-type-", 
  which will cause a different set of default styles and rendering to be used. 
  Specifying "menu-type-xxx" is roughly equivalent to specifying type="xxx" on @menu macro.
  The "menu-type-xxx" name will only be included in the resulting class attribute if it is specified as a style in the styles hash; otherwise is automatically removed.
  See @menu macro and style hash for possible types.
  
  NOTE: This may be called recursively. In stock Ofbiz, @renderMenuBegin was never called recursively.
  
  TODO?: menu-container-style does not currently fully support the standard Scipio +/= class prefix; generally, "+" will be assumed.
-->
<#macro renderMenuFull boundaryComment="" id="" style="" title="" inlineEntries=false menuCtxRole="" items=[] selected=false selectedAncestor=false extraArgs...>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
<#--<p><@objectAsScript lang="raw" object=items /></p>-->
  <#local prevMenuInfo = readRequestStack("renderMenuStack")!{}>
  <#local topLevel = !(prevMenuInfo.type)??>
  <#local isNestedMenu = !topLevel>
  <#local menuLevel = (getRequestStackSize("renderMenuStack")!0)+1>
  
  <#local parentMenuType = "">
  <#local parentStyleName = "">
  <#local parentMenuSpecialType = "">
  
  <#if isNestedMenu>
    <#local parentMenuType = (prevMenuInfo.type)!"">
    <#local parentStyleName = parentMenuType?replace("-","_")>
    
    <#if parentMenuType?has_content>
      <#-- make sure to look this up again because caller may override
      <#local parentMenuSpecialType = (prevMenuInfo.specialType)!"">-->
      <#local parentMenuSpecialType = styles["menu_" + parentStyleName + "_specialtype"]!"">
    </#if>
  </#if>

  <#-- Extract menu types from style string, remove, and get global style -->
  <#local type = "">
  <#local class = style>
  <#local menuTypeStyles = getStyleNamesByPrefix(class, "menu-type-")>
  <#if menuTypeStyles?has_content>
    <#local class = removeStyleNames(class, menuTypeStyles)>
    <#-- Use only the LAST type entry, so last overrides the first, without java having to do anything special -->
    <#local type = menuTypeStyles?last?substring(10)>
  </#if>

  <#if !type?has_content>
    <#if isNestedMenu && parentMenuType?has_content>
      <#local type = parentMenuType>
    <#else>
      <#-- Do NOT use generic as default for widgets, for now
      <#local type = "generic"> -->
    </#if>
  </#if>  

  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || (!(styles["menu_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>

  <#if isNestedMenu && (type == parentMenuType)>
    <#-- If nested menu of same type as parent, use alternate menu class -->
    <#local class = addClassArgDefault(class, styles["menu_" + styleName + "_altnested"]!styles["menu_default_altnested"]!"")>
  <#else>
    <#local class = addClassArgDefault(class, styles["menu_" + styleName]!styles["menu_default"]!"")>
  </#if>

  <#local active = selected || selectedAncestor>
  <#local activeTarget = selected>
  <#local class = menuAppendActiveStyle(class, styleName, "_active", active, activeTarget)>

  <#local menuLevelPrefix = styles["menu_" + styleName + "_levelprefix"]!styles["menu_default_levelprefix"]!"">
  <#if menuLevelPrefix?has_content>
    <#local class = addClassArg(class, menuLevelPrefix + menuLevel?string)>
  </#if>

  <#-- Count menu and make sure has ID -->
  <#local menuIdNum = getRequestVar("scipioMenuIdNum")!0>
  <#local menuIdNum = menuIdNum + 1 />
  <#local dummy = setRequestVar("scipioMenuIdNum", menuIdNum)>
  <#if !id?has_content>
    <#local id = "menu_" + menuIdNum><#-- FIXME? is this name too generic? -->
  </#if>

  <#-- Special menu settings -->
  <#local htmlwrap = styles["menu_" + styleName + "_htmlwrap"]!styles["menu_default_htmlwrap"]!true>
  <#if htmlwrap?is_boolean>
    <#local htmlwrap = htmlwrap?string("ul", "")>
  </#if>
  <#local specialType = styles["menu_" + styleName + "_specialtype"]!"">
  
  <#-- Add this for all top-level menus (very generic identifier) -->
  <#if !isNestedMenu>
    <#local class = addClassArg(class, styles["menu_" + styleName + "_toplevel"]!styles["menu_default_toplevel"]!"")>
  </#if>  
  
  <#-- Add this for all nested menus (very generic identifier) -->
  <#if isNestedMenu>
    <#local class = addClassArg(class, styles["menu_" + styleName + "_nested"]!styles["menu_default_nested"]!"")>
    <#if type == parentMenuType>
      <#local class = addClassArg(class, styles["menu_" + styleName + "_nestedsame"]!styles["menu_default_nestedsame"]!"")>
    </#if>
  </#if>
  
  <#local mainButtonClass = "">
  <#local mainButtonClass = addClassArgDefault(mainButtonClass, styles["menu_" + styleName + "_mainbutton"]!"")>
  
  <#local menuInfo = {"type":type, "specialType":specialType, "styleName":styleName, "class":class, "id":id, 
    "menuIdNum":menuIdNum, "menuCtxRole":menuCtxRole, "inlineEntries":inlineEntries, "htmlwrap":htmlwrap,
    "isNestedMenu":isNestedMenu, "menuLevel":menuLevel,
    "parentMenuType":parentMenuType, "parentMenuSpecialType":parentMenuSpecialType, "parentStyleName":parentStyleName}>
  <#local dummy = pushRequestStack("renderMenuStack", menuInfo)> <#-- pushing info to stack, so that this can be used by subsequently --> 
  <#if inlineEntries>
    <#list items as item>
        <#-- DEV NOTE: always copy-paste this call again below, should be exactly the same (can't use capture) -->
        <@renderMenuItemFull style=item.style toolTip=item.toolTip linkArgs=(item.linkArgs!{}) linkStr=(item.linkStr!"") 
            containsNestedMenus=item.containsNestedMenus menuCtxRole=item.menuCtxRole items=(item.items![]) 
            subMenuId=item.subMenuId subMenuStyle=item.subMenuStyle subMenuTitle=item.subMenuTitle subMenuList=(item.subMenuList![])
            disabled=item.disabled selected=item.selected selectedAncestor=item.selectedAncestor itemIndex=item_index menuInfo=menuInfo/>
    </#list>
  <#else>
    <@menu_markup type=type specialType=specialType class=class id=id style="" attribs=extraMenuAttribs 
        excludeAttribs=["class", "id", "style"] inlineItems=false mainButtonClass=mainButtonClass title=title 
        htmlwrap=htmlwrap parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType
        isNestedMenu=isNestedMenu menuLevel=menuLevel active=active activeTarget=activeTarget>
      <#list items as item>
        <@renderMenuItemFull style=item.style toolTip=item.toolTip linkArgs=(item.linkArgs!{}) linkStr=(item.linkStr!"") 
            containsNestedMenus=item.containsNestedMenus menuCtxRole=item.menuCtxRole items=(item.items![]) 
            subMenuId=item.subMenuId subMenuStyle=item.subMenuStyle subMenuTitle=item.subMenuTitle subMenuList=(item.subMenuList![])
            disabled=item.disabled selected=item.selected selectedAncestor=item.selectedAncestor itemIndex=item_index menuInfo=menuInfo/>
      </#list>
    </@menu_markup>
  </#if>
  <#local dummy = popRequestStack("renderMenuStack")>
  
  <#if topLevel>
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

<#-- SCIPIO: Render full menu item. Separate macro required due to recursive nested menus. 
    NOTE: if linkArgs empty, there may still be content in linkStr (that was not traditionally passed through a macro call), which is not necessarily a link! 
    NOTE (2016-08-26): The items arg is no longer populated; instead, an explicit subMenuList is now passed.
        The subMenuStyle/subMenuTitle/subMenuId are now considered deprecated and appear as style/title/id on the subMenuList
        hash entries instead. -->
<#macro renderMenuItemFull style="" toolTip="" linkArgs={} linkStr="" containsNestedMenus=false menuCtxRole="" items=[] 
    subMenuStyle="" subMenuTitle="" itemIndex=0 menuInfo={} disabled=false selected=false selectedAncestor=false subMenuId="" subMenuList=[] extraArgs...>
  <#local class = style>
  <#local id = "">
  <#local type = ""> <#-- TODO: set this to something appropriate based on whether link, submit, etc. (but markup doesn't currently use)... -->
  <#local htmlwrap = styles["menu_" + menuInfo.styleName + "_item_htmlwrap"]!styles["menu_default_item_htmlwrap"]!true>
  <#if htmlwrap?is_boolean>
    <#local htmlwrap = htmlwrap?string("li", "")>
  </#if>
  <#local attribs = {}>
  <#if toolTip?has_content>
    <#local attribs = attribs + {"title":toolTip}>
  </#if>
  <#local menuStyleName = menuInfo.styleName>

  <#if disabled>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_itemdisabled"]!styles["menu_default_itemdisabled"]!""))>
  </#if>

  <#local active = selected || selectedAncestor>
  <#local activeTarget = selected>
  <#local class = menuAppendActiveStyle(class, menuStyleName, "_itemactive", active, activeTarget)>

  <#local class = addClassArgDefault(class, styles["menu_" + menuStyleName + "_item"]!styles["menu_default_item"]!"")>

  <#-- NOTE: our "selected" actually means "active" to the Scipio macros -->
  <@menuitem_markup type=type menuType=menuInfo.type!"" menuSpecialType=menuInfo.specialType!"" class=class id=id 
      style="" attribs=attribs excludeAttribs=["class", "id", "style"] inlineItem=false htmlwrap=htmlwrap 
      disabled=disabled active=active activeTarget=activeTarget
      isNestedMenu=menuInfo.isNestedMenu menuLevel=menuInfo.menuLevel parentMenuType=menuInfo.parentMenuType parentMenuSpecialType=menuInfo.parentMenuSpecialType itemIndex=itemIndex><#rt>
    <#if linkArgs?has_content>
      <@renderLink linkUrl=linkArgs.linkUrl parameterList=linkArgs.parameterList targetWindow=linkArgs.targetWindow 
          uniqueItemName=linkArgs.uniqueItemName actionUrl=linkArgs.actionUrl linkType=linkArgs.linkType id=linkArgs.id 
          style=linkArgs.style name=linkArgs.name height=linkArgs.height width=linkArgs.width text=linkArgs.text imgArgs=(linkArgs.imgArgs!{}) imgStr=(linkArgs.imgStr!"")
          menuCtxRole=linkArgs.menuCtxRole disabled=linkArgs.disabled selected=linkArgs.selected selectedAncestor=linkArgs.selectedAncestor itemIndex=itemIndex menuInfo=menuInfo/><#t>
    <#elseif linkStr?has_content>
      <#-- 2016-09-12: we can now reasonably assume that in this case we intended to render a text string, so delegate this to @renderLink too, which should render it as text-only entry -->
      <#--${linkStr}-->
      <@renderLink linkUrl="" parameterList="" targetWindow=""
          uniqueItemName="" actionUrl="" linkType=linkArgs.linkType id="" 
          style="" name="" height="" width="" text=linkStr imgArgs={} imgStr=""
          menuCtxRole=menuCtxRole disabled=disabled selected=selected selectedAncestor=selectedAncestor itemIndex=itemIndex menuInfo=menuInfo/><#t>
    </#if><#t>
    <#if containsNestedMenus>
      <#-- NEW IN SCIPIO: Use recursion to render sub-menu... must be careful... -->
      <#-- NOTE (2016-08-26): Now using explicit submenu list as opposed to implicit sub-items -->
      <#list subMenuList as subMenu>
        <@renderMenuFull boundaryComment="" id=subMenu.id style=subMenu.style title=subMenu.title inlineEntries=false menuCtxRole=menuInfo.menuCtxRole 
            items=subMenu.items selected=subMenu.selected selectedAncestor=subMenu.selectedAncestor/>
      </#list>
      <#-- Previous code (manual, no recursion, unmaintained)...
      <#if menuInfo.htmlwrap?has_content><${menuInfo.htmlwrap}<@compiledClassAttribStr class=subMenuStyle />></#if>
      <#list items as item>
        <@renderMenuItemFull style=item.style toolTip=item.toolTip linkArgs=item.linkArgs!{} linkStr=item.linkStr!"" 
            containsNestedMenus=item.containsNestedMenus menuCtxRole=item.menuCtxRole items=item.items![] 
            subMenuId=item.subMenuId subMenuStyle=item.subMenuStyle subMenuTitle=item.subMenuTitle menuInfo=menuInfo />
      </#list>
      <#if menuInfo.htmlwrap?has_content></${menuInfo.htmlwrap}></#if>-->
    </#if>
  </@menuitem_markup>
</#macro>


<#-- 
*************************************
* SCIPIO: TRADITIONAL MENU MACROS *
*************************************
Only those not marked DEPRECATED should still be used.
-->

<#macro renderImage src id style width height border menuCtxRole="" extraArgs...>
  <img src="${escapeFullUrl(src, 'html')}"<#if id?has_content> id="${escapePart(id, 'html')}"</#if><#if style?has_content> class="${escapePart(style, 'html')}"</#if><#if width?has_content> width="${width}"</#if><#if height?has_content> height="${height}"</#if><#if border?has_content> border="${escapePart(border, 'html')}"</#if> />
</#macro>

<#-- SCIPIO: Highly modified @renderLink call, delegates markup to @menuitem_xxx_markup macros and images to @renderImage -->
<#macro renderLink linkUrl parameterList targetWindow uniqueItemName actionUrl linkType="" id="" style="" name="" height="" width="" text="" imgStr="" menuCtxRole="" imgArgs={} disabled=false selected=false selectedAncestor=false itemIndex=0 menuInfo={} extraArgs...>
  <#local class = style>
  <#local isLink = (linkType == "hidden-form" || linkUrl?has_content)>
  <#local hasImg = imgArgs?has_content || imgStr?has_content>
  <#local isText = !isLink && !hasImg && text?has_content>
  <#-- isLink: ${isLink?string} hasImg: ${hasImg?string} isText: ${isText?string} -->
  <#-- SCIPIO: hack: for screenlet nav menus, always impose buttons if no style specified, 
       because can't centralize these menus easily anywhere else. -->
  <#if menuCtxRole == "screenlet-nav-menu">
    <#if !class?has_content && isLink>
      <#local class = "${styles.menu_section_item_link!}">
    </#if>
  </#if>
  <#if linkType?has_content && "hidden-form" == linkType>
    <#local hiddenFormContent>
      <form method="post" action="${escapeFullUrl(actionUrl, 'html')}"<#if targetWindow?has_content> target="${escapePart(targetWindow, 'html')}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${escapePart(uniqueItemName, 'html')}" class="menu-widget-action-form"><#t>
        <#list parameterList as parameter>
          <input name="${escapePart(parameter.name, 'html')}" value="${escapePart(parameter.value, 'html')}" type="hidden"/><#t>
        </#list>
      </form><#t>
    </#local>
    <#local renderMenuHiddenFormContent = getRequestVar("renderMenuHiddenFormContent")!"">
    <#local dummy = setRequestVar("renderMenuHiddenFormContent", renderMenuHiddenFormContent+hiddenFormContent)>
  </#if>
  <#local innerContent> <#-- SCIPIO: WARN: this capture is only safe because nested sub-menus are outside link (outside this call) -->
    <#if imgArgs?has_content>
      <@renderImage src=imgArgs.src id=imgArgs.id style=imgArgs.style width=imgArgs.width height=imgArgs.height 
          border=imgArgs.border menuCtxRole=imgArgs.menuCtxRole /><#t>
    <#elseif imgStr?has_content>
      ${imgStr}<#t>
    </#if>
    <#if text?has_content>
      ${escapePart(text, 'htmlmarkup')}<#t>
    </#if>
  </#local>

  <#local menuStyleName = menuInfo.styleName>
  <#if disabled>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_item_contentdisabled"]!styles["menu_default_item_contentdisabled"]!""))>
  </#if>
  
  <#if isLink>
    <#if linkType == "hidden-form">
        <#-- SCIPIO: NOTE: only JS escaped here; macro markup does the html part -->
        <#local href>javascript:document['${escapePart(uniqueItemName, 'js')}'].submit()</#local>
    <#else>
        <#local href = linkUrl>
    </#if>
    <#if disabled>
      <#-- FIXME: this static method of disabling links means the link loses information and not easily toggleable! -->
      <#local href = styles.menu_link_href_default!"">
    </#if>

    <#local active = selected || selectedAncestor>
    <#local activeTarget = selected>
    <#local class = menuAppendActiveStyle(class, menuStyleName, "_item_contentactive", active, activeTarget)>

    <#local class = addClassArgDefault(class, styles["menu_" + menuStyleName + "_item_link"]!styles["menu_default_item_link"]!"")>
    <@menuitem_link_markup class=class id=id style="" name=name href=href onClick="" target=targetWindow title="" menuLevel=menuInfo.menuLevel
        attribs={} excludeAttribs=[] disabled=disabled active=active activeTarget=activeTarget itemIndex=itemIndex>${innerContent}</@menuitem_link_markup><#t>
  <#elseif isText>
    <#local class = addClassArgDefault(class, styles["menu_" + menuStyleName + "_item_text"]!styles["menu_default_item_text"]!"")>
    <@menuitem_text_markup class=class id=id style="" onClick="" title="" menuLevel=menuInfo.menuLevel
        attribs={} excludeAttribs=[] disabled=disabled active=active activeTarget=activeTarget itemIndex=itemIndex>${innerContent}</@menuitem_text_markup><#t>
  <#else>
    <#local class = addClassArgDefault(class, styles["menu_" + menuStyleName + "_item_generic"]!styles["menu_default_item_generic"]!"")>
    <@menuitem_generic_markup class=class id=id style="" onClick="" title="" menuLevel=menuInfo.menuLevel
        attribs={} excludeAttribs=[] disabled=disabled active=active activeTarget=activeTarget itemIndex=itemIndex>${innerContent}</@menuitem_generic_markup><#t>
  </#if>
</#macro>

<#-- SCIPIO: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
<#macro renderMenuBegin boundaryComment="" id="" style="" title="" inlineEntries=false menuCtxRole="">
  <#local styleSet = splitStyleNamesToSet(style)>
  <#local remStyle = "">
<#if boundaryComment?has_content>
<!- ${boundaryComment} ->
</#if>
  <#local menuIdNum = getRequestVar("scipioMenuIdNum")!0>
  <#local menuIdNum = menuIdNum + 1 />
  <#local dummy = setRequestVar("scipioMenuIdNum", menuIdNum)>
  <#if !id?has_content>
    <#local id = "menu_" + menuIdNum>
  </#if>
  <#if !inlineEntries>
    <#local extraMenuAttribs = {}>
    <#if styleSet.contains("menu-main")>
      <#local remStyle = removeStyleNames(style, "menu-main")>
        <li class="${styles.menu_main_wrap!}"><a href="#" class="${styles.menu_main_item_link!}"
            <#if styles.framework?has_content && styles.framework == "bootstrap"> data-toggle="dropdown"</#if>>${title!}<#if styles.framework?has_content && styles.framework =="bootstrap"> <i class="fa fa-fw fa-caret-down"></i></#if></a>
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
            <#local dashboardLink><a href="<@ofbizUrl>main</@ofbizUrl>">${uiLabelMap.CommonDashboard!}</a></#local>
            <@renderMenuItemBegin style="${styles.menu_sidebar_itemdashboard!}" linkStr=(dashboardLink!) /><@renderMenuItemEnd/>->
  </#if>
   <#local dummy = pushRequestStack("renderMenuStack", {"style":style,"remStyle":remStyle,"id":id,"inlineEntires":inlineEntries})> <#- pushing info to stack, so that this can be used by subsequently -> 
</#macro>
-->

<#-- SCIPIO: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
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

<#-- SCIPIO: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
<#macro renderMenuItemBegin style toolTip="" linkStr="" containsNestedMenus=false menuCtxRole="">
        <li<#if style?has_content> class="${style}"</#if><#if toolTip?has_content> title="${toolTip}"</#if>><#if linkStr?has_content>${linkStr}</#if><#if containsNestedMenus><ul></#if><#rt/>
</#macro>-->

<#-- SCIPIO: DEPRECATED/unmaintained/obsolete, replaced by one-shot macros, kept for reference only
<#macro renderMenuItemEnd containsNestedMenus=false menuCtxRole="">
<#if containsNestedMenus></ul></#if></li>
</#macro>-->


<#-- 
*************************************
* SCIPIO: TRANSITION MENU MACROS *
*************************************
WARN: No longer maintained.
-->

<#-- SCIPIO: Delegating implementation of one shot menu - used as reference
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
