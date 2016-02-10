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
Must be enabled in catoWebapp.properties.

WARN: this is currently DUPLICATED from the traditional macros further below.
Until this is tested and sorted out, please apply changes to both copies.

TODO/FIXME:
* I don't see a good way to separate markup in these yet...
* expand renderLink/renderImage... maybe...
* the java is inefficient and implementation maybe not final (see OneShotMacro.java) - depends
  on how much end up using and where
-->

<#-- Cato: One-shot macro menu rendering
  Data structure (indented means member of): 
  items: List of maps, each entry corresponding to old @renderMenuItemBegin arguments
    (item)
      "linkArgs": Link args corresponding to @renderLink args. In some cases, this will be missing, and linkStr (string) will be present instead. must test for both cases. This is because some cases don't render an actual link but text instead (misleading ofbiz var name).
        "imgArgs": Image args corresponding to @renderImage args, for menu entries that are images instead of text.
      "items": List of sub-menu items. Same format as parent items. Will be non-empty if parent item.containsNestedMenus is true. NOTE: there's no dedicated submenu (<ul>) open/close element or map. Implementation decides how to handle and what to call recursively.
        (item)
          "items": This goes on recursively for nested menus...
-->
<#macro renderMenuFull boundaryComment="" id="" style="" title="" inlineEntries=false menuCtxRole="" items=[]>
  <#--<p><@objectAsScript lang="raw" object=items /></p>-->
  <#local styleSet = splitStyleNamesToSet(style)>
  <#local remStyle = "">
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
  <#if !inlineEntries>
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
    <#elseif styleSet.contains("menu-tab")>    
      <#local remStyle = removeStyleNames(style, "menu-tab")>
      <#local classes = joinStyleNames(styles.menu_tab!, remStyle)>
    <#elseif styleSet.contains("button-bar")>
      <#-- NOTE (2016-02-08): There should be no more "button-bar" style left in *Menus.xml... should all go through CommonButtonBarMenu (menu-button) or alternative base menu -->
      <#local remStyle = removeStyleNames(style, ["button-bar"])> <#-- ["button-bar", "no-clear"] -->
      <#-- right now translating button-bar menu-container-style here to avoid modifying all menu styles
           note: in stock, button-bar usually accompanied by one of: button-style-2, tab-bar; also found: no-clear (removed above) -->
      <#-- WARN: stock ofbiz usually applied styles to a containing div, 
           not sure should keep that behavior or not, but might not consistent with foundation styles? -->
      <#local classes = joinStyleNames(styles.menu_button!, remStyle)>
    <#else>
      <#-- all other cases -->
      <#-- WARN: stock ofbiz usually applied styles to a containing div, 
           not sure should keep that behavior or not, but might not consistent with foundation styles? -->
      <#local classes = joinStyleNames(styles.menu_default!, style)>
    </#if>
        <ul<#if id?has_content> id="${id}"</#if><#if classes?has_content> class="${classes}"</#if>>
            <#-- Hardcoded alternative that will always display a Dashboard link on top of the sidebar
            <#local dashboardLink><a href="<@ofbizUrl>/main</@ofbizUrl>">${uiLabelMap.CommonDashboard!}</a></#local>
            <@renderMenuItemBegin style="${styles.menu_sidebar_itemdashboard!}" linkStr=dashboardLink! /><@renderMenuItemEnd/>-->
  </#if>
   <#local dummy = pushRequestStack("renderMenuStack", {"style":style,"remStyle":remStyle,"id":id,"inlineEntires":inlineEntries})> <#-- pushing info to stack, so that this can be used by subsequently --> 
   
  <#list items as item>
    <@renderMenuItemFull style=item.style toolTip=item.toolTip linkArgs=item.linkArgs!{} linkStr=item.linkStr!"" containsNestedMenus=item.containsNestedMenus menuCtxRole=item.menuCtxRole items=item.items![]/>
  </#list>

  <#local styleSet = splitStyleNamesToSet(style)>
  <#local menu = popRequestStack("renderMenuStack")>
  <#if !inlineEntries>
    <#--        
    <#if isSubMenu>
            </ul>
    <#else>
        </ul>
        </li>
        <#global isSubMenu=true/>
    </#if>
    -->
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
  
  <#if !readRequestStack("renderMenuStack")??> <#-- if top-level menu -->
    <#local renderMenuHiddenFormContent = getRequestVar("renderMenuHiddenFormContent")!"">
    <#if renderMenuHiddenFormContent?has_content>
      ${renderMenuHiddenFormContent}
      <#-- note: we don't have to worry about recursion here; will accumulate all forms from sub-menus as well;
           note: for simplicity, don't use xxxRequestStack for now, probably not needed -->
      <#local dummy = setRequestVar("renderMenuHiddenFormContent", "")>
    </#if>
  </#if>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#-- Cato: Render full menu item. Separate macro required due to recursive nested menus. 
    NOTE: if linkArgs empty, there may still be content in linkStr (that was not traditionally passed through a macro call), which is not necessarily a link! -->
<#macro renderMenuItemFull style="" toolTip="" linkArgs={} linkStr="" containsNestedMenus=false menuCtxRole="" items=[]>
  <#-- TODO? maybe want to expand the renderLink/renderImage calls -->
  <#if linkArgs?has_content>
    <#local imgStr = "">
    <#if linkArgs.imgArgs?has_content>
      <#local imgArgs = linkArgs.imgArgs>
      <#local imgStr><@renderImage imgArgs.src imgArgs.id imgArgs.style imgArgs.width imgArgs.height imgArgs.border imgArgs.menuCtxRole /></#local>
    </#if>
    <#local linkStr><@renderLink linkArgs.linkUrl linkArgs.parameterList linkArgs.targetWindow linkArgs.uniqueItemName linkArgs.actionUrl linkArgs.linkType linkArgs.id linkArgs.style linkArgs.name linkArgs.height linkArgs.width linkArgs.text imgStr linkArgs.menuCtxRole /></#local>
  </#if>
  <li<#if style?has_content> class="${style}"</#if><#if toolTip?has_content> title="${toolTip}"</#if>><#if linkStr?has_content>${linkStr}</#if><#rt>
    <#if containsNestedMenus>
      <ul>
      <#list items as item>
        <@renderMenuItemFull item.style item.toolTip item.linkArgs!{} item.linkStr!"" item.containsNestedMenus item.menuCtxRole item.items![] />
      </#list>
      </ul>
    </#if>
  </li><#lt>
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
-->

<#-- 
Menu styles can be set via menu-container-style attribute. The rendering will differ if one of the following classes is set
    * menu-main
    * menu-sidebar
    * menu-button
    * menu-tab // ToDo
-->
<#macro renderMenuBegin boundaryComment="" id="" style="" title="" inlineEntries=false menuCtxRole="">
  <#local styleSet = splitStyleNamesToSet(style)>
  <#local remStyle = "">
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
  <#if !inlineEntries>
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
    <#elseif styleSet.contains("menu-tab")>    
      <#local remStyle = removeStyleNames(style, "menu-tab")>
      <#local classes = joinStyleNames(styles.menu_tab!, remStyle)>
    <#elseif styleSet.contains("button-bar")>
      <#-- NOTE (2016-02-08): There should be no more "button-bar" style left in *Menus.xml... should all go through CommonButtonBarMenu (menu-button) or alternative base menu -->
      <#local remStyle = removeStyleNames(style, ["button-bar"])> <#-- ["button-bar", "no-clear"] -->
      <#-- right now translating button-bar menu-container-style here to avoid modifying all menu styles
           note: in stock, button-bar usually accompanied by one of: button-style-2, tab-bar; also found: no-clear (removed above) -->
      <#-- WARN: stock ofbiz usually applied styles to a containing div, 
           not sure should keep that behavior or not, but might not consistent with foundation styles? -->
      <#local classes = joinStyleNames(styles.menu_button!, remStyle)>
    <#else>
      <#-- all other cases -->
      <#-- WARN: stock ofbiz usually applied styles to a containing div, 
           not sure should keep that behavior or not, but might not consistent with foundation styles? -->
      <#local classes = joinStyleNames(styles.menu_default!, style)>
    </#if>
        <ul<#if id?has_content> id="${id}"</#if><#if classes?has_content> class="${classes}"</#if>>
            <#-- Hardcoded alternative that will always display a Dashboard link on top of the sidebar
            <#local dashboardLink><a href="<@ofbizUrl>/main</@ofbizUrl>">${uiLabelMap.CommonDashboard!}</a></#local>
            <@renderMenuItemBegin style="${styles.menu_sidebar_itemdashboard!}" linkStr=dashboardLink! /><@renderMenuItemEnd/>-->
  </#if>
   <#local dummy = pushRequestStack("renderMenuStack", {"style":style,"remStyle":remStyle,"id":id,"inlineEntires":inlineEntries})> <#-- pushing info to stack, so that this can be used by subsequently --> 
</#macro>

<#macro renderMenuEnd boundaryComment="" style="" inlineEntries=false menuCtxRole="">
  <#local styleSet = splitStyleNamesToSet(style)>
  <#local menu = popRequestStack("renderMenuStack")>
  <#if !inlineEntries>
    <#--        
    <#if isSubMenu>
            </ul>
    <#else>
        </ul>
        </li>
        <#global isSubMenu=true/>
    </#if>
    -->
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
  
  <#if !readRequestStack("renderMenuStack")??> <#-- if top-level menu -->
    <#local renderMenuHiddenFormContent = getRequestVar("renderMenuHiddenFormContent")!"">
    <#if renderMenuHiddenFormContent?has_content>
      ${renderMenuHiddenFormContent}
      <#-- note: we don't have to worry about recursion here; will accumulate all forms from sub-menus as well;
           note: for simplicity, don't use xxxRequestStack for now, probably not needed -->
      <#local dummy = setRequestVar("renderMenuHiddenFormContent", "")>
    </#if>
  </#if>
<#if boundaryComment?has_content>
<!-- ${boundaryComment} -->
</#if>
</#macro>

<#macro renderImage src id style width height border menuCtxRole="">
<img src="${src}"<#if id?has_content> id="${id}"</#if><#if style?has_content> class="${style}"</#if><#if width?has_content> width="${width}"</#if><#if height?has_content> height="${height}"</#if><#if border?has_content> border="${border}"</#if> />
</#macro>

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

<#macro renderMenuItemBegin style toolTip="" linkStr="" containsNestedMenus=false menuCtxRole="">
        <li<#if style?has_content> class="${style}"</#if><#if toolTip?has_content> title="${toolTip}"</#if>><#if linkStr?has_content>${linkStr}</#if><#if containsNestedMenus><ul></#if><#rt/>
</#macro>

<#macro renderMenuItemEnd containsNestedMenus=false menuCtxRole="">
<#if containsNestedMenus></ul></#if></li>
</#macro>
