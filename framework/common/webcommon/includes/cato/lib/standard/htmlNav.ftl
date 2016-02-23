<#--
* 
* Navigation HTML template include, standard Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#-- 
*************
* Nav List
************
Since this is very foundation specific, this function may be dropped in future installations

  * Usage Example *  
    <@nav type="">
        <li>Text or <a href="#">Anchor</a></li>
    </@nav>
    
    Or:
    <@nav type="magellan">
        <@mli arrival="MyTargetAnchor">Text or <a href="#">Anchor</a></@mli>
    </@nav>
    
    <@heading attribs=makeMagTargetAttribMap("MyTargetAnchor") id="MyTargetAnchor">Grid</@heading>
                    
  * Parameters *
    type            = (inline|magellan|breadcrumbs) (default:inline)
    class           = Adds classes - please use "(small|medium|large)-block-grid-#"    
-->
<#assign nav_defaultArgs = {
  "type":"inline", "passArgs":{}
}>
<#macro nav args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.nav_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@nav_markup type=type origArgs=origArgs passArgs=passArgs><#nested></@nav_markup>
</#macro>

<#-- @nav main markup - theme override -->
<#macro nav_markup type="" origArgs={} passArgs={} catchArgs...>
  <#switch type>
    <#case "magellan">
      <div data-magellan-expedition="fixed">
        <dl class="sub-nav">
          <#nested>
        </dl>
      </div>
    <#break>
    <#case "breadcrumbs">
      <ul class="${styles.nav_breadcrumbs!}">
          <#nested>
      </ul>
    <#break>
    <#default>
      <ul class="${styles.list_inline!} ${styles.nav_subnav!}">
        <#nested>
      </ul>
    <#break>
  </#switch>
</#macro>

<#assign mli_defaultArgs = {
  "arrival":"", "passArgs":{}
}>
<#macro mli args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.mli_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@mli_markup arrival=arrival origArgs=origArgs passArgs=passArgs><#nested></@mli_markup>
</#macro>

<#-- @mli main markup - theme override -->
<#macro mli_markup arrival="" origArgs={} passArgs={} catchArgs...>
  <dd data-magellan-arrival="${arrival}"><#nested></dd>
</#macro>

<#function mtarget id>
  <#local returnValue="data-magellan-destination=\"${id}\""/>
  <#return returnValue>
</#function>

<#function makeMagTargetAttribMap id>
  <#return {"data-magellan-destination":id}>
</#function>

<#-- 
*************
* Menu
************
Menu macro, mainly intended for small inline menu definitions in templates, but can substitute for widget menu
definitions if needed.

It can be used in two forms:
  <#assign items = [{"type":"link", ...}, {"type":"link", ...}, ...]>
  <@menu ... items=items />
  OR
  <@menu ...>
    <@menuitem type="link" ... />
    <@menuitem type="link" ... />
    ...
  </@menu>
  
In the first, each hash of the items list represents a menu item with the exact same arguments as the @menuitem macro.
The first method gives the @menu macro more control over the items, and to delegate the definitions, while 
second is cleaner to express.

Note that both macros support arguments passed in a hash (or map) using the "args" argument, so the entire menu definition
can be delegated in infinite ways (even to data prep). The inline args have priority over the hash args, as would be expected.
                  
FIXME? doesn't survive screens.render (uses #globals only), but probably doesn't need to.      
    should use set/getRequestVar and/or stack.            
                    
  * Parameters *
    type            = menu type: [generic|section|section-inline|main|tab|subtab|button|...], default generic (but discouraged; prefer specific)
    inlineItems     = boolean, if true, generate only items, not menu container
    class           = menu class style, default based on menu type. 
                      can be boolean true/false or string, if string
                      starts with "+" the classes are in addition to defaults, otherwise replace defaults.
                      defaults are based on:
                      styles["menu_" + type?replace("-","_")], or if missing from hash, falls back to
                      styles["menu_default"]
                      NOTE: for this macro, the inline "class" args is now logically combined with the "class"
                          arg from the "args" map using the logic in combineClassArgs function, with
                          inline having priority.
    id              = menu id
    style           = legacy menu style (for <ul element)
    attribs         = hash of other menu attribs (for <ul element, especially those with dashes)
    items           = list of hashes, where each hash contains arguments representing a menu item,
                      same as @menuitem macro parameters.
                      alternatively, the items can be specified as nested content.
    preItems        = special-case list of hashes of items, added before items and #nested.
                      excluded from sorting.
                      templates should generally avoid use unless specific need, but may be used by other macros.
    postItems       = special-case list of hashes of items, added after items and #nested
                      excluded from sorting.
                      avoid use unless specific need; may be needed by cato menu handling.
                      templates should generally avoid use unless specific need, but may be used by other macros.
    sort,
    sortBy,
    sortDesc        = items sorting behavior; will only work if items are specified
                      through items list of hashes, currently does not apply to 
                      nested items. by default, sorts by text, or sortBy can specify a menu item arg to sort by.
                      normally case-insensitive.
    nestedFirst     = default false, if true, use nested items before items list, otherwise items list always first.
                      usually use only one of alternatives but versatile.
    htmlWrap        = wrapping HTML element (ul|div|span, default: ul)
    specialType     = [|button-dropdown]
                      DEV NOTE: each specialType could have its own styles hash menu_special_xxx entries
-->
<#assign menu_defaultArgs = {
  "type":"generic", "class":"", "inlineItems":false, "id":"", "style":"", "attribs":{},
  "items":true, "preItems":true, "postItems":true, "sort":false, "sortBy":"", "sortDesc":false,
  "nestedFirst":false, "title":"", "specialType":"", "mainButtonClass":"", "htmlWrap":true, "passArgs":{}
}>
<#macro menu args={} inlineArgs...>
  <#-- class arg needs special handling here to support extended "+" logic (mostly for section menu defs) -->
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: this MUST be called here (or through concatMaps) to handle .class key properly -->
  <#if inlineArgs?has_content && inlineArgs.class??> <#-- DEV NOTE: do not remove ?has_content check here -->
    <#local class = combineClassArgs(args.class!"", inlineArgs.class)>
  <#else>
    <#local class = args.class!"">
  </#if>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.menu_defaultArgs, {
    <#-- parameters: overrides -->
    "class" : class
  })>
  <#local dummy = localsPutAll(args)>
  <#local attribs = makeAttribMapFromArgMap(args)>
  <#local origArgs = args>

  <#local menuIdNum = getRequestVar("catoMenuIdNum")!0>
  <#local menuIdNum = menuIdNum + 1 />
  <#local dummy = setRequestVar("catoMenuIdNum", menuIdNum)>
  <#if !id?has_content>
    <#local id = "menu_" + menuIdNum> <#-- FIXME? is this name too general? -->
  </#if>

  <#local prevMenuInfo = catoCurrentMenuInfo!>
  <#local prevMenuItemIndex = catoCurrentMenuItemIndex!>
  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || (!(styles["menu_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>

  <#if htmlWrap?is_boolean && htmlWrap == false>
    <#local htmlWrap = "">
  <#elseif (htmlWrap?is_boolean && htmlWrap == true) || !htmlWrap?has_content>
    <#local htmlWrap = styles["menu_" + styleName + "_htmlwrap"]!styles["menu_default_htmlwrap"]!true>
    <#if htmlWrap?is_boolean>
      <#local htmlWrap = htmlWrap?string("ul", "")>
    </#if>
  </#if>

  <#local class = addClassArgDefault(class, styles["menu_" + styleName]!styles["menu_default"]!"")>

  <#if specialType?is_boolean && specialType == false>
    <#local specialType = "">
  <#else>
    <#local specialType = styles["menu_" + styleName + "_specialtype"]!"">
  </#if>
  <#local mainButtonClass = addClassArgDefault(mainButtonClass, styles["menu_" + styleName + "_mainbutton"]!"")>
  
  <#local menuInfo = {"type":type, "specialType":specialType, "styleName":styleName, 
    "inlineItems":inlineItems, "class":class, "id":id, "style":style, "attribs":attribs,
    "preItems":preItems, "postItems":postItems, "sort":sort, "sortBy":sortBy, "sortDesc":sortDesc, "nestedFirst":nestedFirst}>
  <#global catoCurrentMenuInfo = menuInfo>
  <#global catoCurrentMenuItemIndex = 0>
  
  <@menu_markup type=type specialType=specialType class=class id=id style=style attribs=attribs excludeAttribs=["class", "id", "style"] inlineItems=inlineItems htmlWrap=htmlWrap title=title mainButtonClass=mainButtonClass origArgs=origArgs passArgs=passArgs>
  <#if !(preItems?is_boolean && preItems == false)>
    <#if preItems?is_sequence>
      <#list preItems as item>
        <@menuitem args=item passArgs=passArgs />
      </#list>    
    </#if>
  </#if>
  <#if !(items?is_boolean && items == false)>
    <#if nestedFirst>
        <#nested>
    </#if>
    <#if items?is_sequence>
      <#if sort && (!sortBy?has_content)>
        <#local sortBy = "text">
      </#if>
      <#if sortBy?has_content>
        <#local items = items?sort_by(sortBy)>
        <#if sortDesc>
          <#local items = items?reverse>
        </#if>
      </#if>
      <#list items as item>
        <@menuitem args=item passArgs=passArgs/>
      </#list>
    </#if>
    <#if !nestedFirst>
        <#nested>
    </#if>
  </#if>
  <#if !(postItems?is_boolean && postItems == false)>
    <#if postItems?is_sequence>
      <#list postItems as item>
        <@menuitem args=item passArgs=passArgs/>
      </#list>
    </#if>
  </#if>
  </@menu_markup>

  <#global catoCurrentMenuInfo = prevMenuInfo>
  <#global catoCurrentMenuItemIndex = prevMenuItemIndex>
  <#global catoLastMenuInfo = menuInfo>
</#macro>

<#-- @menu container main markup - theme override 
    DEV NOTE: This is called directly from both @menu and widgets @renderMenuFull -->
<#macro menu_markup type="" specialType="" class="" id="" style="" attribs={} excludeAttribs=[] inlineItems=false mainButtonClass="" title="" htmlWrap="ul" origArgs={} passArgs={} catchArgs...>
  <#if !inlineItems && htmlWrap?has_content>
    <#-- NOTE: here we always test specialType and never type, so that many (custom) menu types may reuse the same 
        existing specialType special handling without having to modify this code -->
    <#if specialType == "main">
      <li class="${styles.menu_main_wrap!}"><a href="#" class="${styles.menu_main_item_link!}"
        <#if (styles.framework!"") == "bootstrap"> data-toggle="dropdown"</#if>>${title!}<#if (styles.framework!"") == "bootstrap"> <i class="fa fa-fw fa-caret-down"></i></#if></a>
    <#elseif specialType == "sidebar">
      <nav class="${styles.nav_sidenav!""}">
        <#-- FIXME: this "navigation" variable is way too generic name! is it even still valid? -->
        <#if navigation?has_content><h2>${navigation!}</h2></#if>
    <#elseif specialType == "button-dropdown">
      <button href="#" data-dropdown="${id}" aria-controls="${id}" aria-expanded="false" class="${mainButtonClass}">${title}</button><br>
      <#local attribs = attribs + {"data-dropdown-content":"true", "aria-hidden":"true"}>
    </#if>
    <#if htmlWrap?has_content><${htmlWrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
  </#if>
      <#nested>
  <#if !inlineItems && htmlWrap?has_content>
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
</#macro>

<#-- 
*************
* Menu Item
************
Menu item macro. Must ALWAYS be enclosed in a @menu macro (see @menu options if need to generate items only).
             
  * Parameters *
    type            = menu item (content) type: [generic|link|text|submit], default generic (but discouraged; prefer specific)
    class           = menu item class (for <li> element)
                      NOTE: for this macro, the inline "class" args is now logically combined with the "class"
                          arg from the "args" map using the logic in combineClassArgs function, with inline given priority.
    id              = menu item id
    style           = legacy menu item style (for <li> element)
    attribs         = other menu item attributes (for <li> element, especially those with dashes in names)
    contentClass    = menu item content class (for <a>, <span> or <input> element)
                      NOTE: for this macro, the inline "contentClass" args is now logically combined with the "contentClass"
                          arg from the "args" map using the logic in combineClassArgs function, with inline given priority.
    contentId       = menu item content id
    contentStyle    = legacy menu item content style
    contentName     = content name attrib (name="" on <a> link)
    contentAttribs  = other menu item content attributes (for <a>, <span> or <input> element, especially those with dashes in names)
    text            = text to use as content. for now ALWAYS use this argument to specify
                      text, not nested content.
                      TODO: clarify nested content usage (because may have nested menus?)
    href            = content link, for "link" type
                      Also supports ofbiz request URLs using the notation: ofbizUrl:// (see interpretRequestUri function)
    onClick         = onClick (for content elem)
    title           = logical title attribute of content (link)
    disabled        = whether disabled, default false
    selected        = whether selected or not, default false
    active          = whether active or not, default false
    nestedContent   = alternative to #nested content, so can be passed in @menu items hash list
    nestedMenu      = alternative to nestedContent and #nested content, is a hash of @menu attribs
                      for menu to use as sub-menu.
    wrapNested      = if true, nested content is wrapped in link or span element. default false (nested outside, following).
    nestedFirst     = if true, nested content comes before content elem. default false (comes after content elem/text).
    htmlWrap        = wrapping HTML element (li|span|div, default: li)
    inlineItem      = boolean, if true, generate only items, not menu container
-->
<#assign menuitem_defaultArgs = {
  "type":"generic", "class":"", "contentClass", "", "id":"", "style":"", "attribs":{},
  "contentId":"", "contentStyle":"", "contentName":"", "contentAttribs":"", "text":"", "href":true,
  "onClick":"", "disabled":false, "selected":false, "active":false, "target":"",
  "nestedContent":true, "nestedMenu":false, "wrapNested":false, "nestedFirst":false,
  "htmlWrap":true, "inlineItem":false, "passArgs":{}
}>
<#macro menuitem args={} inlineArgs...>
  <#-- class args need special handling here to support extended "+" logic (mostly for section menu defs) -->
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: this MUST be called here (or through concatMaps) to handle .class key properly -->
  <#if inlineArgs?has_content && inlineArgs.class??> <#-- DEV NOTE: do not remove ?has_content check here -->
    <#local class = combineClassArgs(args.class!"", inlineArgs.class)>
  <#else>
    <#local class = args.class!"">
  </#if>
  <#if inlineArgs?has_content && inlineArgs.contentClass??>
    <#local contentClass = combineClassArgs(args.contentClass!"", inlineArgs.contentClass)>
  <#else>
    <#local contentClass = args.contentClass!"">
  </#if>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.menuitem_defaultArgs, {
    <#-- parameters: overrides -->
    "class" : class,
    "contentClass" : contentClass
  })>
  <#local dummy = localsPutAll(args)>
  <#local attribs = makeAttribMapFromArgMap(args)>
  <#local origArgs = args>

  <#local menuType = (catoCurrentMenuInfo.type)!"">
  <#local menuSpecialType = (catoCurrentMenuInfo.specialType)!"">
  <#local menuStyleName = (catoCurrentMenuInfo.styleName)!"">
  
  <#if htmlWrap?is_boolean && htmlWrap == false>
    <#local htmlWrap = "">
  <#elseif (htmlWrap?is_boolean && htmlWrap == true) || !htmlWrap?has_content>
    <#local htmlWrap = styles["menu_" + menuStyleName + "_item_htmlwrap"]!styles["menu_default_item_htmlwrap"]!true>
    <#if htmlWrap?is_boolean>
      <#local htmlWrap = htmlWrap?string("li", "")>
    </#if>
  </#if>

  <#if disabled>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_itemdisabled"]!styles["menu_default_itemdisabled"]!""))>
    <#local contentClass = addClassArg(contentClass, (styles["menu_" + menuStyleName + "_item_contentdisabled"]!styles["menu_default_item_contentdisabled"]!""))>
    <#-- FIXME: this static method of disabling links means the link loses information and not easily toggleable -->
    <#local href = "javascript:void(0);">
  </#if>
  <#if selected>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_itemselected"]!styles["menu_default_itemselected"]!""))>
    <#local contentClass = addClassArg(contentClass, (styles["menu_" + menuStyleName + "_item_contentselected"]!styles["menu_default_item_contentselected"]!""))>
  </#if>
  <#if active>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_itemactive"]!styles["menu_default_itemactive"]!""))>
    <#local contentClass = addClassArg(contentClass, (styles["menu_" + menuStyleName + "_item_contentactive"]!styles["menu_default_item_contentactive"]!""))>
  </#if>

  <#local class = addClassArgDefault(class, styles["menu_" + menuStyleName + "_item"]!styles["menu_default_item"]!"")>

  <#if type == "link">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_link"]!styles["menu_default_item_link"]!"">
  <#elseif type == "text">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_text"]!styles["menu_default_item_text"]!"">
  <#elseif type == "submit">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_submit"]!styles["menu_default_item_submit"]!"">
  <#else>
    <#local defaultContentClass = "">
  </#if>
  <#local contentClass = addClassArgDefault(contentClass, defaultContentClass)>
  <#local specialType = "">

  <@menuitem_markup type=type menuType=menuType menuSpecialType=menuSpecialType class=class id=id style=style attribs=attribs excludeAttribs=["class", "id", "style"] inlineItem=inlineItem htmlWrap=htmlWrap disabled=disabled selected=selected active=active origArgs=origArgs passArgs=passArgs><#rt>
    <#if !nestedContent?is_boolean>
      <#-- use nestedContent -->
    <#elseif !nestedMenu?is_boolean>
      <#local nestedContent><@menu args=nestedMenu /></#local>
    <#else>
      <#local nestedContent><#nested></#local>
    </#if>
    <#t><#if !wrapNested && nestedFirst>${nestedContent}</#if>
    <#if type == "link">
      <#if !href?is_string>
        <#local href = "javascript:void(0);">
      </#if>
      <#local href = interpretRequestUri(href)>
      <#t><@menuitem_link_markup href=href onclick=onClick class=contentClass id=contentId style=contentStyle name=contentName attribs=contentAttribs excludeAttribs=["class","id","style","href","onclick","target","title"] target=target title=title disabled=disabled selected=selected active=active origArgs=origArgs passArgs=passArgs><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_link_markup>
    <#elseif type == "text">
      <#t><@menuitem_text_markup class=contentClass id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","onclick"] onClick=onClick disabled=disabled selected=selected active=active origArgs=origArgs passArgs=passArgs><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_text_markup>
    <#elseif type == "submit">
      <#t><#if wrapNested && nestedFirst>${nestedContent}</#if><@menuitem_submit_markup class=contentClass id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","value","onclick","disabled","type"] onClick=onClick disabled=disabled selected=selected active=active origArgs=origArgs passArgs=passArgs><#if text?has_content>${text}</#if></@menuitem_submit_markup><#if wrapNested && !nestedFirst> ${nestedContent}</#if>
    <#else>
      <#local hasContentInfo = (contentClass?has_content || contentId?has_content || contentStyle?has_content || contentAttribs?has_content || onClick?has_content)>
      <#t><@menuitem_generic_markup hasContentInfo=hasContentInfo class=contentClass id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","onclick"] onClick=onClick disabled=disabled selected=selected active=active origArgs=origArgs passArgs=passArgs><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_generic_markup>
    </#if>
    <#t><#if !wrapNested && !nestedFirst>${nestedContent}</#if>
  </@menuitem_markup><#lt>
  <#global catoCurrentMenuItemIndex = catoCurrentMenuItemIndex + 1>
</#macro>

<#-- @menuitem container markup - theme override 
  DEV NOTE: This is called directly from both @menuitem and widgets @renderMenuItemFull -->
<#macro menuitem_markup type="" menuType="" menuSpecialType="" class="" id="" style="" attribs={} excludeAttribs=[] inlineItem=false htmlWrap="li" disabled=false selected=false active=false origArgs={} passArgs={} catchArgs...>
  <#if !inlineItem && htmlWrap?has_content>
    <${htmlWrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if>><#rt>
  </#if>
      <#nested><#t>
  <#if !inlineItem && htmlWrap?has_content>
    </${htmlWrap}><#lt>
  </#if>
</#macro>

<#-- @menuitem type="link" markup - theme override -->
<#macro menuitem_link_markup class="" id="" style="" href="" name="" onClick="" target="" title="" attribs={} excludeAttribs=[] disabled=false selected=false active=false origArgs={} passArgs={} catchArgs...>
  <#t><a href="${href}"<#if onClick?has_content> onclick="${onClick}"</#if><@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if target?has_content> target="${target}"</#if><#if title?has_content> title="${title}"</#if>><#nested></a>
</#macro>

<#-- @menuitem type="text" markup - theme override -->
<#macro menuitem_text_markup class="" id="" style="" onClick="" attribs={} excludeAttribs=[] disabled=false selected=false active=false origArgs={} passArgs={} catchArgs...>
  <#t><span<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${onClick}"</#if>><#nested></span>
</#macro>

<#-- @menuitem type="submit" markup - theme override -->
<#macro menuitem_submit_markup class="" id="" style="" text="" onClick="" disabled=false attribs={} excludeAttribs=[] disabled=false selected=false active=false origArgs={} passArgs={} catchArgs...>
  <#t><button type="submit"<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${onClick}"</#if><#if disabled> disabled="disabled"</#if> /><#nested></button>
</#macro>

<#-- @menuitem type="generic" markup - theme override -->
<#macro menuitem_generic_markup hasContentInfo=false class="" id="" style="" onClick="" attribs={} excludeAttribs=[] disabled=false selected=false active=false origArgs={} passArgs={} catchArgs...>
  <#t><#if hasContentInfo><div<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${onClick}"</#if>></#if><#nested><#if hasContentInfo></div></#if>
</#macro>

<#-- 
*************
* Menu Markup Inline Check
************
Function that examines a string containing menu HTML markup and returns true if and only if
the menu items are inlined, i.e. without container. Occasionally macros need to check this,
notably for compatibility with Ofbiz screens.
By default, this checks if the first item is a <li> element. Themes that use a different
menu item element must override this and provide a proper check.
             
  * Parameters *
    menuContent   = string of HTML markup
-->
<#function isMenuMarkupItemsInline menuContent>
  <#return menuContent?matches(r'(\s*<!--((?!<!--).)*?-->\s*)*\s*<li(\s|>).*', 'rs')>
</#function>

<#-- 
*************
* Pagination
************
  * Usage Example *  
    <@paginate mode="single" ... />
    <@paginate mode="content">
      <@table type="data-list">
        ...
      </@table>
    </@paginate>            
                    
  * Parameters *
   mode            = [content|single], default single, but content preferred
                     content: decorates the nested content with one or more pagination menus (depending on layout, and layout can be centralized)
                     single: produces a single pagination menu (layout has no effect)
   type            = [default], default default, type of the pagination menu itself
                     default: default cato pagination menu
   layout          = [default|top|bottom|both], default default, type of layout, only meaningful for "content" mode
                     default: "pagination_layout" from styles hash, otherwise both
                     top: no more than one menu, always at top
                     bottom: no more than one menu, always at bottom
                     both: always two menus, top and bottom
   position        = [top|bottom|], default empty; optional position indicator, only makes sense in single mode.
                     if specified, it may lead to the pagination not rendering depending on resolved value of layout.
                     in content mode (preferred), this is handled automatically.
   noResultsMode   = [default|hide|disable], default default (default may depend on mode)
                     default: "pagination_noresultsmode" from styles hash, otherwise hide
                     hide: hide menu when no results
                     disable: disable but show controls when no results (TODO?: not implemented)
   enabled         = [true|false], default true; manual control to disable the entire macro, sometimes needed to work around FTL language.
                     for "content" mode, with false, will still render nested content (that is the purpose), but will never decorate.
   url             = Base Url to be used for pagination
   class           = css classes 
                     supports prefixes:
                       "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                       "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
   listSize        = size of the list in total
   viewIndex       = page currently displayed
   viewSize        = maximum number of items displayed. NOTE: this should be decided earlier in rendering (data prep)
                     and a valid value MUST be passed.
   forcePost       = Always use POST for non-ajax browsing (note: even if false, large requests are coerced to POST)
   paramStr        = Extra URL parameters in string format, escaped (param1=val1&amp;param2=val2)
   viewIndexFirst  = First viewIndex value number (0 or 1, only affects param values, not display)
   showCount       = If true show "Displaying..." count or string provided in countMsg; if false don't; empty string is let markup/theme decide
   alwaysShowCount = If true, show count even if other pagination controls are supposed to be omitted
   countMsg        = Custom message for count, optional; markup provides its own default or in styles hash
   lowCountMsg     = Alternate custom message for low counts, optional; markup provides its own or in styles hash
   paginateToggle  = if true, include a control to toggle pagination on/off 
                     (specify current state with paginateOn and tweak using paginateToggle* arguments)
   paginateOn      = indicates whether pagination is currently on or off
                     Can be used with paginateToggle to indicate current state, or set to false to prevent
                     pagination controls while still allowing some decorations (depending on styling).
                     NOTE: this is not the same as enabled control. paginateOn does not prevent macro from rendering.
   previousViewSize     = used if paginate state is off. if not specified, it will use a default from general.properties.
   paginateOffViewSize  = a viewSize value send when turning off pagination via toggle. default is specified in general.properties.
   viewSizeSelection  = default false, currently officially unsupported.
                        DEV NOTE: only here for testing purposes
   altParam           = Use viewIndex/viewSize as parameter names, instead of VIEW_INDEX / VIEW_SIZE
   viewIndexString/   = specific param names to use (default VIEW_INDEX / VIEW_SIZE / PAGING)
   viewSizeString/
   paginateToggleString
   paramPrefix        = prefix added to param names. some screens need "~".
                        NOTE: Does not affect paramStr - caller must handle.
   paramDelim         = default "&amp;". Some screens need "/".
                        NOTE: Does not affect paramStr - caller must handle.
-->
<#assign paginate_defaultArgs = {
  "mode":"single", "type":"default", "layout":"default", "noResultsMode":"default", "enabled":true, "url":"", "class":"", 
  "viewIndex":0, "listSize":0, "viewSize":-1, "prioViewSize":false, "altParam":false, 
  "forcePost":false, "paramStr":"", "viewIndexFirst":0, "showCount":"", "alwaysShowCount":"", "countMsg":"", "lowCountMsg":"",
  "paginateToggle":false, "paginateOn":"", "paginateToggleOnValue":"Y", "paginateToggleOffValue":"N", 
  "viewSizeSelection":"", "position":"", 
  "viewIndexString":"", "viewSizeString":"", "paginateToggleString":"", 
  "paramDelim":"", "paramPrefix":"",
  "previousViewSize":"", "paginateOffViewSize":"",
  "passArgs":{}
}>
<#macro paginate args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.paginate_defaultArgs)>
  <#local dummy = localsPutAll(args)>

  <#-- this is also checked in paginate_core, but avoid problems with parameters by checking again early. -->  
  <#if enabled?is_boolean && enabled == false>
    <#if mode != "single">
      <#nested>
    </#if>
  <#else>

    <#-- these errors apparently happen a lot, enforce here cause screens never catch, guarantee other checks work -->
    <#if (!viewSize?is_number)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was not a number type: " + viewSize!, "htmlUtilitiesPaginate")!>
      <#local viewSize = viewSize?number>
    </#if>
    <#local viewSize = viewSize?floor>
    <#if (viewSize <= 0)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was a positive number: " + viewSize!, "htmlUtilitiesPaginate")!>
      <#local viewSize = 1>
    </#if>  
    <#if (!viewIndex?is_number)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was not a number type: " + viewIndex!, "htmlUtilitiesPaginate")!>
      <#local viewIndex = viewIndex?number>
    </#if>
    <#local viewIndex = viewIndex?floor>
    
  
    <#if !paramDelim?has_content>
      <#local paramDelim = "&amp;">
    </#if>

    <#if !previousViewSize?has_content>
      <#local previousViewSize = getPropertyValue("general.properties", "record.paginate.defaultViewSize")!20>
    </#if>
    <#if !paginateOffViewSize?has_content>
      <#local paginateOffViewSize = getPropertyValue("general.properties", "record.paginate.disabled.defaultViewSize")!99999>
    </#if>

    <#if !paginateOn?has_content>
      <#local paginateOn = true>
      <#-- DEV NOTE: we could try to infer this from the passed view size as below, for automatic paging toggle support everywhere (with paginateToggle=true)... 
          the only thing missing would be previousViewSize, but global default is not bad...
          HOWEVER there is no point right now because the URLs are prepared before @paginate_core and the form widgets
          don't handle this case... in general @paginate[_core] is indirectly limited by form widget implementation.
      <#local paginateOn = (viewSize < paginateOffViewSize)> 
      <#local paginateToggle = true>-->
    </#if>
    
    <#local viewIndexLast = viewIndexFirst + ((listSize/viewSize)?ceiling-1)>
    <#if (viewIndexLast < viewIndexFirst)>
      <#local viewIndexLast = viewIndexFirst>
    </#if>
    <#if (viewIndex < viewIndexFirst) || (viewIndex > viewIndexLast)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was out of bounds: " + viewIndex, "htmlUtilitiesPaginate")!>
      <#if (viewIndex < viewIndexFirst)>
        <#local viewIndex = viewIndexFirst>
      <#else>
        <#local viewIndex = viewIndexLast>
      </#if>
    </#if>
    
    <#if paginateOn>
      <#local lowIndex = (viewIndex - viewIndexFirst) * viewSize/>
      <#local highIndex = ((viewIndex - viewIndexFirst) + 1) * viewSize/>
      <#if (listSize < highIndex)>
        <#local realHighIndex = listSize/>
      <#else>
        <#local realHighIndex = highIndex/>
      </#if>
    <#else>
      <#local lowIndex = 0>
      <#local highIndex = listSize>
      <#local realHighIndex = listSize/>
    </#if>

    <#if !viewIndexString?has_content>
      <#local viewIndexString = altParam?string("viewIndex", "VIEW_INDEX")>
    </#if>
    <#local viewIndexString = paramPrefix + viewIndexString>
    <#if !viewSizeString?has_content>
      <#local viewSizeString = altParam?string("viewSize", "VIEW_SIZE")>
    </#if>
    <#local viewSizeString = paramPrefix + viewSizeString>
    <#if !paginateToggleString?has_content>
      <#local paginateToggleString = altParam?string("paging", "PAGING")>
    </#if>
    <#local paginateToggleString = paramPrefix + paginateToggleString>

    <#if (viewIndexLast > (viewIndex))>
      <#local viewIndexNext = (viewIndex+1)>
    <#else>
      <#local viewIndexNext = viewIndex>
    </#if>
    <#if (viewIndex > viewIndexFirst)>
      <#local viewIndexPrevious = (viewIndex-1)>
    <#else>
      <#local viewIndexPrevious = viewIndex>
    </#if>
  
    <#local origUrl = url>
    <#local origParamStr = paramStr>
  
    <#-- SPECIAL CASE: if paramDelim=="/" and url contains ";" or "?" we must strip the non-dir params and reappend them later 
         WARN: we can ignore paramStr to simplify; assume caller followed his own conventions... -->
    <#local urlSuffix = "">
    <#if paramDelim?contains("/")>
      <#local url = stripParamStrFromUrl(url)>
      <#if (url?length < origUrl?length)>
        <#local urlSuffix = origUrl[url?length..]>
      </#if>
    </#if>

    <#local commonUrl = addParamDelimToUrl(url, paramDelim)>
    <#if paramStr?has_content>
      <#local commonUrl = commonUrl + trimParamStrDelims(paramStr, paramDelim) + paramDelim>
    </#if>
    
    <#local firstUrl = "">
    <#if (!firstUrl?has_content)>
      <#local firstUrl=commonUrl+"${viewSizeString}=${viewSize}${paramDelim}${viewIndexString}=${viewIndexFirst}"+urlSuffix/>
    </#if>
    <#local previousUrl = "">
    <#if (!previousUrl?has_content)>
      <#local previousUrl=commonUrl+"${viewSizeString}=${viewSize}${paramDelim}${viewIndexString}=${viewIndexPrevious}"+urlSuffix/>
    </#if>
    <#local nextUrl="">
    <#if (!nextUrl?has_content)>
      <#local nextUrl=commonUrl+"${viewSizeString}=${viewSize}${paramDelim}${viewIndexString}=${viewIndexNext}"+urlSuffix/>
    </#if>
    <#local lastUrl="">
    <#if (!lastUrl?has_content)>
      <#local lastUrl=commonUrl+"${viewSizeString}=${viewSize}${paramDelim}${viewIndexString}=${viewIndexLast}"+urlSuffix/>
    </#if>
    <#local selectUrl="">
    <#if (!selectUrl?has_content)>
      <#local selectUrl=commonUrl+"${viewSizeString}=${viewSize}${paramDelim}${viewIndexString}=_VIEWINDEXVALUE_"+urlSuffix/>
    </#if>
    <#local selectSizeUrl="">
    <#if (!selectSizeUrl?has_content)>
      <#local selectSizeUrl=commonUrl+"${viewSizeString}='+this.value+'${paramDelim}${viewIndexString}=${viewIndexFirst}"+urlSuffix/>
    </#if>
  
    <#local paginateOnUrl="">
    <#if (!paginateOnUrl?has_content)>
      <#local paginateOnUrl=commonUrl+"${viewSizeString}=${previousViewSize}${paramDelim}${viewIndexString}=${viewIndexFirst}${paramDelim}${paginateToggleString}=${paginateToggleOnValue}"+urlSuffix/>
    </#if>
    <#local paginateOffUrl="">
    <#if (!paginateOffUrl?has_content)>
      <#local paginateOffUrl=commonUrl+"${viewSizeString}=${paginateOffViewSize}${paramDelim}${viewIndexString}=${viewIndexFirst}${paramDelim}${paginateToggleString}=${paginateToggleOffValue}"+urlSuffix/>
    </#if>
    
    <#-- NOTE: javaScriptEnabled is a context var -->
    <#-- DEV NOTE: make sure all @paginate_core calls same (DO NOT use #local capture; risks duplicate IDs) -->
    <#if mode == "single">
      <@paginate_core ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateClass=class paginateFirstClass="${styles.pagination_item_first!}" viewIndex=viewIndex lowIndex=lowIndex highIndex=highIndex realHighIndex=realHighIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel="" paginatePreviousClass="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel="" pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl showCount=showCount alwaysShowCount=alwaysShowCount countMsg=countMsg lowCountMsg="" paginateNextClass="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel="" paginateLastClass="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel="" paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst enabled=enabled paginateToggle=paginateToggle paginateOn=paginateOn ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnClass="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffClass="" paginateOffLabel="" noResultsMode=noResultsMode viewSizeSelection=viewSizeSelection layout=layout position=position passArgs=passArgs/>
    <#else>
      <@paginate_core ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateClass=class paginateFirstClass="${styles.pagination_item_first!}" viewIndex=viewIndex lowIndex=lowIndex highIndex=highIndex realHighIndex=realHighIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel="" paginatePreviousClass="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel="" pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl showCount=showCount alwaysShowCount=alwaysShowCount countMsg=countMsg lowCountMsg="" paginateNextClass="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel="" paginateLastClass="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel="" paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst enabled=enabled paginateToggle=paginateToggle paginateOn=paginateOn ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnClass="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffClass="" paginateOffLabel="" noResultsMode=noResultsMode viewSizeSelection=viewSizeSelection layout=layout position="top" passArgs=passArgs/>
        <#nested>
      <@paginate_core ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateClass=class paginateFirstClass="${styles.pagination_item_first!}" viewIndex=viewIndex lowIndex=lowIndex highIndex=highIndex realHighIndex=realHighIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel="" paginatePreviousClass="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel="" pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl showCount=showCount alwaysShowCount=alwaysShowCount countMsg=countMsg lowCountMsg="" paginateNextClass="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel="" paginateLastClass="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel="" paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst enabled=enabled paginateToggle=paginateToggle paginateOn=paginateOn ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnClass="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffClass="" paginateOffLabel="" noResultsMode=noResultsMode viewSizeSelection=viewSizeSelection layout=layout position="bottom" passArgs=passArgs/>
    </#if>
  </#if>
</#macro>

<#-- Core implementation of @paginate. 
    More options than @paginate, but raw and less friendly interface; not meant for template use, but can be called from other macro implementations.
     
    Migrated from @renderNextPrev form widget macro.
     
  * Parameters *
    enabled         = this disables the whole macro
    paginate        = display hint, does not seem to mean guarantee data wasn't paginated
    forcePost       = if true, HTTP requests must be in HTTP POST (sometimes required, other times simply better)
    viewIndexFirst  = first index
    listItemsOnly   = only show core paginate items, no container
    paginateToggle  = if true, include a control to toggle pagination on or off
    paginateOn      = this tells if current state is on or off (but doesn't prevent whole macro)
    position        = "top", "bottom", or nothing if unknown. informs the macro and markup of how/where the menu is used.
-->
<#assign paginate_core_defaultArgs = {
  "paginateClass":"", "paginateFirstClass":"", "viewIndex":1, "lowIndex":0, "highIndex":0, "realHighIndex":-1, "listSize":0, "viewSize":1, 
  "ajaxEnabled":false, "javaScriptEnabled":false, "ajaxFirstUrl":"", "firstUrl":"", 
  "paginateFirstLabel":"", "paginatePreviousClass":"", "ajaxPreviousUrl":"", "previousUrl":"", "paginatePreviousLabel":"", 
  "pageLabel":"", "ajaxSelectUrl":"", "selectUrl":"", "ajaxSelectSizeUrl":"", "selectSizeUrl":"", "showCount":"", "alwaysShowCount":"", "countMsg":"", "lowCountMsg":"",
  "paginateNextClass":"", "ajaxNextUrl":"", "nextUrl":"", "paginateNextLabel":"", "paginateLastClass":"", "ajaxLastUrl":"", 
  "lastUrl":"", "paginateLastLabel":"", "paginateViewSizeLabel":"", 
  "enabled":true, "forcePost":false, "viewIndexFirst":0, "listItemsOnly":false, "paginateToggle":false, "paginateOn":true, "ajaxPaginateOnUrl":"", 
  "paginateOnUrl":"", "paginateOnClass":"", "paginateOnLabel":"", "ajaxPaginateOffUrl":"", "paginateOffUrl":"", "paginateOffClass":"", 
  "paginateOffLabel":"", "noResultsMode":"default", "layout":"", "position":"", 
  "viewSizeSelection":"", "passArgs":{}
}>
<#macro paginate_core args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.paginate_core_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  
  <#-- some code doesn't pass realHighIndex... try to use highIndex, but this is not guaranteed to work -->
  <#if !realHighIndex?has_content || (realHighIndex < 0)>
    <#local realHighIndex = highIndex>
  </#if>
  
  <#if !noResultsMode?has_content || noResultsMode == "default">
    <#local noResultsMode = styles.pagination_noresultsmode!"hide">
  </#if>
  <#if noResultsMode == "hide" && (listSize <= 0)>
    <#-- force disabled -->
    <#local enabled = false>
  </#if>

  <#if !viewSizeSelection?has_content>
    <#local viewSizeSelection = false>
  </#if>

  <#-- check if layout allows our position 
      DEV NOTE: doing this here instead of @paginate allows the filtering to work in more situations,
          and is ok for this simple layout case. -->
  <#if !layout?has_content || layout == "default">
    <#local layout = styles.pagination_layout!"both">
  </#if>
  <#-- only filter if position is specified -->
  <#if position?has_content>
    <#if layout == "top" && position != "top">
      <#local enabled = false>
    <#elseif layout == "bottom" && position != "bottom">
      <#local enabled = false>
    </#if>
  </#if>
  
  <#-- note: possible that data was paginated even if enabled false, but don't bother right now. 
      seems pagination is hardcoded into a lot of ofbiz (so may be paginated even if form widget had paginate off). -->  
  <#if enabled>
  
    <#if viewSizeSelection>
      <#local availPageSizes = [10, 20, 30, 50, 100, 200]>
      <#local minPageSize = availPageSizes?first>
    <#else>
      <#local availPageSizes = [viewSize]>
      <#local minPageSize = viewSize>  
    </#if>
    <#local viewIndexLast = 0>
    <#local multiPage = false>
    
    <#-- these errors apparently happen a lot, enforce here cause screens never catch, guarantee other checks work -->
    <#if (!viewSize?is_number)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was not a number type: " + viewSize!, "htmlFormMacroLibraryRenderNextPrev")!><#t>
      <#local viewSize = viewSize?number>
    </#if>
    <#local viewSize = viewSize?floor>
    <#if (!viewIndex?is_number)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was not a number type: " + viewIndex!, "htmlFormMacroLibraryRenderNextPrev")!><#t>
      <#local viewIndex = viewIndex?number>
    </#if>
    <#local viewIndex = viewIndex?floor>
    
    <#local viewIndexLast = viewIndexFirst + ((listSize/viewSize)?ceiling-1)>
    <#if (viewIndexLast < viewIndexFirst)>
      <#local viewIndexLast = viewIndexFirst>
    </#if>
    <#if (viewIndex < viewIndexFirst) || (viewIndex > viewIndexLast)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was out of bounds: " + viewIndex, "htmlFormMacroLibraryRenderNextPrev")!><#t>
      <#if (viewIndex < viewIndexFirst)>
        <#local viewIndex = viewIndexFirst>
      <#else>
        <#local viewIndex = viewIndexLast>
      </#if>
    </#if>
    <#local multiPage = (listSize > viewSize)>
    
    <#-- Fix up ajaxSelectUrl here so doesn't affect other render types (?) -->
    <#local ajaxSelectUrl = ajaxSelectUrl?replace("' + this.value + '", "' + '")>
    
    <#-- This is workaround for Ofbiz bug (?), passes URLs params unescaped, but only for some (?)... 
         unclear if should be fixed in java or FTL but safer/easier here... 
         java comments say intentional but unclear why (?) -->
    <#local ajaxFirstUrl = escapeUrlParamDelims(ajaxFirstUrl)>
    <#local firstUrl = escapeUrlParamDelims(firstUrl)>
    <#local ajaxPreviousUrl = escapeUrlParamDelims(ajaxPreviousUrl)>
    <#local previousUrl = escapeUrlParamDelims(previousUrl)>
    <#local ajaxSelectUrl = escapeUrlParamDelims(ajaxSelectUrl)>
    <#local selectUrl = escapeUrlParamDelims(selectUrl)>
    <#local ajaxSelectSizeUrl = escapeUrlParamDelims(ajaxSelectSizeUrl)>
    <#local selectSizeUrl = escapeUrlParamDelims(selectSizeUrl)>
    <#local ajaxNextUrl = escapeUrlParamDelims(ajaxNextUrl)>
    <#local nextUrl = escapeUrlParamDelims(nextUrl)>
    <#local ajaxLastUrl = escapeUrlParamDelims(ajaxLastUrl)>
    <#local lastUrl = escapeUrlParamDelims(lastUrl)>
    <#local ajaxPaginateOnUrl = escapeUrlParamDelims(ajaxPaginateOnUrl)>
    <#local paginateOnUrl = escapeUrlParamDelims(paginateOnUrl)>
    <#local ajaxPaginateOffUrl = escapeUrlParamDelims(ajaxPaginateOffUrl)>
    <#local paginateOffUrl = escapeUrlParamDelims(paginateOffUrl)>

    <#-- SPECIAL CASE: markup expects selectUrl to contain the value _VIEWINDEXVALUE_, but legacy ofbiz code
        may not set it. in that case, simply append, since it used to appen at the end. -->
    <#if !selectUrl?contains("_VIEWINDEXVALUE_")>
      <#local selectUrl = selectUrl + "_VIEWINDEXVALUE_">
    </#if>

    <#if alwaysShowCount?is_boolean && alwaysShowCount == true>
      <#local showCount = true>
    </#if>

    <@paginate_markup paginateClass=paginateClass paginateFirstClass=paginateFirstClass viewIndex=viewIndex lowIndex=lowIndex highIndex=highIndex realHighIndex=realHighIndex listSize=listSize viewSize=viewSize ajaxEnabled=ajaxEnabled javaScriptEnabled=javaScriptEnabled ajaxFirstUrl=ajaxFirstUrl firstUrl=firstUrl 
      paginateFirstLabel=paginateFirstLabel paginatePreviousClass=paginatePreviousClass ajaxPreviousUrl=ajaxPreviousUrl previousUrl=previousUrl paginatePreviousLabel=paginatePreviousLabel 
      pageLabel=pageLabel ajaxSelectUrl=ajaxSelectUrl selectUrl=selectUrl ajaxSelectSizeUrl=ajaxSelectSizeUrl selectSizeUrl=selectSizeUrl showCount=showCount alwaysShowCount=alwaysShowCount countMsg=countMsg lowCountMsg=lowCountMsg
      paginateNextClass=paginateNextClass ajaxNextUrl=ajaxNextUrl nextUrl=nextUrl paginateNextLabel=paginateNextLabel paginateLastClass=paginateLastClass ajaxLastUrl=ajaxLastUrl 
      lastUrl=lastUrl paginateLastLabel=paginateLastLabel paginateViewSizeLabel=paginateViewSizeLabel 
      forcePost=forcePost viewIndexFirst=viewIndexFirst listItemsOnly=listItemsOnly paginateToggle=paginateToggle paginateOn=paginateOn ajaxPaginateOnUrl=ajaxPaginateOnUrl 
      paginateOnUrl=paginateOnUrl paginateOnClass=paginateOnClass paginateOnLabel=paginateOnLabel ajaxPaginateOffUrl=ajaxPaginateOffUrl paginateOffUrl=paginateOffUrl paginateOffClass=paginateOffClass 
      paginateOffLabel=paginateOffLabel
      availPageSizes=availPageSizes minPageSize=minPageSize viewIndexLast=viewIndexLast multiPage=multiPage viewSizeSelection=viewSizeSelection position=position origArgs=origArgs passArgs=passArgs/>

  </#if>
</#macro>

<#-- @paginate main markup - theme override -->
<#macro paginate_markup paginateClass="" paginateFirstClass="" viewIndex=1 lowIndex=0 highIndex=0 realHighIndex=0 listSize=0 viewSize=1 
    ajaxEnabled=false javaScriptEnabled=false ajaxFirstUrl="" firstUrl="" 
    paginateFirstLabel="" paginatePreviousClass="" ajaxPreviousUrl="" previousUrl="" paginatePreviousLabel="" 
    pageLabel="" ajaxSelectUrl="" selectUrl="" ajaxSelectSizeUrl="" selectSizeUrl="" showCount="" alwaysShowCount="" countMsg="" lowCountMsg=""
    paginateNextClass="" ajaxNextUrl="" nextUrl="" paginateNextLabel="" paginateLastClass="" ajaxLastUrl="" 
    lastUrl="" paginateLastLabel="" paginateViewSizeLabel="" 
    forcePost=false viewIndexFirst=0 listItemsOnly=false paginateToggle=false paginateOn=true ajaxPaginateOnUrl="" 
    paginateOnUrl="" paginateOnClass="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl="" paginateOffClass="" 
    paginateOffLabel=""
    availPageSizes=[] minPageSize=1 viewIndexLast=1 multiPage=true viewSizeSelection=false position="" origArgs={} passArgs={} catchArgs...>
    
  <#local paginateClass = addClassArg(paginateClass, styles.pagination_wrap!)> 
  <#local paginateClass = addClassArgDefault(paginateClass, "nav-pager")>  
    
  <#-- DEV NOTE: you could force-disable toggling paginate like this (per-theme even), but not clear if wanted.
      NOTE: not possible to force-enable because every screen has to implement the toggle (and widgets don't?).
      DO NOT remove the actual toggle code.
  <#local paginateToggle = false>-->

  <#if !paginateFirstLabel?has_content>
    <#local paginateFirstLabel = uiLabelMap.CommonFirst>
  </#if>
  <#if !paginatePreviousLabel?has_content>
    <#local paginatePreviousLabel = uiLabelMap.CommonPrevious>
  </#if>
  <#if !paginateNextLabel?has_content>
    <#local paginateNextLabel = uiLabelMap.CommonNext>
  </#if>
  <#if !paginateLastLabel?has_content>
    <#local paginateLastLabel = uiLabelMap.CommonLast>
  </#if>

  <#if paginateToggle>
     <#if !paginateOffLabel?has_content>
       <#local paginateOffLabel = (uiLabelMap.CommonPagingOff)!"">  
     </#if>
     <#if !paginateOnLabel?has_content>
       <#local paginateOnLabel = (uiLabelMap.CommonPagingOn)!"">  
     </#if>
  </#if>

  <#if !alwaysShowCount?has_content>
    <#-- don't force count message by default -->
    <#local alwaysShowCount = styles.pagination_alwaysshowcount!false>
  </#if>
  <#if !showCount?has_content>
    <#-- show count message by default -->
    <#if alwaysShowCount>
      <#local showCount = true>
    <#else>
      <#local showCount = styles.pagination_showcount!true>
    </#if>
  </#if>
  <#if showCount && lowCountMsg?has_content && (listSize <= minPageSize)>
    <#local countMsg = lowCountMsg>
  </#if>
  <#if showCount && (!countMsg?has_content)>
    <#if (listSize > minPageSize)>
      <#local countMsgLabel = styles.pagination_countmsglabel!"CommonDisplayingShort">
    <#else>
      <#local countMsgLabel = styles.pagination_lowcountmsglabel!"CommonDisplayingShort">
    </#if>
    <#local messageMap = {"lowCount": lowIndex+1, "highCount": realHighIndex, "total": listSize}>
    <#local countMsg = Static["org.ofbiz.base.util.UtilProperties"].getMessage("CommonUiLabels", countMsgLabel, messageMap, locale)!"">
  </#if>

  <#-- note: (listSize > minPageSize) implies (listSize > 0); some cases this gets called with listSize zero -->
  <#if paginateOn && (listSize > minPageSize)>
    
      <#local itemRange = 2/>
      <#local placeHolder ="..."/>
    
      <#if !listItemsOnly>
        <div class="${styles.grid_row!}">

          <div class="${styles.grid_large!}2 ${styles.grid_cell!}"><#if showCount>${countMsg}</#if></div>
          <div class="${styles.grid_large!}8 ${styles.grid_cell!}">
            <div<@compiledClassAttribStr class=paginateClass />>
              <ul class="${styles.pagination_list!}">
      </#if>
  
    
            <#-- NOTE: must use submitPaginationPost JS function to force send as POST for some requests, because Ofbiz security feature prevents
                 GET params passed to controller service event when request is https="true".
                 note: submitPagination (new in stock Ofbiz 14) already sends as POST in some cases, but not based on controller.
                 FIXME: POST/forcePost currently only supported when js enabled (non-js need extra markup for a form, ugly),
                    currently non-js falls back to GET only, won't always work -->
  
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxFirstUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${firstUrl}')"</#if><#else>href="${firstUrl}"</#if></#local>
                <li class="${styles.pagination_item!} ${compileClassArg(paginateFirstClass)}<#if (viewIndex> viewIndexFirst)>"><a ${actionStr}>${paginateFirstLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginateFirstLabel}</span></#if></li>
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPreviousUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${previousUrl}')"</#if><#else>href="${previousUrl}"</#if></#local>
                <li class="${styles.pagination_item!} ${compileClassArg(paginatePreviousClass)}<#if (viewIndex> viewIndexFirst)>"><a ${actionStr}>${paginatePreviousLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginatePreviousLabel}</span></#if></li>
            <#local displayDots = true/>
            <#if (listSize > 0)> 
              <#local x=(listSize/viewSize)?ceiling>
                <#list 1..x as i>
                  <#local vi = viewIndexFirst + (i - 1)>
                  <#if (vi gte viewIndexFirst && vi lte viewIndexFirst+itemRange) || (vi gte viewIndex-itemRange && vi lte viewIndex+itemRange)>
                    <#local displayDots = true/>
                    <#if vi == viewIndex>
                      <li class="${styles.pagination_item!} ${styles.pagination_item_active!}"><a href="javascript:void(0)">${i}</a></li>
                    <#else>
                      <#local finalSelectUrl = selectUrl?replace("_VIEWINDEXVALUE_", vi)>
                      <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxSelectUrl}${vi}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${finalSelectUrl}')"</#if><#else>href="${finalSelectUrl}"</#if></#local>
                      <li><a ${actionStr}>${i}</a></li>
                    </#if>
                  <#else>
                  <#if displayDots><li>${placeHolder!}</li></#if>
                  <#local displayDots = false/>
                  </#if>
                </#list>
            </#if>
            
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxNextUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${nextUrl}')"</#if><#else>href="${nextUrl}"</#if></#local>
                <li class="${styles.pagination_item!} ${compileClassArg(paginateNextClass)}<#if (highIndex < listSize)>"><a ${actionStr}>${paginateNextLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginateNextLabel}</span></#if></li>
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxLastUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${lastUrl}')"</#if><#else>href="${lastUrl}"</#if></#local>
                <li class="${styles.pagination_item!} ${compileClassArg(paginateLastClass)}<#if (highIndex < listSize)>"><a ${actionStr}>${paginateLastLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginateLastLabel}</span></#if></li>         
  
      <#if !listItemsOnly>  
              </ul>
            </div>
          </div>
          <#if paginateToggle>
            <#local paginateToggleContent>
              <#-- NOTE: duplicated below -->
              <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPaginateOffUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${paginateOffUrl}')"</#if><#else>href="${paginateOffUrl}"</#if></#local>
              <#local paginateOffClass = addClassArg(paginateOffClass, styles.pagination_item!)>
              <span<@compiledClassAttribStr class=paginateOffClass />><a ${actionStr}>${paginateOffLabel}</a></span>       
            </#local>    
          </#if>
          <div class="${styles.grid_large!}2 ${styles.grid_cell!}">
            <#if javaScriptEnabled>
              <#if viewSizeSelection>
                <#local actionStr>onchange="<#if ajaxEnabled>ajaxUpdateAreas('${ajaxSelectSizeUrl}')<#else><#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${selectSizeUrl}')</#if>"</#local>
                <div class="${styles.grid_row!}">
                    <div class="${styles.grid_large!}6 ${styles.grid_cell!}">
                        <label>${paginateViewSizeLabel}</label>
                    </div>
                    <div class="${styles.grid_large!}6 ${styles.grid_cell!}">
                        <select name="pageSize" size="1" ${actionStr}><#rt/>    
                        <#local sufficientPs = false>
                        <#list availPageSizes as ps>
                           <#if !sufficientPs>
                              <option<#if viewSize == ps> selected="selected"</#if> value="${ps}">${ps}</option>
                              <#if (ps >= listSize)>
                                <#local sufficientPs = true>
                              </#if>
                            </#if>
                        </#list>
                        </select>
                    </div>
                </div>
              </#if>
                
              <#if paginateToggle>
                <div class="${styles.grid_row!}">
                    <div class="${styles.grid_large!}12 ${styles.grid_cell!} ${styles.text_right!}">
                        ${paginateToggleContent}
                    </div>
                </div>
              </#if>
            <#elseif paginateToggle>
                <div class="${styles.grid_row!}">
                    <div class="${styles.grid_large!}12 ${styles.grid_cell!} ${styles.text_right!}">
                        ${paginateToggleContent}
                    </div>
                </div>
            </#if>
          </div>
        </div>
      </#if>
  <#elseif paginateToggle>
    <#if !listItemsOnly>
      <div class="${styles.grid_row!}">
      <#if alwaysShowCount>
        <div class="${styles.grid_large!}2 ${styles.grid_cell!} ${styles.grid_end!}">${countMsg}</div>
        <div class="${styles.grid_large!}8 ${styles.grid_cell!}">&nbsp;</div>
        <div class="${styles.grid_large!}2 ${styles.grid_cell!}">
      <#else>
        <div class="${styles.grid_large!}10 ${styles.grid_cell!}">&nbsp;</div>
        <div class="${styles.grid_large!}2 ${styles.grid_cell!}">
      </#if>
          <div<@compiledClassAttribStr class=paginateClass />>
            <ul class="${styles.pagination_list!}">
    </#if>
            <#if !paginateOn>
              <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPaginateOnUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${paginateOnUrl}')"</#if><#else>href="${paginateOnUrl}"</#if></#local>
              <#local paginateOffClass = addClassArg(paginateOnClass, styles.pagination_item!)>
              <li<@compiledClassAttribStr class=paginateOnClass />><a ${actionStr}>${paginateOnLabel}</a></li>  
            <#else>
              <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPaginateOffUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${paginateOffUrl}')"</#if><#else>href="${paginateOffUrl}"</#if></#local>
              <#local paginateOffClass = addClassArg(paginateOffClass, styles.pagination_item!)>
              <li<@compiledClassAttribStr class=paginateOffClass />><a ${actionStr}>${paginateOffLabel}</a></li> 
            </#if>
    <#if !listItemsOnly>  
            </ul>
          </div>
        </div>
      </div>
    </#if>
  <#elseif alwaysShowCount>
      <#if !listItemsOnly>
        <div class="${styles.grid_row!}">
          <div class="${styles.grid_large!}12 ${styles.grid_cell!} ${styles.grid_end!}">${countMsg}</div>
        </div>
      </#if>
  </#if>
</#macro>    

<#-- 
*************
* Tree Menu
************
Render menu in a tree fashion way 

  * Usage Example *  
    <@treemenu type="">
        <li>Text or <a href="#">Anchor</a></li>
    </@treemenu>
    
    Or:
    <@treemenu type="magellan">
        <@treemenuitem arrival="MyTargetAnchor">Text or <a href="#">Anchor</a></@mli>
    </@treemenu>
                    
  * Parameters *
      treeMenuLibrary = (jsTree) (default:jsTree)
    inlineItems     = boolean, if true, generate only items, not menu container    
    id              = menu id    
    attribs         = hash of other tree menu attribs
    data            = if jstTree: list of JsTreeHelper$JsTreeDataItem objects, where each object contains fields representing a tree menu item
                      same as @treemenuitem macro parameters.
    settings        = if jsTree: 
                      alternatively, the items can be specified as nested content.        
-->
<#assign treemenu_defaultArgs = {
  "library":"jsTree", "data":{}, "settings": {}, "plugins": [], "inlineItems":false, "id":"",  "attribs":{}, "passArgs":{}
}>
<#macro treemenu args={} inlineArgs...>
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: this MUST be called here (or through concatMaps) to handle .class key properly -->  
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.menu_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local attribs = makeAttribMapFromArgMap(args)>  
  
  <#local treeMenuLibrary = library!"jsTree"/>
  
  <@treemenu_markup treeMenuLibrary=treeMenuLibrary treeMenuData=data treeMenuSettings=settings treeMenuPlugins=plugins id=id attribs=attribs excludeAttribs=["class", "id", "style"] origArgs=origArgs passArgs=passArgs>
        <#nested>
  </@treemenu_markup>
</#macro>

<#-- @treemenu main markup - theme override -->
<#macro treemenu_markup treeMenuLibrary="" treeMenuData={} treeMenuSettings={} treeMenuPlugins=[] id="" attribs={} excludeAttribs=[] origArgs={} passArgs={} catchArgs...>
    <#if treeMenuLibrary == "jsTree">
        ${Static["org.ofbiz.base.util.Debug"].log("id ====> " + id)}  
        ${Static["org.ofbiz.base.util.Debug"].log("data ====> " + treeMenuData)}
        <#local treeMenuDataJson><@objectAsScript lang="json" object=treeMenuData /></#local> 
        <#local nestedEvents><#nested></#local>

        <div id="${id!''}"></div>
        <script type="text/javascript"> 
            $(document).ready(function() {
               $("#${id!''}")
                ${nestedEvents?trim}
                .jstree({
                    "core" : {
                        "data" : ${treeMenuDataJson}
                        <#if treeMenuSettings?has_content>
                           , <@objectAsScript lang="json" object=treeMenuSettings wrap=false />
                        </#if>
                     }
                     
                     <#if treeMenuPlugins?has_content>
                        <#list treeMenuPlugins as plugin>
                            , "${plugin.pluginName()}" : <@objectAsScript lang="json" object=plugin />
                        </#list>
                        
                        , "plugins" : [
                            <#list treeMenuPlugins as plugin>
                                "${plugin.pluginName()}"                               
                                <#if treeMenuPlugins?last.pluginName() != plugin.pluginName()>, </#if> 
                            </#list>
                        ]
                     </#if>
                });
            });
        </script>
    </#if>
    
</#macro>

<#macro treemenu_event event="">
    <#if event?has_content>
        <#assign validEvents = Static["com.ilscipio.cato.helper.JsTreeHelper$JsTreeEvent"].VALID_EVENTS />        
        <#assign e = event?keep_before(Static["com.ilscipio.cato.helper.JsTreeHelper$JsTreeEvent"].JSTREE_EVENT) />
        ${Static["org.ofbiz.base.util.Debug"].log("e ====> " + e)}

        <#if validEvents?has_content && validEvents?seq_contains(e)>
            ${Static["org.ofbiz.base.util.Debug"].log("valid event")}            
            .on("${event}", function (e, data) {
				
				/*for(var propertyName in e) {
   					console.log("e propertyName =====> " + propertyName);
 
				}*/
				/*for(var propertyName in data.node) {
                    console.log("data node propertyName =====> " + propertyName);
                }*/
                console.log("data node ====> " + data.node.id);

                <#nested>
            })
        </#if>
    </#if>
</#macro>
