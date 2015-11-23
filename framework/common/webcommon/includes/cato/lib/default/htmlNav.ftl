<#--
* 
* Navigation HTML template include, default Cato markup.
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
<#macro nav type="inline">
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

<#macro mli arrival="">
    <dd data-magellan-arrival="${arrival!}"><#nested></dd>
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
-->
<#macro menu args={} inlineArgs...>
  <#-- class arg needs special handling here to support extended "+" logic (mostly for section menu defs) -->
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: this MUST be called here (or through concatMaps) to handle .class key properly -->
  <#if inlineArgs?has_content && inlineArgs.class??> <#-- DEV NOTE: do not remove ?has_content check here -->
    <#local class = combineClassArgs(args.class!"", inlineArgs.class)>
  <#else>
    <#local class = args.class!"">
  </#if>
  <#local args = concatMaps(args, inlineArgs)> <#-- DEV NOTE: this takes care of case where inlineArgs is empty sequence (but NOT non-empty sequence) -->
  <#local type = args.type!"generic">
  <#local inlineItems = args.inlineItems!false>
  <#local id = args.id!"">
  <#local style = args.style!"">
  <#local attribs = args.attribs!"">
  <#local items = args.items!true>
  <#local preItems = args.preItems!true>
  <#local postItems = args.postItems!true>
  <#local sort = args.sort!false>
  <#local sortBy = args.sortBy!"">
  <#local sortDesc = args.sortDesc!false>
  <#local nestedFirst = args.nestedFirst!false>
  <#local htmlWrap  = args.htmlWrap!true/>

  <#if htmlWrap?is_boolean>
    <#local htmlWrap = htmlWrap?string("ul", "")>
  </#if>

  <#local prevMenuInfo = catoCurrentMenuInfo!>
  <#local prevMenuItemIndex = catoCurrentMenuItemIndex!>
  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || (!(styles["menu_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>
  <#local menuInfo = {"type":type, "styleName":styleName, 
    "inlineItems":inlineItems, "class":class, "id":id, "style":style, "attribs":attribs,
    "preItems":preItems, "postItems":postItems, "sort":sort, "sortBy":sortBy, "sortDesc":sortDesc, "nestedFirst":nestedFirst}>
  <#global catoCurrentMenuInfo = menuInfo>
  <#global catoCurrentMenuItemIndex = 0>

  <#local class = addClassArgDefault(class, styles["menu_" + styleName]!styles["menu_default"]!"")>

  <@menu_markup class=class id=id style=style attribs=attribs excludeAttribs=["class", "id", "style"] inlineItems=inlineItems htmlWrap=htmlWrap>
  <#if !(preItems?is_boolean && preItems == false)>
    <#if preItems?is_sequence>
      <#list preItems as item>
        <@menuitem args=item />
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
        <@menuitem args=item />
      </#list>
    </#if>
    <#if !nestedFirst>
        <#nested>
    </#if>
  </#if>
  <#if !(postItems?is_boolean && postItems == false)>
    <#if postItems?is_sequence>
      <#list postItems as item>
        <@menuitem args=item />
      </#list>
    </#if>
  </#if>
  </@menu_markup>

  <#global catoCurrentMenuInfo = prevMenuInfo>
  <#global catoCurrentMenuItemIndex = prevMenuItemIndex>
  <#global catoLastMenuInfo = menuInfo>
</#macro>

<#-- @menu container main markup - theme override -->
<#macro menu_markup class="" id="" style="" attribs={} excludeAttribs=[] inlineItems=false htmlWrap="ul" extraArgs...>
  <#if !inlineItems && htmlWrap?has_content>
    <${htmlWrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>>
  </#if>
      <#nested>
  <#if !inlineItems && htmlWrap?has_content>
    </${htmlWrap}>
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
  <#local args = concatMaps(args, inlineArgs)> <#-- DEV NOTE: this takes care of case where inlineArgs is empty sequence (but NOT non-empty sequence) -->
  <#local type = args.type!"generic">
  <#local id = args.id!"">
  <#local style = args.style!"">
  <#local attribs = args.attribs!"">
  <#local contentId = args.contentId!"">
  <#local contentStyle = args.contentStyle!"">
  <#local contentAttribs = args.contentAttribs!"">
  <#local text = args.text!"">
  <#local href = args.href!true>
  <#local onClick = args.onClick!"">
  <#local disabled = args.disabled!false>
  <#local selected = args.selected!false>
  <#local active = args.active!false>
  <#local target = args.target!"">
  <#local nestedContent = args.nestedContent!true>
  <#local nestedMenu = args.nestedMenu!false>
  <#local wrapNested = args.wrapNested!false>
  <#local nestedFirst = args.nestedFirst!false>
  <#local htmlWrap  = args.htmlWrap!true>
  <#local inlineItem = args.inlineItem!false>

  <#local menuType = (catoCurrentMenuInfo.type)!"">
  <#local menuStyleName = (catoCurrentMenuInfo.styleName)!"">
  
  <#if htmlWrap?is_boolean>
    <#local htmlWrap = htmlWrap?string("li", "")>
  </#if>

  <#if disabled>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_itemdisabled"]!styles["menu_default_itemdisabled"]!""))>
    <#local contentClass = addClassArg(contentClass, (styles["menu_" + menuStyleName + "_item_contentdisabled"]!styles["menu_default_item_contentdisabled"]!""))>
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

  <@menuitem_markup class=class id=id style=style attribs=attribs excludeAttribs=["class", "id", "style"] inlineItem=inlineItem htmlWrap=htmlWrap><#rt>
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
      <#t><@menuitem_link_markup href=href onclick=onClick class=contentClass id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","href","onclick","target","title"] target=target title=title><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_link_markup>
    <#elseif type == "text">
      <#t><@menuitem_text_markup class=contentClass id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","onclick"] onClick=onClick><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_text_markup>
    <#elseif type == "submit">
      <#t><#if wrapNested && nestedFirst>${nestedContent}</#if><@menuitem_submit_markup class=contentClass id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","value","onclick","disabled","type"] onClick=onClick disabled=disabled><#if text?has_content>${text}</#if></@menuitem_submit_markup><#if wrapNested && !nestedFirst> ${nestedContent}</#if>
    <#else>
      <#t><#if text?has_content>${text}</#if><#if wrapNested>${nestedContent}</#if>
    </#if>
    <#t><#if !wrapNested && !nestedFirst>${nestedContent}</#if>
  </@menuitem_markup><#lt>
  <#global catoCurrentMenuItemIndex = catoCurrentMenuItemIndex + 1>
</#macro>

<#-- @menuitem container markup - theme override -->
<#macro menuitem_markup class="" id="" style="" attribs={} excludeAttribs=[] inlineItem=false htmlWrap="li" extraArgs...>
  <#if !inlineItem && htmlWrap?has_content>
    <${htmlWrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if>><#rt>
  </#if>
      <#nested><#t>
  <#if !inlineItem && htmlWrap?has_content>
    </${htmlWrap}><#lt>
  </#if>
</#macro>

<#-- @menuitem type="link" markup - theme override -->
<#macro menuitem_link_markup class="" id="" style="" href="" onClick="" target="" title="" attribs={} excludeAttribs=[] extraArgs...>
  <#t><a href="${href}"<#if onClick?has_content> onclick="${onClick}"</#if><@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if target?has_content> target="${target}"</#if><#if title?has_content> title="${title}"</#if>><#nested></a>
</#macro>

<#-- @menuitem type="text" markup - theme override -->
<#macro menuitem_text_markup class="" id="" style="" onClick="" attribs={} excludeAttribs=[] extraArgs...>
  <#t><span<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${onClick}"</#if>><#nested></span>
</#macro>

<#-- @menuitem type="submit" markup - theme override -->
<#macro menuitem_submit_markup class="" id="" style="" text="" onClick="" disabled=false attribs={} excludeAttribs=[] extraArgs...>
  <#t><button type="submit"<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${onClick}"</#if><#if disabled> disabled="disabled"</#if> /><#nested></button>
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
      <@table>
        ...
      </@table>
    </@paginate>            
                    
  * Parameters *
   mode            = [single|content], default single
                     single: produces a single pagination menu
                     content: decorates the nested content with one or more pagination menus
   type            = [default], default default, type of the pagination menu itself
                     default: default cato pagination menu
   layout          = [default|top|bottom|both], default default, type of layout, only meaningful for "content" mode
                     default: cato default layout (currently "both")
                     top: no more than one menu, always at top
                     bottom: no more than one menu, always at bottom
                     both: always two menus, top and bottom
   noResultsMode   = [default|hide|disable], default default (default may depend on mode)
                     default: cato default (currently "hide", for both modes)
                     hide: hide menu when no results
                     disable: disable but show controls when no results (TODO?: not implemented)
   paginateOn      = indicates whether pagination is currently on or off (use with paginateToggle)
   url             = Base Url to be used for pagination
   class           = css classes 
                     supports prefixes:
                       "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                       "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
   listSize        = size of the list in total
   viewIndex       = page currently displayed
   viewSize        = maximum number of items displayed
   altParam        = Use viewIndex/viewSize as parameters, instead of VIEW_INDEX / VIEW_SIZE
   forcePost       = Always use POST for non-ajax browsing (note: even if false, large requests are coerced to POST)
   paramStr        = Extra URL parameters in string format, escaped (param1=val1&amp;param2=val2)
   viewIndexFirst  = First viewIndex value number (0 or 1, only affects param values, not display)
   showCount       = If true show "Displaying..." count or string provided in countMsg; if false don't
   countMsg        = Custom message for count, optional
   paginateToggle  = if true, include a control to toggle pagination on/off 
                     (specify current state with paginateOn and tweak using paginateToggle* arguments)
-->
<#macro paginate mode="single" type="default" layout="default" noResultsMode="default" paginateOn=true url="" class="" viewIndex=0 listSize=0 viewSize=1 altParam=false 
    forcePost=false paramStr="" viewIndexFirst=0 showCount=true countMsg=""
    paginateToggle=false paginateToggleString="" paginateToggleOnValue="Y" paginateToggleOffValue="N">

    <#-- these errors apparently happen a lot, enforce here cause screens never catch, guarantee other checks work -->
    <#if (!viewSize?is_number)>
        ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was not a number type: " + viewSize!, "htmlUtilitiesPaginate")!}<#t>
        <#local viewSize = viewSize?number>
    </#if>
    <#local viewSize = viewSize?floor>
    <#if (!viewIndex?is_number)>
        ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was not a number type: " + viewIndex!, "htmlUtilitiesPaginate")!}<#t>
        <#local viewIndex = viewIndex?number>
    </#if>
    <#local viewIndex = viewIndex?floor>
    
    <#local viewIndexLast = viewIndexFirst + ((listSize/viewSize)?ceiling-1)>
    <#if (viewIndexLast < viewIndexFirst)>
        <#local viewIndexLast = viewIndexFirst>
    </#if>
    <#if (viewIndex < viewIndexFirst) || (viewIndex > viewIndexLast)>
        ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was out of bounds: " + viewIndex, "htmlUtilitiesPaginate")!}<#t>
        <#if (viewIndex < viewIndexFirst)>
            <#local viewIndex = viewIndexFirst>
        <#else>
            <#local viewIndex = viewIndexLast>
        </#if>
    </#if>
    
    <#local lowIndex = (viewIndex - viewIndexFirst) * viewSize/>
    <#local highIndex = ((viewIndex - viewIndexFirst) + 1) * viewSize/>
    <#if (listSize < highIndex)>
        <#local highIndex = listSize/>
    </#if>
    <#if altParam>
        <#local viewIndexString = "viewIndex">
        <#local viewSizeString = "viewSize">
        <#if !paginateToggleString?has_content>
            <#local paginateToggleString = "paging">
        </#if>
    <#else>
        <#local viewIndexString = "VIEW_INDEX">
        <#local viewSizeString = "VIEW_SIZE">
        <#if !paginateToggleString?has_content>
            <#local paginateToggleString = "PAGING">
        </#if>
    </#if>
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

    <#local commonUrl = addParamDelimToUrl(url, "&amp;")>
    <#if paramStr?has_content>
        <#local commonUrl = commonUrl + trimParamStrDelims(paramStr) + "&amp;">
    </#if>
    
    <#local firstUrl = "">
    <#if (!firstUrl?has_content)>
        <#local firstUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexFirst}"/>
    </#if>
    <#local previousUrl = "">
    <#if (!previousUrl?has_content)>
         <#local previousUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexPrevious}"/>
    </#if>
    <#local nextUrl="">
    <#if (!nextUrl?has_content)>
        <#local nextUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexNext}"/>
    </#if>
    <#local lastUrl="">
    <#if (!lastUrl?has_content)>
        <#local lastUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndexLast}"/>
    </#if>
    <#local selectUrl="">
    <#if (!selectUrl?has_content)>
        <#local selectUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}="/>
    </#if>
    <#local selectSizeUrl="">
    <#if (!selectSizeUrl?has_content)>
        <#local selectSizeUrl=commonUrl+"${viewSizeString}='+this.value+'&amp;${viewIndexString}=${viewIndexFirst}"/>
    </#if>

    <#local paginateOnUrl="">
    <#if (!paginateOnUrl?has_content)>
        <#local paginateOnUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndex}&amp;${paginateToggleString}=${paginateToggleOnValue}"/>
    </#if>
    <#local paginateOffUrl="">
    <#if (!paginateOffUrl?has_content)>
        <#local paginateOffUrl=commonUrl+"${viewSizeString}=${viewSize}&amp;${viewIndexString}=${viewIndex}&amp;${paginateToggleString}=${paginateToggleOffValue}"/>
    </#if>
    
    <#if showCount && (!countMsg?has_content)>
       <#local messageMap = {"lowCount": lowIndex+1, "highCount": highIndex, "total": listSize}>
       <#local countMsg = Static["org.ofbiz.base.util.UtilProperties"].getMessage("CommonUiLabels", "CommonDisplaying", messageMap, locale)!"">
    </#if>
    
    <#-- TODO: noResultsMode should be delegated to @renderNextPrev... but generally hiding everything for now... -->
    <#if noResultsMode == "default">
        <#local noResultsMode = "hide">
    </#if>
    <#local showNextPrev = (noResultsMode != "hide") || (listSize > 0)>  
    
    <#-- NOTE: javaScriptEnabled is a context var -->
    <#-- DEV NOTE: make sure all @paginate_core calls same (DO NOT use #local capture; risks duplicate IDs) -->
    <#if mode == "single">
        <#if showNextPrev>
            <@paginate_core ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateClass=class paginateFirstClass="${styles.pagination_item_first!}" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousClass="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextClass="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastClass="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnClass="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffClass="" paginateOffLabel=uiLabelMap.CommonPagingOff />
        </#if>
    <#else>
        <#if showNextPrev && layout != "bottom">
            <@paginate_core ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateClass=class paginateFirstClass="${styles.pagination_item_first!}" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousClass="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextClass="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastClass="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnClass="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffClass="" paginateOffLabel=uiLabelMap.CommonPagingOff />
        </#if>
        <#nested>
        <#if showNextPrev && layout != "top">
            <@paginate_core ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateClass=class paginateFirstClass="${styles.pagination_item_first!}" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousClass="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextClass="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastClass="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnClass="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffClass="" paginateOffLabel=uiLabelMap.CommonPagingOff />
        </#if>
    </#if>
</#macro>

<#-- Core implementation of @paginate. 
    More options than @paginate, but raw and less friendly interface; not meant for template use, but can be called from other macro implementations.
     
    Migrated from @renderNextPrev form widget macro.
     
  * Parameters *
    paginate        = display hint, does not seem to mean guarantee data wasn't paginated
    forcePost       = if true, HTTP requests must be in HTTP POST (sometimes required, other times simply better)
    viewIndexFirst  = first index
    listItemsOnly   = only show core paginate items, no container
    paginateToggle  = if true, include a control to toggle pagination on or off
-->
<#macro paginate_core paginateClass="" paginateFirstClass="" viewIndex=1 highIndex=0 listSize=0 viewSize=1 
    ajaxEnabled=false javaScriptEnabled=false ajaxFirstUrl="" firstUrl="" 
    paginateFirstLabel="" paginatePreviousClass="" ajaxPreviousUrl="" previousUrl="" paginatePreviousLabel="" 
    pageLabel="" ajaxSelectUrl="" selectUrl="" ajaxSelectSizeUrl="" selectSizeUrl="" commonDisplaying="" 
    paginateNextClass="" ajaxNextUrl="" nextUrl="" paginateNextLabel="" paginateLastClass="" ajaxLastUrl="" 
    lastUrl="" paginateLastLabel="" paginateViewSizeLabel="" 
    paginate=true forcePost=false viewIndexFirst=0 listItemsOnly=false paginateToggle=false ajaxPaginateOnUrl="" 
    paginateOnUrl="" paginateOnClass="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl="" paginateOffClass="" 
    paginateOffLabel="">
    
  <#local availPageSizes = [10, 20, 30, 50, 100, 200]>
  <#local minPageSize = availPageSizes?first>
  <#local viewIndexLast = 0>
  <#local multiPage = false>
    
  <#-- note: possible that data was paginated even if paginate false, but don't bother right now -->
  <#if paginate>
    
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

    <@paginate_markup paginateClass=paginateClass paginateFirstClass=paginateFirstClass viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxEnabled=ajaxEnabled javaScriptEnabled=javaScriptEnabled ajaxFirstUrl=ajaxFirstUrl firstUrl=firstUrl 
      paginateFirstLabel=paginateFirstLabel paginatePreviousClass=paginatePreviousClass ajaxPreviousUrl=ajaxPreviousUrl previousUrl=previousUrl paginatePreviousLabel=paginatePreviousLabel 
      pageLabel=pageLabel ajaxSelectUrl=ajaxSelectUrl selectUrl=selectUrl ajaxSelectSizeUrl=ajaxSelectSizeUrl selectSizeUrl=selectSizeUrl commonDisplaying=commonDisplaying 
      paginateNextClass=paginateNextClass ajaxNextUrl=ajaxNextUrl nextUrl=nextUrl paginateNextLabel=paginateNextLabel paginateLastClass=paginateLastClass ajaxLastUrl=ajaxLastUrl 
      lastUrl=lastUrl paginateLastLabel=paginateLastLabel paginateViewSizeLabel=paginateViewSizeLabel 
      paginate=paginate forcePost=forcePost viewIndexFirst=viewIndexFirst listItemsOnly=listItemsOnly paginateToggle=paginateToggle ajaxPaginateOnUrl=ajaxPaginateOnUrl 
      paginateOnUrl=paginateOnUrl paginateOnClass=paginateOnClass paginateOnLabel=paginateOnLabel ajaxPaginateOffUrl=ajaxPaginateOffUrl paginateOffUrl=paginateOffUrl paginateOffClass=paginateOffClass 
      paginateOffLabel=paginateOffLabel
      availPageSizes=availPageSizes minPageSize=minPageSize viewIndexLast=viewIndexLast multiPage=multiPage />
  
  <#elseif paginateToggle>
  
    <#local ajaxPaginateOnUrl = escapeUrlParamDelims(ajaxPaginateOnUrl)>
    <#local paginateOnUrl = escapeUrlParamDelims(paginateOnUrl)>
  
    <@paginate_markup paginateClass=paginateClass paginateFirstClass=paginateFirstClass viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxEnabled=ajaxEnabled javaScriptEnabled=javaScriptEnabled ajaxFirstUrl=ajaxFirstUrl firstUrl=firstUrl 
      paginateFirstLabel=paginateFirstLabel paginatePreviousClass=paginatePreviousClass ajaxPreviousUrl=ajaxPreviousUrl previousUrl=previousUrl paginatePreviousLabel=paginatePreviousLabel 
      pageLabel=pageLabel ajaxSelectUrl=ajaxSelectUrl selectUrl=selectUrl ajaxSelectSizeUrl=ajaxSelectSizeUrl selectSizeUrl=selectSizeUrl commonDisplaying=commonDisplaying 
      paginateNextClass=paginateNextClass ajaxNextUrl=ajaxNextUrl nextUrl=nextUrl paginateNextLabel=paginateNextLabel paginateLastClass=paginateLastClass ajaxLastUrl=ajaxLastUrl 
      lastUrl=lastUrl paginateLastLabel=paginateLastLabel paginateViewSizeLabel=paginateViewSizeLabel 
      paginate=paginate forcePost=forcePost viewIndexFirst=viewIndexFirst listItemsOnly=listItemsOnly paginateToggle=paginateToggle ajaxPaginateOnUrl=ajaxPaginateOnUrl 
      paginateOnUrl=paginateOnUrl paginateOnClass=paginateOnClass paginateOnLabel=paginateOnLabel ajaxPaginateOffUrl=ajaxPaginateOffUrl paginateOffUrl=paginateOffUrl paginateOffClass=paginateOffClass 
      paginateOffLabel=paginateOffLabel
      availPageSizes=availPageSizes minPageSize=minPageSize viewIndexLast=viewIndexLast multiPage=multiPage />
  </#if>
</#macro>

<#-- @paginate main markup - theme override -->
<#macro paginate_markup paginateClass="" paginateFirstClass="" viewIndex=1 highIndex=0 listSize=0 viewSize=1 
    ajaxEnabled=false javaScriptEnabled=false ajaxFirstUrl="" firstUrl="" 
    paginateFirstLabel="" paginatePreviousClass="" ajaxPreviousUrl="" previousUrl="" paginatePreviousLabel="" 
    pageLabel="" ajaxSelectUrl="" selectUrl="" ajaxSelectSizeUrl="" selectSizeUrl="" commonDisplaying="" 
    paginateNextClass="" ajaxNextUrl="" nextUrl="" paginateNextLabel="" paginateLastClass="" ajaxLastUrl="" 
    lastUrl="" paginateLastLabel="" paginateViewSizeLabel="" 
    paginate=true forcePost=false viewIndexFirst=0 listItemsOnly=false paginateToggle=false ajaxPaginateOnUrl="" 
    paginateOnUrl="" paginateOnClass="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl="" paginateOffClass="" 
    paginateOffLabel=""
    availPageSizes=[] minPageSize=1 viewIndexLast=1 multiPage=true>
    
  <#local paginateClass = addClassArg(paginateClass, styles.pagination_wrap!)> 
  <#local paginateClass = addClassArgDefault(paginateClass, "nav-pager")>  
    
  <#-- note: possible that data was paginated even if paginate false, but don't bother right now -->
  <#if paginate>
    <#local itemRange = 2/>
    <#local placeHolder ="..."/>
    
    <#-- note: implies (listSize > 0), some cases this gets called with listSize zero -->
    <#if (listSize > minPageSize)>
    
      <#if !listItemsOnly>
        <div class="${styles.grid_row!}">
          <div class="${styles.grid_large!}2 ${styles.grid_cell!}">${commonDisplaying}</div>
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
                      <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxSelectUrl}${vi}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${selectUrl}${vi}')"</#if><#else>href="${selectUrl}${vi}"</#if></#local>
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
                <#local actionStr>onchange="<#if ajaxEnabled>ajaxUpdateAreas('${ajaxSelectSizeUrl}')<#else><#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${selectSizeUrl}')</#if>"</#local>
                <div class="${styles.grid_row!}">
                    <div class="${styles.grid_large!}6 ${styles.grid_cell!}">
                        <label>${paginateViewSizeLabel}</span>
                    </div>
                    <div class="${styles.grid_large!}6 ${styles.grid_cell!}">
                        <select name="pageSize" size="1" ${actionStr}><#rt/>    
                        <#local sufficientPs = false>
                        <#list availPageSizes as ps>
                           <#if !sufficientPs>
                              <option <#if viewSize == ps> selected="selected" </#if> value="${ps}">${ps}</option>
                              <#if (ps >= listSize)>
                                <#local sufficientPs = true>
                              </#if>
                            </#if>
                        </#list>
                        </select>
                    </div>
                </div>
                
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
    </#if>
  <#elseif paginateToggle>
    <#if !listItemsOnly>
      <div class="${styles.grid_row!}">
        <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
          <div<@compiledClassAttribStr class=paginateClass />>
            <ul class="${styles.pagination_list!}">
    </#if>
  
              <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPaginateOnUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${paginateOnUrl}')"</#if><#else>href="${paginateOnUrl}"</#if></#local>
              <#local paginateOffClass = addClassArg(paginateOnClass, styles.pagination_item!)>
              <li<@compiledClassAttribStr class=paginateOnClass />><a ${actionStr}>${paginateOnLabel}</a></li>  
  
    <#if !listItemsOnly>  
            </ul>
          </div>
        </div>
      </div>
    </#if>
  </#if>
</#macro>    

