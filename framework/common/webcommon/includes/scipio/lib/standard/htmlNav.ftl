<#--
* 
* Navigation & Menus
*
* Navigational elements that can be used to generate complexe menus, pagination controls or other 
* navigation elements (magelan, step elements, breadcrumbs or alike). The usage should be largely simplified
* by the elements provided. 
*
* Included by htmlTemplate.ftl.
*
* NOTES: 
* * May have implicit dependencies on other parts of Scipio API.
*
-->

<#-- 
*************
* Nav List
************
Creates a navigation list, for example based on magellan-destination or breadcrumbs.

[[[<img src="http://www.scipioerp.com/files/2016/05/breadcrumbs.png" alt=""/>]]]

Since this is very foundation specific, this function may be dropped in future installations

  * Usage Examples *  
  
    <@nav type="">
        <li>Text or <a href="#">Anchor</a></li>
    </@nav>
    
    OR
    
    <@nav type="magellan">
        <@mli arrival="MyTargetAnchor">Text or <a href="#">Anchor</a></@mli>
    </@nav>
    
    <@heading attribs=makeMagTargetAttribMap("MyTargetAnchor") id="MyTargetAnchor">Grid</@heading>
                    
  * Parameters *
    type                    = (inline|magellan|breadcrumbs|steps|, default: inline)
    class                   = ((css-class)) CSS classes
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)   
    id                      = ID
    style                   = Legacy HTML {{{style}}} attribute
    activeElem              = ((string)|(list)) Name of the active element or elements
                              The meaning and effect depends on the nav type.
-->
<#assign nav_defaultArgs = {
  "type":"inline", "id":"", "class":"", "style":"", "activeElem":"", "passArgs":{}
}>
<#macro nav args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.nav_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local navInfo = {"type":type, "id":id, "class":class, "style":style, "activeElem":activeElem, "passArgs":passArgs}>
  <#local dummy = setRequestVar("scipioNavInfo", navInfo)>
  <#local dummy = setRequestVar("scipioNavEntryIndex", 0)>
  <#local dummy = setRequestVar("scipioNavActiveElemIndex", -1)><#-- currently mainly for steps type -->
  <@nav_markup type=type id=id class=class style=style activeElem=activeElem origArgs=origArgs passArgs=passArgs><#nested></@nav_markup>
  <#local dummy = setRequestVar("scipioNavInfo", {})>
</#macro>

<#-- @nav main markup - theme override -->
<#macro nav_markup type="" id="" class="" style="" activeElem="" origArgs={} passArgs={} catchArgs...>
  <#switch type>
    <#case "magellan">
      <div data-magellan-expedition="fixed"<#if id?has_content> id="${escapeVal(id, 'html')}</#if><#if style?has_content> style="${escapeVal(style, 'html')}</#if>>
        <#local class = addClassArg(class, styles.nav_subnav!)>
        <dl<@compiledClassAttribStr class=class />>
          <#nested>
        </dl>
      </div>
    <#break>
    <#case "breadcrumbs">
      <#local class = addClassArg(class, styles.nav_breadcrumbs!)>
      <ul<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}</#if><#if style?has_content> style="${escapeVal(style, 'html')}</#if>>
        <#nested>
      </ul>
    <#break>
    <#case "steps">
      <#local class = addClassArg(class, styles.nav_steps!)>
      <ul<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}</#if><#if style?has_content> style="${escapeVal(style, 'html')}</#if>>
        <#nested>
      </ul>
    <#break>
    <#default>
      <#local class = addClassArg(class, styles.list_inline! + " " + styles.nav_subnav!)>
      <ul<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}</#if><#if style?has_content> style="${escapeVal(style, 'html')}</#if>>
        <#nested>
      </ul>
    <#break>
  </#switch>
</#macro>

<#-- 
*************
* mli
************
Creates a magellan-destination link.
-->
<#assign mli_defaultArgs = {
  "arrival":"", "passArgs":{}
}>
<#macro mli args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.mli_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@mli_markup arrival=arrival origArgs=origArgs passArgs=passArgs><#nested></@mli_markup>
</#macro>

<#-- @mli main markup - theme override -->
<#macro mli_markup arrival="" origArgs={} passArgs={} catchArgs...>
  <dd data-magellan-arrival="${escapeVal(arrival, 'html')}"><#nested></dd>
</#macro>

<#-- 
*************
* mtarget
************
Creates an magellan-destination attribute string.
-->
<#function mtarget id>
  <#local returnValue="data-magellan-destination=\"${escapeVal(id, 'html')}\""/>
  <#return returnValue>
</#function>

<#-- 
*************
* makeMagTargetAttribMap
************
Makes an attrib map container a magellan-destination attribute.
-->
<#function makeMagTargetAttribMap id>
  <#return {"data-magellan-destination":id}>
</#function>


<#-- 
*************
* step
************
Creates a single step - to be used with {{{<@nav type="steps" />}}}.

  * Parameters *
    name                    = Step name, for auto-matching purposes (optional if not using auto active matching)
    icon                    = Generates icon inside the step  
    disabled                = ((boolean)) step is disabled (override)
    active                  = ((boolean)) marks the current step (override)
    completed               = ((boolean)) step is completed (will override icon if icon is set)
    class                   = ((css-class)) CSS classes
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    href                    = link (if not disabled or active)
-->
<#assign step_defaultArgs = {
  "name":"", "icon":"", "completed":"", "disabled":"", "active":"", "class":"", "href":"", "passArgs":{}
}>
<#macro step args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.step_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local navInfo = getRequestVar("scipioNavInfo")!{}>
  <#local stepIndex = getRequestVar("scipioNavEntryIndex")!0>
  <#local activeStep = navInfo.activeElem!>
  <#local activeStepIndex = getRequestVar("scipioNavActiveElemIndex")!-1>
  
  <#if activeStep?has_content>
    <#if name == activeStep>
      <#local activeStepIndex = stepIndex>
      <#local dummy = setRequestVar("scipioNavActiveElemIndex", stepIndex)>
      <#if !active?is_boolean>
        <#local active = true>
      </#if>
    <#elseif (activeStepIndex < 0)>
      <#-- haven't reached active step yet; we assume it's somewhere down the line... -->
      <#if !completed?is_boolean>
        <#local completed = true>
      </#if>
    <#elseif (stepIndex > activeStepIndex)>
      <#if !disabled?is_boolean>
        <#local disabled = true>
      </#if> 
    <#else>
      <#-- this shouldn't happen?... just set to disabled in case -->
      <#if !disabled?is_boolean>
        <#local disabled = true>
      </#if> 
    </#if>
  </#if>
  
  <#if !completed?is_boolean>
    <#local completed = false>
  </#if>
  <#if !disabled?is_boolean>
    <#local disabled = false>
  </#if> 
  <#if !active?is_boolean>
    <#local active = false>
  </#if>

  <@step_markup class=class icon=icon completed=completed disabled=disabled active=active 
    href=href origArgs=origArgs passArgs=passArgs><#nested></@step_markup>
  <#local dummy = setRequestVar("scipioNavEntryIndex", stepIndex + 1)>
</#macro>

<#-- @step main markup - theme override -->
<#macro step_markup class="" icon="" completed=false disabled=false active=false href="" origArgs={} passArgs={} catchArgs...>
  <#local class = addClassArg(class, styles.nav_step!)>
  <#if active> 
    <#local class = addClassArg(class, styles.nav_step_active!)>
  </#if>
  <#if disabled> 
    <#local class = addClassArg(class, styles.nav_step_disabled!)>
  </#if>
  <li<@compiledClassAttribStr class=class />>
    <#local showLink = href?has_content && !disabled><#-- allow link to active for clean page refresh: && !active -->
    <#if showLink>
      <a href="${escapeFullUrl(href, 'html')}">
    </#if>
    <#if icon?has_content><i class="<#if completed>${styles.nav_step_completed!}<#else>${escapeVal(icon, 'html')}</#if>"></i></#if>
    <#nested>
    <#if showLink>
      </a>
    </#if>
  </li>
</#macro>

<#-- 
*************
* Menu
************
Menu macro, mainly intended for small inline menu definitions in templates, but able to substitute for widget menu
definitions if needed.

[[[<img src="http://www.scipioerp.com/files/2016/05/menu.png" alt=""/>]]]

It may be used in two forms:
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
          
The generated menu ID following a call can be read using: {{{getRequestVar("scipioLastMenuInfo").id}}}.
At current time (2016-09-16), other members of that map should not be relied upon.
          
* Nested Menus *

Nested menus (sub-menus) will inherit the type of the parent if no type is specified. The macro will automatically
try to determine if the menu is nested and the type of the parent, but these may be overridden so a nested
menu may behave as a top-level menu. Note the macro makes a distinction between sub-menus that are the same
type as the parent and sub-menus that are a different type as the parent, which may require different
handling.

The submenu's main class may be set as altnested in global styles. 
    
  * Parameters *
    type                    = (generic|section|section-inline|main|sidebar|tab|subtab|button|..., default: generic) The menu type
                              For nested menus, this will inherit the type of the parent.
                              General:
                              * {{{generic}}}: any content, but specific type should be preferred.
    name                    = (optional) Internal name
                              If present, may be used for logic and styling needs.
                              2017-02-17: now used to generate a class name in the form: "menu-name-${name}",
                              which can be used for styling purposes. Note that the name is not unique globally
                              and must be used in conjunction with more precise selectors.             
    inlineItems             = ((boolean), default: false) If true, generate only items, not menu container
    class                   = ((css-class), default: -based on menu type-) CSS classes for menu
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)  
                              defaults are based on:
                                styles["menu_" + type?replace("-","_")], or if missing from hash, falls back to
                                styles["menu_default"]
                              NOTE: for this macro, the inline "class" args is now logically combined with the "class"
                                  arg from the "args" map using the logic in combineClassArgs function, with
                                  inline having priority.
    id                      = Menu ID
    style                   = Legacy menu HTML style attribute (for <ul> element)
    attribs                 = ((maps)) Other menu attributes (for <ul> element)
    items                   = ((list)) List of maps, where each hash contains arguments representing a menu item,
                              Same as @menuitem macro parameters.
                              Alternatively, the items can be specified as nested content.
    preItems                = ((list)) Special-case list of maps of items, added before items and nested content
                              Excluded from sorting.
                              Templates should generally avoid use unless specific need, but may be used by other macros.
    postItems               = ((list)) Special-case list of maps of items, added after items and nested content
                              Excluded from sorting.
                              Avoid use unless specific need; may be needed by scipio menu handling.
                              Templates should generally avoid use unless specific need, but may be used by other macros.
    sort,
    sortBy,
    sortDesc                = Items sorting behavior; will only work if items are specified
                              through items list of hashes, currently does not apply to 
                              nested items. by default, sorts by text, or sortBy can specify a menu item arg to sort by.
                              normally case-insensitive.
    nestedFirst             = ((boolean), default: false) If true, use nested items before items list, otherwise items list always first
                              Usually should use only one of alternatives, but is versatile.
    htmlwrap                = (ul|div|span, default: ul)
    specialType             = (button-dropdown|, default: -none-)
                              DEV NOTE: each specialType could have its own styles hash menu_special_xxx entries
    isNestedMenu            = ((boolean)|, default: -empty, automatic-) Override to tell the macro if it's nested or not
                              The menu macro will try to figure out if nested or not on its own. In rare custom code, this boolean may need to be specified,
                              in case it is needed a nested menu behaves as a top-level menu (by passing false).
    menuLevel               = ((int), default: -empty, automatic-) Override to tell the macro which level it's on
                              The menu macro will try to figure out automatically if not specified.
                              Starts at 1.
    parentMenuType          = Manual override to tell macro what the parent menu type was
                              This is usually determined automatically, but in esoteric cases may need to specify.
    active                  = ((boolean), default: false) Indicates it is the parent menu of an active item, either target or ancestor
                              NOTE(2016-08-30): Is currently NOT determined automatically, only in menu widgets
                              NOTE: "active" translates to ("selected" OR "selected ancestor") in menu widget terminology (see CommonMenus.xml).
    activeTarget            = ((boolean)|, default: -empty/unspecified/unknown-) Indicates it is the parent menu of an active target but not ancestor menu item (current page)
                              If true, implies active. If explicit false, means active ancestor. If not set, unknown or unspecified by template.
                              NOTE(2016-09-01): currently this is NOT determined automatically except for menu widgets.
                              NOTE: "active target" translates to "selected" item in menu widget terminology (see CommonMenus.xml)
    title                   = Menu title (abstract)
                              This has a generic/abstract meaning and semantics meaning use will depend on
                              the specific menu type and implementation.
                              Currently mostly needed for {{{button-dropdown}}}.
    titleClass              = ((css-class), default: -based on menu type-) CSS classes for title (abstract)
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
                              NOTE: Previously this was named "mainButtonClass"; mainButtonClass is deprecated, and
                                  will get the same value as titleClass now.
    sepMenuType             = (default-sidebar|...|, default: -none-) Separate-menu type
                              These are the entries named "sepmenu_[sepMenuType]_config" (dashes replaced by underscores) in 
                              the global styles hash and are customizable.
                              Standard provided types:
                              * default-sidebar
    sepMenuDef              = ((map)) Separate-menu content for the core menu entries (only), represented as hash of @menu args
                              NOTE: not all of the @menu arguments are applicable
    itemClass               = ((css-class)) Extra CSS classes for every item of the immediate level
                                  
  * History *
    Enhanced for 1.14.3.
    Enhanced for 1.14.2.       
-->
<#assign menu_defaultArgs = {
  "type":"", "name":"", "class":"", "inlineItems":false, "id":"", "style":"", "attribs":{},
  "items":true, "preItems":true, "postItems":true, "sort":false, "sortBy":"", "sortDesc":false,
  "nestedFirst":false, "title":"", "specialType":"", "titleClass":"", "mainButtonClass":"", "htmlwrap":true, 
  "isNestedMenu":"", "parentMenuType":"", "active":"", "activeTarget":"", "menuLevel":"", 
  "sepMenuType":"", "sepMenuDef":{}, "itemClass":"", "passArgs":{}
}>
<#macro menu args={} inlineArgs...>
  <#-- class arg needs special handling here to support extended "+" logic (mostly for section menu defs) -->
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: this MUST be called here (or through concatMaps) to handle .class key properly -->
  <#if inlineArgs?has_content && inlineArgs.class??> <#-- DEV NOTE: do not remove ?has_content check here -->
    <#local class = combineClassArgs(args.class!"", inlineArgs.class)>
  <#else>
    <#local class = args.class!"">
  </#if>
  <#local explArgs = args>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.menu_defaultArgs, {
    <#-- parameters: overrides -->
    "class" : class
  })>
  <#local dummy = localsPutAll(args)>
  <#local attribs = makeAttribMapFromArgMap(args)>
  <#local origArgs = args>
  
  <#local menuIdNum = getRequestVar("scipioMenuIdNum")!0>
  <#local menuIdNum = menuIdNum + 1 />
  <#local dummy = setRequestVar("scipioMenuIdNum", menuIdNum)>
  <#if !id?has_content>
    <#local id = "menu_" + menuIdNum><#-- FIXME?: is this name too generic? -->
  </#if>

  <#local prevMenuInfo = readRequestStack("scipioMenuStack")!>
  <#local prevMenuItemIndex = getRequestVar("scipioCurrentMenuItemIndex")!>
  <#if !isNestedMenu?is_boolean>
    <#-- rudimentary check for parent menu -->
    <#local isNestedMenu = (prevMenuInfo.type)??>
  </#if>
  
<@renderTarget dirName=isNestedMenu?string("sub-menu", "menu") id=id name=name dirArgs=args>
  <#if !menuLevel?is_number>
    <#-- 2017-04-25: DO NOT use physical stack size for menu level - we need to keep relative increases
    <#local menuLevel = (getRequestStackSize("scipioMenuStack")!0) + 1>-->
    <#local menuLevel = (prevMenuInfo.menuLevel!0)+1>
    <#-- 2017-04-25: if we have inline entries, do NOT increase level -->
    <#if inlineItems && (menuLevel > 1)>
      <#local menuLevel = menuLevel - 1>
    </#if>
  </#if>

  <#local parentStyleName = "">
  <#local parentMenuSpecialType = "">
  
  <#if isNestedMenu>
    <#if !parentMenuType?has_content>
      <#local parentMenuType = (prevMenuInfo.type)!"">
    </#if>
    <#local parentStyleName = parentMenuType?replace("-","_")>
    
    <#if parentMenuType?has_content>
      <#-- make sure to look this up again because caller may override
      <#local parentMenuSpecialType = (prevMenuInfo.specialType)!"">-->
      <#local parentMenuSpecialType = styles["menu_" + parentStyleName + "_specialtype"]!"">
    </#if>
  <#else>
    <#-- force this off -->
    <#local parentMenuType = "">
  </#if>

  <#if !type?has_content>
    <#if isNestedMenu && parentMenuType?has_content>
      <#local type = parentMenuType>
    <#else>
      <#local type = "generic">
    </#if>
  </#if>

  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || (!(styles["menu_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>

  <#if htmlwrap?is_boolean && htmlwrap == false>
    <#local htmlwrap = "">
  <#elseif (htmlwrap?is_boolean && htmlwrap == true) || !htmlwrap?has_content>
    <#local htmlwrap = styles["menu_" + styleName + "_htmlwrap"]!styles["menu_default_htmlwrap"]!true>
    <#if htmlwrap?is_boolean>
      <#local htmlwrap = htmlwrap?string("ul", "")>
    </#if>
  </#if>

  <#if isNestedMenu && (type == parentMenuType)>
    <#-- If nested menu of same type as parent, use alternate menu class -->
    <#local class = addClassArgDefault(class, styles["menu_" + styleName + "_altnested"]!styles["menu_default_altnested"]!"")>
  <#else>
    <#local class = addClassArgDefault(class, styles["menu_" + styleName]!styles["menu_default"]!"")>
  </#if>

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

  <#local menuLevelPrefix = styles["menu_" + styleName + "_levelprefix"]!styles["menu_default_levelprefix"]!"">
  <#if menuLevelPrefix?has_content>
    <#local class = addClassArg(class, menuLevelPrefix + menuLevel?string)>
  </#if>

  <#if specialType?is_boolean && specialType == false>
    <#local specialType = "">
  <#else>
    <#local specialType = styles["menu_" + styleName + "_specialtype"]!"">
  </#if>

  <#-- NOTE: DEPRECATED mainButtonClass (was never documented) -->
  <#local titleClass = inlineArgs.mainButtonClass!explArgs.mainButtonClass!titleClass>
  <#local titleClass = addClassArgDefault(titleClass, styles["menu_" + styleName + "_title"]!styles["menu_" + styleName + "_mainbutton"]!styles["menu_default_title"]!"")>

  <#if activeTarget?is_boolean>
    <#if activeTarget>
      <#local active = true>
    </#if>
  </#if>
  <#if !active?is_boolean>
    <#local active = false>
  </#if>
  <#local class = menuAppendActiveStyle(class, styleName, "_active", active, activeTarget)>
  
  <#local namePrefix = styles["menu_" + styleName + "_nameprefix"]!styles["menu_default_nameprefix"]!"">
  <#if name?has_content && namePrefix?has_content>
    <#local class = addClassArg(class, namePrefix + name)>
  </#if>
  
  <#if sepMenuType?has_content>
    <#local sepStyleName = sepMenuType?replace("-","_")>
    <#local sepMenu = (styles["sepmenu_" + sepStyleName + "_config"]!{}) + {
         "type":sepMenuType,
         "hasMenu":(sepMenuDef.items)?has_content,
         "render":separateMenuImpl, 
         "menuDef":toSimpleMap(sepMenuDef)
    }>
  <#else>
    <#local sepMenu = {"hasMenu":false}>
  </#if>
  
  <#local menuInfo = {"type":type, "name":name, "specialType":specialType, "styleName":styleName, 
    "inlineItems":inlineItems, "class":class, "id":id, "style":style, "attribs":attribs, "titleClass":titleClass,
    "preItems":preItems, "postItems":postItems, "sort":sort, "sortBy":sortBy, "sortDesc":sortDesc, 
    "nestedFirst":nestedFirst, "isNestedMenu":isNestedMenu, 
    "parentMenuType":parentMenuType, "parentMenuSpecialType":parentMenuSpecialType, "parentStyleName":parentStyleName,
    "active":active, "activeTarget":activeTarget, "menuLevel":menuLevel, "itemClass":itemClass}>
  <#local dummy = setRequestVar("scipioCurrentMenuInfo", menuInfo)><#-- for easier access from nested content, in case -->
  <#local dummy = pushRequestStack("scipioMenuStack", menuInfo)>
  <#local dummy = setRequestVar("scipioCurrentMenuItemIndex", 0)>
  
  <@menu_markup type=type name=name specialType=specialType class=class id=id style=style attribs=attribs excludeAttribs=["class", "id", "style"] 
    inlineItems=inlineItems htmlwrap=htmlwrap title=title titleClass=titleClass mainButtonClass=titleClass isNestedMenu=isNestedMenu 
    parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType 
    active=active activeTarget=activeTarget menuLevel=menuLevel sepMenu=sepMenu origArgs=origArgs passArgs=passArgs>
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

  <#local dummy = popRequestStack("scipioMenuStack")>
  <#local dummy = setRequestVar("scipioCurrentMenuItemIndex", prevMenuItemIndex)>
  <#local dummy = setRequestVar("scipioLastMenuInfo", menuInfo)>
</@renderTarget>
</#macro>

<#function menuAppendActiveStyle class styleName propBasePrefix active activeTarget>
  <#-- already done by callers
  <#if activeTarget?is_boolean>
    <#if activeTarget>
      <#local active = true>
    </#if>
  </#if>
  <#if !active?is_boolean>
    <#local active = false>
  </#if>-->

  <#if active>
    <#local baseStyle = "menu_" + styleName + propBasePrefix>
    <#local class = addClassArg(class, styles[baseStyle]!styles["menu_default"+propBasePrefix]!"")>
    <#if activeTarget?is_boolean>
      <#if activeTarget>
        <#local class = addClassArg(class, styles[baseStyle+"target"]!styles["menu_default"+propBasePrefix+"target"]!"")>
      <#else>
        <#local class = addClassArg(class, styles[baseStyle+"ancestor"]!styles["menu_default"+propBasePrefix+"ancestor"]!"")>
      </#if>
    </#if>
  </#if>
  <#return class>
</#function>

<#-- 2017-04-25: callback that renders the core of the separate menu, to keep logic out of menu_markup,
    and optionally will also do the superficial extra entries like titles and separators.
    menuOnly=false is ONLY supported for the following layouts: before-inline. 
    NOTE: menu markup can optionally pass menuOnly=true and implement the extra entries itself.
        but in simple cases, this macro can handle the extra items fine. 
    DEV NOTE: NOT USED BY THE WIDGET CODE (in the end) -->
<#macro separateMenuImpl sepMenu={} menuOnly=true catchArgs...>
  <#local menuDef = sepMenu.menuDef!{}>
  <#if sepMenu.hasMenu>
    <#if sepMenu.layout!?ends_with("inline")>
      <#local menuDef = menuDef + {"inlineItems":true}>
    </#if>
    <#if sepMenu.sepItemClass?has_content>
      <#local menuDef = menuDef + {"itemClass":sepMenu.sepItemClass}>
    </#if>
    <#if sepMenu.sepMenuClass?has_content>
      <#local menuDef = menuDef + {"class":addClassArg(menuDef.class!"", getPlainClassArgNames(sepMenu.sepMenuClass))}><#-- TODO?: support crushing (=)? -->
    </#if>
  </#if>
  <#if menuOnly>
    <#if sepMenu.hasMenu>
      <@menu args=menuDef/>
    </#if>
  <#else>
    <#if (sepMenu.layout!) == "before-inline">
      <#if sepMenu.hasMenu>
        <#if sepMenu.sepTitle?has_content>
          <@menuitem type="text" class=sepMenu.sepTitleItemClass text=getTextLabelFromExpr(sepMenu.sepTitle)/>
        </#if>
        <#-- NOTE: for now we can let the target macro handle the extra items, but if we needed to we could
          pass menuOnly=true and implement the extra superficial entries manually instead -->
        <@menu args=menuDef/>
        <#if sepMenu.separatorEnabled>
          <@menuitem type="generic" class=sepMenu.separatorItemClass/>
        </#if>
      </#if>
      <#if sepMenu.nonsepTitle?has_content && (sepMenu.hasMenu || sepMenu.nonsepTitleAlways)>
        <@menuitem type="text" class=sepMenu.nonsepTitleItemClass text=getTextLabelFromExpr(sepMenu.nonsepTitle)/>
      </#if>
    </#if>
  </#if>
</#macro>

<#-- @menu container main markup - theme override 
    DEV NOTE: This is called directly from both @menu and widgets @renderMenuFull -->
<#macro menu_markup type="" specialType="" name="" class="" id="" style="" attribs={} excludeAttribs=[] 
    inlineItems=false titleClass="" title="" htmlwrap="ul" isNestedMenu=false parentMenuType="" parentMenuSpecialType=""
    active=false activeTarget="" menuLevel=1 sepMenu={}
    origArgs={} passArgs={} catchArgs...>
  <#if !inlineItems && htmlwrap?has_content>
    <#-- NOTE: here we always test specialType and never type, so that many (custom) menu types may reuse the same 
        existing specialType special handling without having to modify this code -->
    <#if specialType == "main">
      <#-- WARN: isNestedMenu check here would not be logical -->
      <li class="${styles.menu_main_wrap!}"><a href="#" class="${styles.menu_main_item_link!}"
        <#if (styles.framework!"") == "bootstrap"> data-toggle="dropdown"</#if>>${escapeVal(title, 'htmlmarkup')}<#if (styles.framework!"") == "bootstrap"> <i class="fa fa-fw fa-caret-down"></i></#if></a>
    <#elseif specialType == "sidebar" && !isNestedMenu>
      <#-- WARN: isNestedMenu check here is flawed, but it's all we need for now -->
      <nav class="${styles.nav_sidenav!""}">
        <#-- FIXME: this "navigation" variable is way too generic name! is it even still valid? -->
        <#if navigation?has_content><h2>${escapeVal(navigation, 'htmlmarkup')}</h2></#if>
    <#elseif specialType == "button-dropdown">
      <button href="#" data-dropdown="${escapeVal(id, 'html')}" aria-controls="${escapeVal(id, 'html')}" data-toggle="dropdown"aria-expanded="false"<@compiledClassAttribStr class=titleClass />>${escapeVal(title, 'htmlmarkup')}</button><br>
      <#local attribs = attribs + {"data-dropdown-content":"true", "aria-hidden":"true"}>
    </#if>
    <#if htmlwrap?has_content><${htmlwrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
  </#if>
  <#-- 2017-04-25: new separate-menu implementation. sepMenu contains all the entries defined in styles["sepmenu_[sepMenu.type]_config"] entries. -->
  <#if (sepMenu.layout!) == "before-inline">
    <#-- NOTE: for now we can let the target macro handle the extra items, but if we needed to we could
        pass menuOnly=true and implement the extra superficial entries manually instead -->
    <@sepMenu.render sepMenu=sepMenu menuOnly=false/>
  </#if>
      <#nested>
  <#if !inlineItems && htmlwrap?has_content>
    <#if specialType == "main">
        <#if htmlwrap?has_content></${htmlwrap}></#if>
      </li>
    <#elseif specialType == "sidebar" && !isNestedMenu>
        <#if htmlwrap?has_content></${htmlwrap}></#if>
      </nav>
    <#else>
      <#if htmlwrap?has_content></${htmlwrap}></#if>
    </#if>
  </#if>
</#macro>

<#-- 
*************
* Menu Item
************
Menu item macro. Must ALWAYS be enclosed in a @menu macro (see @menu options if need to generate items only).

WARN: Currently the enclosing @menu and sub-menus should never cross widget boundaries, and at most will
    survive direct FTL file includes.
             
  * Parameters *
    type                    = (generic|link|text|submit, default: generic) Menu item (content) type
                              * {{{generic}}}: any generic content, but specific types should be preferred.
    name                    = (optional) Internal name
                              If present, may be used for logic and styling needs.
                              2017-02-17: this is now used to generate a class name in the form: "item-name-${name}",
                              which can be used for styling purposes. Note that the name is not unique globally
                              and must be used in conjunction with more precise selectors.   
    class                   = ((css-class), default: -based on menu type-) CSS classes for menu item
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)  
                              NOTE: for this macro, the inline "class" args is now logically combined with the "class"
                                  arg from the "args" map using the logic in combineClassArgs function, with inline given priority.
    id                      = Menu item ID
    style                   = Legacy menu item style (for <li> element)
    attribs                 = ((map)) Extra menu item attributes (for <li> element)
    contentClass            = ((css-class)) CSS classes, for menu item content element (<a>, <span> or <input> element)
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly) 
                              NOTE: for this macro, the inline "contentClass" args is now logically combined with the "contentClass"
                                  arg from the "args" map using the logic in combineClassArgs function, with inline given priority.
    contentId               = Menu item content ID
    contentStyle            = Legacy menu item content style
    contentName             = Content name attrib ({{{name=""}}} on {{{<a>}}} link)
    contentAttribs          = ((map)) Extra menu item content attributes (for {{{<a>}}}, {{{<span>}}} or {{{<input>}}} element)
    text                    = Text to use as content
                              By default, you '''must''' use this to specific link text, not nested content;
                              nested content will by default be put outside the link.
    href                    = Content link, for {{{link}}} type
                              Also supports ofbiz request URLs using the notation: {{{ofbizUrl://}}} (see interpretRequestUri function)
                              NOTE: This parameter is automatically (re-)escaped for HTML and javascript (using #escapeFullUrl or equivalent) 
                                  to help prevent injection, as it is high-risk. It accepts pre-escaped query string delimiters for compatibility,
                                  but other characters should not be manually escaped (apart from URL parameter encoding).
    onClick                 = ((js)) onClick (for content elem)
    title                   = Logical title attribute of content
    disabled                = ((boolean), default: false) Whether menu item disabled
    active                  = ((boolean), default: false) Whether menu item active, either target or ancestor (current page)
                              NOTE: "active" translates to ("selected" OR "selected ancestor") in menu widget terminology (see CommonMenus.xml).
    activeTarget            = ((boolean)|, default: -empty/unspecified/unknown-) Whether menu item is the active target but not ancestor (current page)
                              If true, implies active. If explicit false, means active ancestor. If not set, unknown or unspecified by template.
                              NOTE(2016-09-01): currently this is NOT determined automatically.
                              NOTE: "active target" translates to "selected" item in menu widget terminology (see CommonMenus.xml)
    selected                = ((boolean), default: false) Whether selected or not (selected but not necessarily current)
                              NOTE: Currently this is not used much. It would be used for marking an item as preselected.
    nestedContent           = Macro arg alternative to macro nested content
                              This may be passed in @menu items list.
                              NOTE: Not escaped by macro.
    nestedMenu              = ((map)) Map of @menu arguments, alternative to nestedContent arg and macro nested content
                              For menu to use as sub-menu.
    wrapNested              = ((boolean), default: -true for type generic, false for all other types-) If true, nested content is wrapped within the content element (link, span, etc.) 
                              If false, the nested content will come before or after (depending on nestedFirst) the content element.
    nestedFirst             = ((boolean), default: false) If true, nested content comes before content elem
    htmlwrap                = (li|span|div, default: -from global styles-, fallback default: li) Wrapping HTML element
    inlineItem              = ((boolean)) If true, generate only items, not menu container
    contentWrapElem         = ((boolean)|div|..., default: -depends on type-) For {{{generic}}} and {{{text}}} type items, controls the extra content wrapper around nested
                              If true, a {{{div}}} (generic) or {{{span}}} (text) will be added around nested; if any string, the given string will be used as the element.
                              This wrapper will receive the contentClass and other content attributes normally given to inline elements for the other types.
                              If false, no wrapper will be added.
                              For generic, the default is false. For text, the default is true.
                              NOTE: This currently only works for {{{generic}}} and {{{text}}} type items. 
-->
<#assign menuitem_defaultArgs = {
  "type":"generic", "name":"", "class":"", "contentClass":"", "id":"", "style":"", "attribs":{},
  "contentId":"", "contentStyle":"", "contentName":"", "contentAttribs":"", "text":"", "href":true,
  "onClick":"", "disabled":"", "selected":"", "active":"", "activeTarget":"", "target":"", "title":"",
  "nestedContent":true, "nestedMenu":false, "wrapNested":"", "nestedFirst":false,
  "htmlwrap":true, "inlineItem":false, "contentWrapElem":-1, "isNestedMenu":"", "passArgs":{}
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
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.menuitem_defaultArgs, {
    <#-- parameters: overrides -->
    "class" : class,
    "contentClass" : contentClass
  })>
  <#local dummy = localsPutAll(args)>
  <#local attribs = makeAttribMapFromArgMap(args)>
  <#local origArgs = args>

  <#local menuInfo = readRequestStack("scipioMenuStack")!{}>

  <#local itemIndex = getRequestVar("scipioCurrentMenuItemIndex")!0>

  <#local menuType = (menuInfo.type)!"">
  <#local menuSpecialType = (menuInfo.specialType)!"">
  <#local menuStyleName = (menuInfo.styleName)!"">
  <#local parentMenuType = (menuInfo.parentMenuType)!"">
  <#local parentMenuSpecialType = (menuInfo.parentMenuSpecialType)!"">
  <#local menuLevel = (menuInfo.menuLevel)!1>
  <#local menuName = (menuInfo.name)!"">
  
  <#if !isNestedMenu?is_boolean>
    <#local isNestedMenu = (menuInfo.isNestedMenu)!false>
  </#if>
  
  <#if htmlwrap?is_boolean && htmlwrap == false>
    <#local htmlwrap = "">
  <#elseif (htmlwrap?is_boolean && htmlwrap == true) || !htmlwrap?has_content>
    <#local htmlwrap = styles["menu_" + menuStyleName + "_item_htmlwrap"]!styles["menu_default_item_htmlwrap"]!true>
    <#if htmlwrap?is_boolean>
      <#local htmlwrap = htmlwrap?string("li", "")>
    </#if>
  </#if>

  <#if !wrapNested?is_boolean>
    <#if !type?has_content || type == "generic">
      <#local wrapNested = true>
    <#else>
      <#local wrapNested = false>
    </#if>
  </#if>

  <#if menuInfo.itemClass?has_content>
    <#local class = addClassArg(class, getPlainClassArgNames(menuInfo.itemClass))><#-- TODO?: support crushing (=)? -->
  </#if>

  <#if !disabled?is_boolean>
    <#local disabled = false>
  </#if>
  <#if disabled>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_itemdisabled"]!styles["menu_default_itemdisabled"]!""))>
    <#local contentClass = addClassArg(contentClass, (styles["menu_" + menuStyleName + "_item_contentdisabled"]!styles["menu_default_item_contentdisabled"]!""))>
    <#-- FIXME: this static method of disabling links means the link loses information and not easily toggleable! -->
    <#local href = styles.menu_link_href_default!"">
  </#if>
  <#-- NOTE: selected means something different than it does for menu widgets. what you want is "active". -->
  <#if !selected?is_boolean>
    <#local selected = false>
  </#if>
  <#if selected>
    <#local class = addClassArg(class, (styles["menu_" + menuStyleName + "_itemselected"]!styles["menu_default_itemselected"]!""))>
    <#local contentClass = addClassArg(contentClass, (styles["menu_" + menuStyleName + "_item_contentselected"]!styles["menu_default_item_contentselected"]!""))>
  </#if>

  <#if activeTarget?is_boolean>
    <#if activeTarget>
      <#local active = true>
    </#if>
  </#if>
  <#if !active?is_boolean>
    <#local active = false>
  </#if>
  <#local class = menuAppendActiveStyle(class, menuStyleName, "_itemactive", active, activeTarget)>
  <#local contentClass = menuAppendActiveStyle(contentClass, menuStyleName, "_item_contentactive", active, activeTarget)>

  <#local namePrefix = styles["menu_" + menuStyleName + "_itemnameprefix"]!styles["menu_default_itemnameprefix"]!"">
  <#if name?has_content && namePrefix?has_content>
    <#local class = addClassArg(class, namePrefix + name)>
  </#if>
  <#local class = addClassArgDefault(class, styles["menu_" + menuStyleName + "_item"]!styles["menu_default_item"]!"")>

  <#if type == "link">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_link"]!styles["menu_default_item_link"]!"">
  <#elseif type == "text">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_text"]!styles["menu_default_item_text"]!"">
  <#elseif type == "submit">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_submit"]!styles["menu_default_item_submit"]!"">
  <#else>
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_submit"]!styles["menu_default_item_generic"]!"">
  </#if>
  <#if name?has_content && namePrefix?has_content>
    <#local contentClass = addClassArg(contentClass, namePrefix + name)>
  </#if>
  <#local contentClass = addClassArgDefault(contentClass, defaultContentClass)>
  <#local specialType = "">

  <@menuitem_markup type=type menuType=menuType menuSpecialType=menuSpecialType name=name menuName=menuName class=class id=id style=style attribs=attribs 
    excludeAttribs=["class", "id", "style"] inlineItem=inlineItem htmlwrap=htmlwrap disabled=disabled selected=selected active=active activeTarget=activeTarget
    isNestedMenu=isNestedMenu menuLevel=menuLevel parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType origArgs=origArgs passArgs=passArgs><#rt>
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
        <#local href = styles.menu_link_href_default!>
      </#if>
      <#local href = interpretRequestUri(href)>
      <#t><@menuitem_link_markup href=href onClick=onClick class=contentClass id=contentId style=contentStyle 
            name=contentName attribs=contentAttribs excludeAttribs=["class","id","style","href","onclick","target","title"] 
            target=target title=title disabled=disabled selected=selected active=active activeTarget=activeTarget isNestedMenu=isNestedMenu menuLevel=menuLevel 
            parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType
            itemType=type menuType=menuType menuSpecialType=menuSpecialType itemIndex=itemIndex
            itemName=name menuName=menuName 
            origArgs=origArgs passArgs=passArgs><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_link_markup>
    <#elseif type == "text">
      <#if contentWrapElem?is_number>
        <#local contentWrapElem = true>
      </#if>
      <#t><@menuitem_text_markup contentWrapElem=contentWrapElem class=contentClass id=contentId style=contentStyle attribs=contentAttribs 
            excludeAttribs=["class","id","style","onclick"] onClick=onClick disabled=disabled selected=selected active=active activeTarget=activeTarget 
            isNestedMenu=isNestedMenu menuLevel=menuLevel parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType 
            itemType=type menuType=menuType menuSpecialType=menuSpecialType itemIndex=itemIndex
            itemName=name menuName=menuName 
            origArgs=origArgs passArgs=passArgs><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_text_markup>
    <#elseif type == "submit">
      <#t><#if wrapNested && nestedFirst>${nestedContent}</#if><@menuitem_submit_markup class=contentClass 
            id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","value","onclick","disabled","type"] 
            onClick=onClick disabled=disabled selected=selected active=active activeTarget=activeTarget isNestedMenu=isNestedMenu menuLevel=menuLevel 
            parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType 
            itemType=type menuType=menuType menuSpecialType=menuSpecialType itemIndex=itemIndex
            itemName=name menuName=menuName 
            origArgs=origArgs passArgs=passArgs><#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if></@menuitem_submit_markup><#if wrapNested && !nestedFirst> ${nestedContent}</#if>
    <#else>
      <#if contentWrapElem?is_number>
        <#local contentWrapElem = false>
      </#if>
      <#t><@menuitem_generic_markup contentWrapElem=contentWrapElem class=contentClass id=contentId style=contentStyle 
            attribs=contentAttribs excludeAttribs=["class","id","style","onclick"] onClick=onClick disabled=disabled 
            selected=selected active=active activeTarget=activeTarget isNestedMenu=isNestedMenu menuLevel=menuLevel parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType
            itemType=type menuType=menuType menuSpecialType=menuSpecialType itemIndex=itemIndex
            itemName=name menuName=menuName 
            origArgs=origArgs passArgs=passArgs><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_generic_markup>
    </#if>
    <#t><#if !wrapNested && !nestedFirst>${nestedContent}</#if>
  </@menuitem_markup><#lt>
  <#local dummy = setRequestVar("scipioCurrentMenuItemIndex", itemIndex + 1)>
</#macro>

<#-- @menuitem container markup - theme override 
  DEV NOTE: This is called directly from both @menuitem and widgets @renderMenuItemFull -->
<#macro menuitem_markup type="" menuType="" menuSpecialType="" name="" menuName="" class="" id="" style="" attribs={} 
    excludeAttribs=[] inlineItem=false htmlwrap="li" disabled=false selected=false active=false activeTarget=""
    isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0 origArgs={} passArgs={} catchArgs...>
  <#if !inlineItem && htmlwrap?has_content>
    <${htmlwrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if>><#rt>
  </#if>
      <#nested><#t>
  <#if !inlineItem && htmlwrap?has_content>
    </${htmlwrap}><#lt>
  </#if>
</#macro>

<#-- @menuitem type="link" markup - theme override -->
<#macro menuitem_link_markup itemType="" menuType="" menuSpecialType="" itemName="" menuName="" class="" id="" style="" href="" name="" onClick="" target="" title="" 
    attribs={} excludeAttribs=[] disabled=false selected=false active=false activeTarget="" isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0 
    origArgs={} passArgs={} catchArgs...>
  <#t><a href="${escapeFullUrl(href, 'html')}"<#if onClick?has_content> onclick="${escapeVal(onClick, 'html')}"</#if><@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if name?has_content> name="${escapeVal(name, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if target?has_content> target="${escapeVal(target, 'html')}"</#if><#if title?has_content> title="${escapeVal(title, 'html')}"</#if>><#nested></a>
</#macro>

<#-- @menuitem type="text" markup - theme override -->
<#macro menuitem_text_markup itemType="" menuType="" menuSpecialType="" itemName="" menuName="" contentWrapElem=true class="" id="" style="" onClick="" attribs={} excludeAttribs=[] 
    disabled=false selected=false active=false activeTarget="" isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0 
    origArgs={} passArgs={} catchArgs...>
  <#if contentWrapElem?is_boolean>
    <#local contentWrapElem = contentWrapElem?string("span", "")>
  </#if>
  <#t><#if contentWrapElem?has_content><${contentWrapElem}<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${escapeVal(onClick, 'html')}"</#if>></#if><#nested><#if contentWrapElem?has_content></${contentWrapElem}></#if>
</#macro>

<#-- @menuitem type="submit" markup - theme override -->
<#macro menuitem_submit_markup itemType="" menuType="" menuSpecialType="" itemName="" menuName="" class="" id="" style="" text="" onClick="" disabled=false attribs={} 
    excludeAttribs=[] disabled=false selected=false active=false activeTarget="" isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0 
    origArgs={} passArgs={} catchArgs...>
  <#t><button type="submit"<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${escapeVal(onClick, 'html')}"</#if><#if disabled> disabled="disabled"</#if>/><#nested></button>
</#macro>

<#-- @menuitem type="generic" markup - theme override -->
<#macro menuitem_generic_markup itemType="" menuType="" menuSpecialType="" itemName="" menuName="" contentWrapElem=false class="" id="" style="" onClick="" attribs={} 
    excludeAttribs=[] disabled=false selected=false active=false activeTarget="" isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0 
    origArgs={} passArgs={} catchArgs...>
  <#if contentWrapElem?is_boolean>
    <#local contentWrapElem = contentWrapElem?string("div", "")>
  </#if>
  <#t><#if contentWrapElem?has_content><${contentWrapElem}<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${escapeVal(onClick, 'html')}"</#if>></#if><#nested><#if contentWrapElem?has_content></${contentWrapElem}></#if>
</#macro>

<#-- 
*************
* Menu Markup Inline Check
************
Function that examines a string containing menu HTML markup and returns true if and only if
the menu items are inlined, i.e. without container. 

Occasionally macros need to check this, notably for compatibility with Ofbiz screens.
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
Creates a pagination menu, for example around a data table, using Ofbiz view pagination
functionality.

  * Usage Examples *  
    <@paginate mode="single" ... />
    <@paginate mode="content">
      <@table type="data-list">
        ...
      </@table>
    </@paginate>            
                    
  * Parameters *
   mode                     = (content|single, default: single)
                              * {{{content}}}: decorates the nested content with one or more pagination menus (depending on layout, and layout can be centralized)
                                NOTE: in overwhelmingly most cases, this mode should be preferred, as it offers more control to the theme.
                              * {{{single}}}: produces a single pagination menu (layout argument has no effect)
   type                     = (default, default: default) Type of the pagination menu itself
                              * {{{default}}}: default scipio pagination menu
   layout                   = (default|top|bottom|both, default: default) Type of layout, only meaningful for "content" mode
                              * {{{default}}}: "pagination_layout" from styles hash, otherwise both
                              * {{{top}}}: no more than one menu, always at top
                              * {{{bottom}}}: no more than one menu, always at bottom
                              * {{{both}}}: always two menus, top and bottom
   position                 = (top|bottom|, default: -empty-) Optional position indicator, only makes sense in single mode.
                              If specified, it may lead to the pagination not rendering depending on resolved value of layout.
                              In content mode (preferred), this is handled automatically.
   noResultsMode            = (default|hide|disable, default: default)
                              * {{{default}}}: "pagination_noresultsmode" from styles hash, otherwise hide. may depend on mode argument.
                              * {{{hide}}}: hide menu when no results
                              * {{{disable}}}: disable but show controls when no results (TODO?: not implemented)
   enabled                  = ((boolean), default: true) Manual control to disable the entire macro
                              Sometimes needed to work around FTL language.
                              For "content" mode, with false, will still render nested content (that is the purpose), but will never decorate.
   url                      = Base Url to be used for pagination
                              NOTE: This parameter is automatically (re-)escaped for HTML and javascript (using #escapeFullUrl or equivalent) 
                                  to help prevent injection, as it is high-risk. It accepts pre-escaped query string delimiters for compatibility,
                                  but other characters should not be manually escaped (apart from URL parameter encoding).
   class                    = ((css-class)) CSS classes 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
   listSize                 = Size of the list in total
   viewIndex                = ((int)) Page currently displayed
   viewSize                 = ((int)) Maximum number of items displayed
                              NOTE: this should be decided earlier in rendering (data prep) and a valid value MUST be passed.
   forcePost                = ((boolean), default: false) Always use POST for non-ajax browsing 
                              NOTE: even if false, large requests may be coerced to POST.
   paramStr                 = Extra URL parameters in string format, escaped 
                              e.g.
                                param1=val1&amp;param2=val2
                              NOTE: This parameter is automatically (re-)escaped for HTML and javascript (using #escapeFullUrl or equivalent) 
                                  to help prevent injection, as it is high-risk. It accepts pre-escaped query string delimiters for compatibility,
                                  but other characters should not be manually escaped (apart from URL parameter encoding).
   viewIndexFirst           = ((int)) First viewIndex value number (0 or 1, only affects param values, not display)
   showCount                = ((boolean)) If true show "Displaying..." count or string provided in countMsg; if false don't; empty string is let markup/theme decide
   alwaysShowCount          = ((boolean)) If true, show count even if other pagination controls are supposed to be omitted
   countMsg                 = Custom message for count, optional; markup provides its own default or in styles hash
   lowCountMsg              = Alternate custom message for low counts, optional; markup provides its own or in styles hash
   paginateToggle           = ((boolean)) If true, include a control to toggle pagination on/off 
                              (specify current state with paginateOn and tweak using paginateToggle* arguments)
   paginateOn               = ((boolean)) Indicates whether pagination is currently on or off
                              Can be used with paginateToggle to indicate current state, or set to false to prevent
                              pagination controls while still allowing some decorations (depending on styling).
                              NOTE: this is not the same as enabled control. paginateOn does not prevent macro from rendering.
   previousViewSize         = ((int)) Used if paginate state is off. if not specified, it will use a default from general.properties.
   paginateOffViewSize      = ((int), default: -in general.properties-) A viewSize value send when turning off pagination via toggle
   viewSizeSelection        = ((boolean), default: false) Currently officially unsupported.
                              DEV NOTE: only here for testing purposes
   altParam                 = Use viewIndex/viewSize as parameter names, instead of VIEW_INDEX / VIEW_SIZE
   viewIndexString          = (default: VIEW_INDEX) Specific param name to use        
   viewSizeString           = (default: VIEW_SIZE) Specific param name to use        
   paginateToggleString     = (default: PAGING) Specific param name to use     
   paramPrefix              = (default: -empty-) Prefix added to param names. Some screens need "~".
                              NOTE: Does not affect paramStr - caller must handle.
   paramDelim               = (default: "&amp;") Param delimiter. Some screens need "/".
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
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.paginate_defaultArgs)>
  <#local dummy = localsPutAll(args)>

  <#-- this is also checked in paginate_core, but avoid problems with parameters by checking again early. -->  
  <#if enabled?is_boolean && enabled == false>
    <#if mode != "single">
      <#nested>
    </#if>
  <#else>

    <#-- these errors apparently happen a lot, enforce here cause screens never catch, guarantee other checks work -->
    <#if (!viewSize?is_number)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was not a number type: " + viewSize, "htmlUtilitiesPaginate")!>
      <#local viewSize = viewSize?number>
    </#if>
    <#local viewSize = viewSize?floor>
    <#if (viewSize <= 0)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was not a positive number: " + viewSize, "htmlUtilitiesPaginate")!>
      <#local viewSize = 1>
    </#if>  
    <#if (!viewIndex?is_number)>
      <#local dummy = Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was not a number type: " + viewIndex, "htmlUtilitiesPaginate")!>
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
  
    <#local origUrl = rawString(url)>
    <#local origParamStr = rawString(paramStr)>
  
    <#-- SPECIAL CASE: if paramDelim=="/" and url contains ";" or "?" we must strip the non-dir params and reappend them later 
         WARN: we can ignore paramStr to simplify; assume caller followed his own conventions... -->
    <#local urlSuffix = "">
    <#if paramDelim?contains("/")>
      <#local url = stripParamStrFromUrl(url)>
      <#if (url?length < origUrl?length)>
        <#local urlSuffix = origUrl[url?length..]>
      </#if>
    </#if>

    <#local commonUrl = addParamDelimToUrl(rawString(url), paramDelim)>
    <#if paramStr?has_content>
      <#local commonUrl = commonUrl + trimParamStrDelims(rawString(paramStr), paramDelim) + paramDelim>
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
    enabled                 = ((boolean)) Disables the whole macro
    paginate                = ((boolean)) Display hint, does not seem to mean guarantee data wasn't paginated
    forcePost               = ((boolean)) If true, HTTP requests must be in HTTP POST (sometimes required, other times simply better)
    viewIndexFirst          = ((int)) First index
    listItemsOnly           = ((boolean)) Only show core paginate items, no container
    paginateToggle          = ((boolean)) If true, include a control to toggle pagination on or off
    paginateOn              = ((boolean)) This tells if current state is on or off (but doesn't prevent whole macro)
    position                = (top|bottom|) Informs the macro and markup of how/where the menu is used
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
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.paginate_core_defaultArgs)>
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
  
  <#-- NOTE: possible that data was paginated even if enabled false, but don't bother right now. 
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

  <#-- NOTE: (listSize > minPageSize) implies (listSize > 0); some cases this gets called with listSize zero -->
  <#if paginateOn && (listSize > minPageSize)>
    
      <#local itemRange = 2/>
      <#local placeHolder ="..."/>
    
      <#if !listItemsOnly>
        <div class="${styles.grid_row!}">

          <div class="${styles.grid_large!}2 ${styles.grid_cell!}"><#if showCount>${escapeVal(countMsg, 'htmlmarkup')}</#if></div>
          <div class="${styles.grid_large!}8 ${styles.grid_cell!}">
            <div<@compiledClassAttribStr class=paginateClass />>
              <ul class="${styles.pagination_list!}">
      </#if>
  
            <#-- NOTE: must use submitPaginationPost JS function to force send as POST for some requests, because Ofbiz security feature prevents
                 GET params passed to controller service event when request is https="true".
                 NOTE: submitPagination (new in stock Ofbiz 14) already sends as POST in some cases, but not based on controller.
                 FIXME: POST/forcePost currently only supported when js enabled (non-js need extra markup for a form, ugly),
                    currently non-js falls back to GET only, won't always work -->
  
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxFirstUrl, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(firstUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(firstUrl, 'html')}"</#if></#local>
                <li class="${styles.pagination_item!} ${escapeVal(compileClassArg(paginateFirstClass), 'html')}<#if (viewIndex > viewIndexFirst)>"><a ${actionStr}>${escapeVal(paginateFirstLabel, 'htmlmarkup')}</a><#else> ${styles.pagination_item_disabled!}"><span>${escapeVal(paginateFirstLabel, 'htmlmarkup')}</span></#if></li>
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxPreviousUrl, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(previousUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(previousUrl, 'html')}"</#if></#local>
                <li class="${styles.pagination_item!} ${escapeVal(compileClassArg(paginatePreviousClass), 'html')}<#if (viewIndex > viewIndexFirst)>"><a ${actionStr}>${escapeVal(paginatePreviousLabel, 'htmlmarkup')}</a><#else> ${styles.pagination_item_disabled!}"><span>${escapeVal(paginatePreviousLabel, 'htmlmarkup')}</span></#if></li>
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
                      <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxSelectUrl + vi?string, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(finalSelectUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(finalSelectUrl, 'html')}"</#if></#local>
                      <li><a ${actionStr}>${i}</a></li>
                    </#if>
                  <#else>
                  <#if displayDots><li><span>${escapeVal(placeHolder, 'htmlmarkup')}</span></li></#if>
                  <#local displayDots = false/>
                  </#if>
                </#list>
            </#if>
            
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxNextUrl, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(nextUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(nextUrl, 'html')}"</#if></#local>
                <li class="${styles.pagination_item!} ${escapeVal(compileClassArg(paginateNextClass), 'html')}<#if (highIndex < listSize)>"><a ${actionStr}>${escapeVal(paginateNextLabel, 'htmlmarkup')}</a><#else> ${styles.pagination_item_disabled!}"><span>${escapeVal(paginateNextLabel, 'htmlmarkup')}</span></#if></li>
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxLastUrl, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(lastUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(lastUrl, 'html')}"</#if></#local>
                <li class="${styles.pagination_item!} ${escapeVal(compileClassArg(paginateLastClass), 'html')}<#if (highIndex < listSize)>"><a ${actionStr}>${escapeVal(paginateLastLabel, 'htmlmarkup')}</a><#else> ${styles.pagination_item_disabled!}"><span>${escapeVal(paginateLastLabel, 'htmlmarkup')}</span></#if></li>         
  
      <#if !listItemsOnly>  
              </ul>
            </div>
          </div>
          <#if paginateToggle>
            <#local paginateToggleContent>
              <#-- NOTE: duplicated below -->
              <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxPaginateOffUrl, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(paginateOffUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(paginateOffUrl, 'html')}"</#if></#local>
              <#local paginateOffClass = addClassArg(paginateOffClass, styles.pagination_item!)>
              <span<@compiledClassAttribStr class=paginateOffClass />><a ${actionStr}>${escapeVal(paginateOffLabel, 'htmlmarkup')}</a></span>       
            </#local>    
          </#if>
          <div class="${styles.grid_large!}2 ${styles.grid_cell!}">
            <#if javaScriptEnabled>
              <#if viewSizeSelection>
                <#local actionStr>onchange="<#if ajaxEnabled>ajaxUpdateAreas('${escapeFullUrl(ajaxSelectSizeUrl, 'js-html')}')<#else><#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(selectSizeUrl, 'js-html')}')</#if>"</#local>
                <div class="${styles.grid_row!}">
                    <div class="${styles.grid_large!}6 ${styles.grid_cell!}">
                        <label>${escapeVal(paginateViewSizeLabel, 'htmlmarkup')}</label>
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
        <div class="${styles.grid_large!}2 ${styles.grid_cell!} ${styles.grid_end!}">${escapeVal(countMsg, 'htmlmarkup')}</div>
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
              <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxPaginateOnUrl, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(paginateOnUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(paginateOnUrl, 'html')}"</#if></#local>
              <#local paginateOffClass = addClassArg(paginateOnClass, styles.pagination_item!)>
              <li<@compiledClassAttribStr class=paginateOnClass />><a ${actionStr}>${escapeVal(paginateOnLabel, 'htmlmarkup')}</a></li>  
            <#else>
              <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${escapeFullUrl(ajaxPaginateOffUrl, 'js-html')}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${escapeFullUrl(paginateOffUrl, 'js-html')}')"</#if><#else>href="${escapeFullUrl(paginateOffUrl, 'html')}"</#if></#local>
              <#local paginateOffClass = addClassArg(paginateOffClass, styles.pagination_item!)>
              <li<@compiledClassAttribStr class=paginateOffClass />><a ${actionStr}>${escapeVal(paginateOffLabel, 'htmlmarkup')}</a></li> 
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
          <div class="${styles.grid_large!}12 ${styles.grid_cell!} ${styles.grid_end!}">${escapeVal(countMsg, 'htmlmarkup')}</div>
        </div>
      </#if>
  </#if>
</#macro>    

<#-- 
*************
* Tree Menu
************
Renders a menu in a tree fashion.

The tree can be defined in nested or flat format to describe the hierarchy, and using either 
macro calls or by passing item lists of maps.

For {{{settings}}} and {{{plugins}}} map arguments, child map elements which should not
be interpreted as string but rather as script (such as javascript functions) can be wrapped
using #wrapRawScript.

WARN: The parameters which contain full javascript code (not just string values/within-literals)
    are not and cannot be escaped by the macro; caller is responsible for escaping them!
    Use #escapeVal (preferred) or {{{?js_string}}}.

DEV NOTE: Currently this does not fully abstract the library used, because difficult without sacrificing options.
    But in theory it should be possible to translate attributes from one library to another.
                                 
  * Usage Examples *  
  
    <@treemenu type="lib-basic">
        <@treeitem text="Some item" />
        <@treeitem text="Some item">
            <@treeitem text="Some item" />
            <@treeitem text="Some item" />
        </@treeitem>
        <@treeitem text="Some item" />
    </@treemenu>
    
    OR
    
    <@treemenu type="lib-basic" items=[
        {"text":"Some item"},
        {"text":"Some item", "items":[
            {"text":"Some item"},
            {"text":"Some item"}
        ]},
        {"text":"Some item"}
    ]/>
    
    OR
    
    <@treemenu type="lib-basic">
        <@treeitem text="Some item" parent="#" id="treeitem_3_root1"/>
        <@treeitem text="Some item" parent="#" id="treeitem_3_root2"/>
        <@treeitem text="Some item" parent="treeitem_3_root2" id="treeitem_3_child1"/>
        <@treeitem text="Some item" parent="treeitem_3_root2" id="treeitem_3_child2"/>
        <@treeitem text="Some item" parent="#" id="treeitem_3_root3"/>
    </@treemenu>
    
    OR
    
    <@treemenu type="lib-basic" items=[
        {"text":"Some item", "isRoot":true, "id":"treeitem_4_root1"},
        {"text":"Some item", "isRoot":true, "id":"treeitem_4_root2"},
        {"text":"Some item", "parent":"treeitem_4_root2", "id":"treeitem_4_child1"},
        {"text":"Some item", "parent":"treeitem_4_root2", "id":"treeitem_4_child2"},
        {"text":"Some item", "isRoot":true, "id":"treeitem_4_root3"}
    ]/>
                    
  * Parameters *
    type                    = (lib-basic|lib-model, default: lib-basic) Type of tree and generation method
                              * {{{lib-basic}}}: uses @treeitem or {{{items}}} list to generate the tree items,
                                while {{{plugins}}} and {{{settings}}} are extra settings, as simple maps
                              * {{{lib-model}}}: uses a (java) model from {{{data}}}, {{{plugins}}}, {{{settings}}} arguments to generate the tree
                              NOTE: 2016-08-04: The default is now lib-basic.
    library                 = (jstree, default: jstree) Library
                              NOTE: is case insensitive
    inlineItems             = ((boolean)) If true, generate only items, not menu container
                              NOTE: currently unused.
                              TODO: implement
    id                      = Menu ID
                              If omitted, will be auto-generated.
    attribs                 = ((map)) Map of other tree menu attribs
                              NOTE: currently unused.
                              TODO: implement
    items                   = ((list)) List of items, each a map of a arguments to @treeitem
    nestedFirst             = ((boolean), default: 
    data                    = ((object)) Data model
                              Depends on type and library:
                              * {{{lib-model}}}, {{{jstree}}}: list of JsTreeHelper$JsTreeDataItem objects, where each object contains fields representing a tree menu item
                              * {{{lib-basic}}}, {{{jstree}}}: unused. use {{{items}}} instead.
    settings                = ((object)) Tree library settings
                              Depends on type and library:
                              * {{{lib-model}}}, {{{jstree}}}: settings model class
                              * {{{lib-basic}}}, {{{jstree}}}: map of settings, added alongside the core data
    plugins                 = ((object)) Tree plugin settings
                              Depends on type and library:
                              * {{{lib-model}}}, {{{jstree}}}: plugins model class
                              * {{{lib-basic}}}, {{{jstree}}}: list of maps where each map follows the format:
                                  {"name":(plugin name), "settings":(map of jstree plugin settings)}
    items                   = ((list)) List of maps, where each hash contains arguments representing a menu item,
                              Same as @treeitem macro parameters.
                              Alternatively, the items can be specified as nested content.
    preItems                = ((list)) Special-case list of maps of items, added before items and nested content
                              Excluded from sorting.
                              Templates should generally avoid use unless specific need, but may be used by other macros.
    postItems               = ((list)) Special-case list of maps of items, added after items and nested content
                              Excluded from sorting.
                              Avoid use unless specific need; may be needed by scipio menu handling.
                              Templates should generally avoid use unless specific need, but may be used by other macros.
    sort,
    sortBy,
    sortDesc                = Items sorting behavior; will only work if items are specified
                              through items list of hashes, currently does not apply to 
                              nested items. by default, sorts by text, or sortBy can specify a menu item arg to sort by.
                              normally case-insensitive.
    nestedFirst             = ((boolean), default: false) If true, use nested items before items list, otherwise items list always first
                              Usually should use only one of alternatives, but is versatile.    
    events                  = ((map)) Map of javascript events to code
                              The code is inlined into a callback function having arguments {{{(e, data)}}}.
                              NOTE: Must not specify "on" prefix.
    defaultNodeIcon         = ((string), default: -depends on library-) Default (leaf) node icon
                              Defaults:
                              * {{{jstree}}}: {{{jstree-file}}} (in standard theme)
    defaultDirIcon          = ((string), default: -depends on library-) Default non-leaf node icon
                              Defaults:
                              * {{{jstree}}}: {{{jstree-folder}}} (in standard theme)
                              NOTE: Currently, this will only be used for nested jstree format.
                                  It does not apply to the flat data format (with parent: attributes).
                                  
  * Related *
    @treeitem
    
  * History *
    Enhanced for 1.14.2.
-->
<#assign treemenu_defaultArgs = {
  "type":"", "nestedFirst":false, "library":"", "data":{}, "settings": {}, "plugins": [], "inlineItems":false, "id":"", "attribs":{}, 
  "items":true, "preItems":true, "postItems":true, "sort":false, "sortBy":"", "sortDesc":false, "nestedFirst":false,
  "defaultNodeIcon":"", "defaultDirIcon":"",
  "events":{}, "passArgs":{}
}>
<#macro treemenu args={} inlineArgs...>
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: this MUST be called here (or through concatMaps) to handle .class key properly -->  
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.treemenu_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local attribs = makeAttribMapFromArgMap(args)>  
  
  <#if !type?has_content>
    <#local type = "lib-basic">
  </#if>
  
  <#-- TODO: change #globals into request vars -->
  
  <#if !library?has_content>
    <#local library = styles.treemenu_deflib!"jstree">
  </#if>
  <#local treeMenuLibrary = library?lower_case/>

  <#global scipioTreeMenuLibrary = treeMenuLibrary>
  
  <#local libStyleName = "lib_" + treeMenuLibrary?replace("-","_")>
  
  <#if !(defaultNodeIcon?is_boolean && defaultNodeIcon == false)>
    <#if defaultNodeIcon?is_boolean || !defaultNodeIcon?has_content>
      <#local defaultNodeIcon = styles["treemenu_"+libStyleName+"_defnodeicon"]!"">
    </#if>
  </#if>
  <#if !(defaultDirIcon?is_boolean && defaultDirIcon == false)>
    <#if defaultDirIcon?is_boolean || !defaultDirIcon?has_content>
      <#local defaultDirIcon = styles["treemenu_"+libStyleName+"_defdiricon"]!"">
    </#if>
  </#if>
  <#global scipioTreeMenuDefNodeIcon = defaultNodeIcon>
  <#global scipioTreeMenuDefDirIcon = defaultDirIcon>

  <#local menuIdNum = getRequestVar("scipioTreeMenuIdNum")!0>
  <#local menuIdNum = menuIdNum + 1 />
  <#local dummy = setRequestVar("scipioTreeMenuIdNum", menuIdNum)>
  <#if !id?has_content>
    <#local id = "treemenu_" + menuIdNum><#-- FIXME?: is this name too generic? -->
  </#if>
  
  <#global scipioTreeMenuIsFirstItem = true>
  
  <@treemenu_markup type=type events=events menuIdNum=menuIdNum treeMenuLibrary=treeMenuLibrary treeMenuData=data treeMenuSettings=settings treeMenuPlugins=plugins id=id attribs=attribs excludeAttribs=["class", "id", "style"] origArgs=origArgs passArgs=passArgs>
    <#if type == "lib-model">
      <#nested>
    <#else>
      <#-- DEV NOTE: TODO?: Currently this is following @menu code. however may want to invert this control/capture... -->
      <#if !(preItems?is_boolean && preItems == false)>
        <#if preItems?is_sequence>
          <#list preItems as item>
            <@treeitem args=item passArgs=passArgs />
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
            <@treeitem args=item passArgs=passArgs/>
          </#list>
        </#if>
        <#if !nestedFirst>
            <#nested>
        </#if>
      </#if>
      <#if !(postItems?is_boolean && postItems == false)>
        <#if postItems?is_sequence>
          <#list postItems as item>
            <@treeitem args=item passArgs=passArgs/>
          </#list>
        </#if>
      </#if>  
    </#if>
  </@treemenu_markup>
</#macro>

<#-- @treemenu main markup - theme override -->
<#macro treemenu_markup type="" items=[] events={} treeMenuLibrary="" treeMenuData={} treeMenuSettings={} treeMenuPlugins=[] id="" attribs={} excludeAttribs=[] origArgs={} passArgs={} catchArgs...>
    <#local events = toSimpleMap(events)>
    <#if treeMenuLibrary == "jstree">     
        <div id="${escapeVal(id, 'html')}"></div>
        <script type="text/javascript"> 
            jQuery(document).ready(function() {
              <#if type == "lib-model">   
                <#local treeMenuDataJson><@objectAsScript lang="json" object=treeMenuData /></#local>
                <#local nestedEvents><#nested></#local>
            
                jQuery("#${escapeVal(id, 'js')}")
                ${nestedEvents?trim}
                <#if events?has_content>
                  <#list mapKeys(events) as eventName>
                    .on("${escapeVal(eventName, 'js')}", function (e, data) {
                      ${events[rawString(eventName)]}
                    })
                  </#list>
                </#if>
                .jstree({
                    "core" : {
                        "data" : ${treeMenuDataJson}
                        <#if treeMenuSettings?has_content>
                           , <@objectAsScript lang="json" object=treeMenuSettings wrap=false />
                        </#if>
                     }
                     
                     <#if treeMenuPlugins?has_content>
                        <#list treeMenuPlugins as plugin>
                            , "${escapeVal(plugin.pluginName(), 'js')}" : <@objectAsScript lang="json" object=plugin />
                        </#list>
                        
                        , "plugins" : [
                            <#list treeMenuPlugins as plugin>
                                "${escapeVal(plugin.pluginName(), 'js')}"                               
                                <#if plugin_has_next>, </#if> 
                            </#list>
                        ]
                     </#if>

                });
              <#elseif type == "lib-basic">
                jQuery("#${escapeVal(id, 'js')}")
                <#if events?has_content>
                  <#list mapKeys(events) as eventName>
                    .on("${escapeVal(eventName, 'js')}", function (e, data) {
                      ${events[rawString(eventName)]}
                    })
                  </#list>
                </#if>
                .jstree({
                    
                    "core" : {
                        <#-- DEV NOTE: TODO: This control should probably be inverted (so that the listing happens here instead of #nested),
                            but it requires inverting a lot more -->
                        "data" : [<#nested>]
                        <#if treeMenuSettings?has_content>
                           , <@objectAsScript lang="json" object=toSimpleMap(treeMenuSettings) wrap=false />
                        </#if>
                    }
                    
                     <#if treeMenuPlugins?has_content>
                        <#list treeMenuPlugins as plugin>
                            , "${escapeVal(plugin.name, 'js')}" : <@objectAsScript lang="json" object=toSimpleMap(plugin.settings!{}) />
                        </#list>
                        
                        , "plugins" : [
                            <#list treeMenuPlugins as plugin>
                                "${escapeVal(plugin.name, 'js')}"                               
                                <#if plugin_has_next>, </#if> 
                            </#list>
                        ]
                     </#if>
                });
              </#if>
            });
        </script>
    </#if>
</#macro>

<#-- legacy events macro for lib-model tree menus 
    DEPRECATED: use @treemenu events arg instead -->
<#macro treemenu_event event="">
    <#if event?has_content>
        <#assign validEvents = Static["com.ilscipio.scipio.treeMenu.jsTree.JsTreeEvent"].VALID_EVENTS />        
        <#assign e = event?keep_before(Static["com.ilscipio.scipio.treeMenu.jsTree.JsTreeEvent"].JSTREE_EVENT) />        

        <#if validEvents?has_content && validEvents?seq_contains(e)>                       
            .on("${escapeVal(event, 'js')}", function (e, data) {
                <#nested>
            })
        </#if>
    </#if>
</#macro>

<#-- 
*************
* Tree Item
************
Renders a tree menu item.

Supports nested or flat format for hierarchy.

  * Parameters *
    id                      = Item ID
                              NOTE: Automatically added to attribs.
    attribs                 = ((map)|(inline)) Attributes for the item, either passed in this map, or inlined to this macro.
                              For jstree, these are: icon, id, text, state (map container: opened, selected), type,
                              li_attr (map), a_attr (map), etc. (see https://www.jstree.com/docs/json/ for full reference)
                              At least "text" must be specified.
                              "children" should be omitted and {{{items}}} arg used instead.
                              "parent" should be used along with "id" when using flat format for hierarchy, where root nodes
                              are marked using {{{parent="#"}}} attribute here (or using {{{isRoot=true}}} macro argument).
                              NOTE: The {{{attribs}}} map macro argument itself is not that useful, and is mainly for bypassing
                                  the macro arguments, if needed, for example when an attribute shares the name of a macro
                                  argument with a different function. But in most cases it is more appropriate to specify inline arguments to the macro
                                  or even use the {{{args}}} map argument, rather than the {{{attribs}}} map argument.
                                  Using the {{{attribs}}} map argument may provide less abstraction.
    isRoot                  = ((boolean)|"", default: "") Helper flag to indicate root nodes
                              This is mainly needed when using flat hierarchies.
                              For jstree, passing true is the same as specifying {{{parent="#"}}}.
                              Default is {{{""}}} which means unspecified.
                              NOTE: 2016-08-04: At current time, unspecified means no action is taken. Subject to change internally.
    items                   = ((list)) Children items: list of maps, where each hash contains arguments representing a menu item,
                              Same as @treeitem macro parameters.
                              Alternatively, the items can be specified as nested content.
    preItems                = ((list)) For children items: Special-case list of maps of items, added before items and nested content
                              Excluded from sorting.
                              Templates should generally avoid use unless specific need, but may be used by other macros.
    postItems               = ((list)) For children items: Special-case list of maps of items, added after items and nested content
                              Excluded from sorting.
                              Avoid use unless specific need; may be needed by scipio menu handling.
                              Templates should generally avoid use unless specific need, but may be used by other macros.
    sort,
    sortBy,
    sortDesc                = For children items: items sorting behavior; will only work if items are specified
                              through items list of hashes, currently does not apply to 
                              nested items. by default, sorts by text, or sortBy can specify a menu item arg to sort by.
                              normally case-insensitive.
    nestedFirst             = ((boolean), default: false) For children items: if true, use nested items before items list, otherwise items list always first
                              Usually should use only one of alternatives, but is versatile.
                              
  * Related *
    @treemenu
    
  * History *
    Added for 1.14.2.
-->
<#assign treeitem_defaultArgs = {
  "type":"", "nestedFirst":false, "attribs":{}, 
  "items":true, "preItems":true, "postItems":true, "sort":false, "sortBy":"", "sortDesc":false, "nestedFirst":false,
  "isRoot":"", "passArgs":{}
}>
<#macro treeitem args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.treeitem_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local attribs = makeAttribMapFromArgMap(args)>  
  
  <#local treeMenuLibrary = scipioTreeMenuLibrary!"jstree">
  
  <#local isFirst = scipioTreeMenuIsFirstItem>
  
  <#local itemIdNum = getRequestVar("scipioTreeItemIdNum")!0>
  <#local itemIdNum = itemIdNum + 1 />
  <#local dummy = setRequestVar("scipioTreeItemIdNum", itemIdNum)>
  <#-- no need for this, jstree will do it
  <#if !id?has_content>
    <#local id = "treeitem_" + itemIdNum><#- FIXME?: is this name too generic? ->
  </#if>
  -->

  <#local attribs = getFilteredAttribMap(attribs, [])>
  <#if id?has_content>
    <#local attribs = attribs + {"id":id}>
  </#if>
  
  <@treeitem_markup isFirst=isFirst isRoot=isRoot treeMenuLibrary=treeMenuLibrary id=id attribs=attribs itemIdNum=itemIdNum 
    defaultNodeIcon=(scipioTreeMenuDefNodeIcon!"") defaultDirIcon=(scipioTreeMenuDefDirIcon!"") origArgs=origArgs passArgs=passArgs>
      <#global scipioTreeMenuIsFirstItem = true>
      <#-- DEV NOTE: TODO?: Currently this is following @menu code. however may want to invert this control/capture... -->
      <#if !(preItems?is_boolean && preItems == false)>
        <#if preItems?is_sequence>
          <#list preItems as item>
            <@treeitem args=item passArgs=passArgs />
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
            <@treeitem args=item passArgs=passArgs/>
          </#list>
        </#if>
        <#if !nestedFirst>
            <#nested>
        </#if>
      </#if>
      <#if !(postItems?is_boolean && postItems == false)>
        <#if postItems?is_sequence>
          <#list postItems as item>
            <@treeitem args=item passArgs=passArgs/>
          </#list>
        </#if>
      </#if>
      <#global scipioTreeMenuIsFirstItem = false>
  </@treeitem_markup>

  <#global scipioTreeMenuIsFirstItem = false>
</#macro>

<#-- @treeitem main markup - theme override 
    NOTE: Unlike other macros, attribs here are already filtered. -->
<#macro treeitem_markup isFirst=false isRoot="" attribs={} treeMenuLibrary="" defaultNodeIcon="" defaultDirIcon="" origArgs={} passArgs={} catchArgs...>
  <#if !isFirst>, </#if>
  <#-- DEV NOTE: TODO?: Currently this is following @menu code. however may want to invert this control/capture... 
      this capture is especially dirty... -->
  <#local nestedContent><#nested></#local>
  <#local nestedContent = nestedContent?trim>
  <#if treeMenuLibrary == "jstree">
    <#if nestedContent?has_content>
      <#local children>[${nestedContent}]</#local>
      <#local attribs = (attribs + {"children":children})>
    </#if>
    <#if !attribs.icon?has_content>
      <#if nestedContent?has_content>
        <#if !defaultDirIcon?is_boolean && defaultDirIcon?has_content>
          <#local attribs = attribs + {"icon":defaultDirIcon}>
        </#if>
      <#else>
        <#if !defaultNodeIcon?is_boolean && defaultNodeIcon?has_content>
          <#local attribs = attribs + {"icon":defaultNodeIcon}>
        </#if>
      </#if>
    </#if>
    <#if !attribs.parent?? && isRoot?is_boolean && isRoot == true>
      <#local attribs = attribs + {"parent":"#"}>
    </#if>
    <@objectAsScript lang="json" object=attribs rawVal={"children":true} />
  </#if>
</#macro>
