<#--
* 
* Structural code (grid, tile, etc.) HTML template include, default Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#--
*************
* Row Macro
************
    Usage example:  
    <@row attr="" >
        <@cell attr=""/>
    </@row>              
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    alt             = boolean, if true alternate row (odd), if false regular (even)
    selected        = boolean, if true row is marked selected
-->
<#macro row class=true id="" collapse=false norows=false alt="" selected="">
    <#if !norows>
      <#local classes = makeClassesArg(class, "")>
      <#local altClass = "">
      <#if alt?is_boolean>
        <#local altClass = alt?string(styles.row_alt!, styles.row_reg!)>
      </#if>
      <#if selected?is_boolean && selected == true>
        <#local altClass = (altClass + " " + styles.row_selected!)?trim>
      </#if>
    <div class="${styles.grid_row!}<#if classes?has_content> ${classes}</#if><#if collapse> collapse</#if><#if altClass?has_content> ${altClass}</#if>"<#if id?has_content> id="${id}"</#if>><#rt/>
    </#if>
        <#nested />
    <#if !norows>    
    </div>
    </#if>    
</#macro>

<#-- 
*************
* Cell Macro
************
    Usage examples:  
    <@row attr="" >
        <@cell attr="">
            cell content goes in here!
        </@cell>
    </@row>  
    
    <@row>
        <@cell>
            cell content goes in here!
        </@cell>
    </@row>   
    
    <@row>
        <@cell columns=3>
            cell content goes in here!
        </@cell>
    </@row> 
    
    <@row>
        <@cell columns=3 small=2 class="mycell">
            cell content goes in here!
        </@cell>
    </@row>            
                    
   * General Attributes *
    class           = css classes (if column sizes specified, adds classes; if no column sizes specified, expected to contain manual column sizes and overrides columns size default)
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    columns         = expected number of columns to be rendered (specify as number, default 12, default only used if class empty and no column sizes specified)
    small           = specific number of small columns (specify as number), overrides small part general columns value above
    large           = specific number of large columns (specify as number), overrides large part of general columns value above
    medium          = specific number of medium columns (specify as number), overrides medium part of general columns value above
    offset          = offset in number of columns
    smallOffset     = specific offset for small columns
    mediumOffset    = specific offset for medium columns
    largeOffset     = specific offset for large columns
    last            = boolean, usually optional, if true indicate last cell in row 
-->
<#macro cell columns=-1 small=-1 medium=-1 large=-1 offset=-1 smallOffset=-1 mediumOffset=-1 largeOffset=-1 class=true id="" collapse=false nocells=false last=false>
    <#local addClass = parseAddClassArg(class)>
    <#local class = parseClassArg(class, "")>  
     
    <#local columns = columns?number>
    <#local small = small?number>
    <#local medium = medium?number>
    <#local large = large?number>
    <#local offset = offset?number>
    <#local smallOffset = smallOffset?number>
    <#local mediumOffset = mediumOffset?number>
    <#local largeOffset = largeOffset?number>
    
    <#if !nocells>
        <#local specColsClasses><#if (small > 0)> ${styles.grid_small!}${small}</#if><#if (medium > 0)> ${styles.grid_medium!}${medium}</#if><#if (large > 0)> ${styles.grid_large!}${large}<#elseif (large != 0) && (columns > 0)> ${styles.grid_large!}${columns}</#if></#local>
        <#if class?has_content>
            <#local colSizeClasses = (class + specColsClasses)?trim>
        <#else>
            <#local colSizeClasses = specColsClasses?trim>
        </#if>
        <#if !colSizeClasses?has_content>
            <#local colSizeClasses = "${styles.grid_large!}12">
        </#if>
        <#local specOffsetClassesStr><#if (smallOffset > 0)> ${styles.grid_small_offset!}${smallOffset}</#if><#if (mediumOffset > 0)> ${styles.grid_medium_offset!}${mediumOffset}</#if><#if (largeOffset > 0)> ${styles.grid_large_offset!}${largeOffset}<#elseif (largeOffset != 0) && (offset > 0)> ${styles.grid_large_offset!}${offset}</#if></#local>
        <div class="${colSizeClasses}${specOffsetClassesStr} ${styles.grid_cell!}<#if last> ${styles.grid_end!}</#if><#if addClass?has_content> ${addClass}</#if>" <#if id?has_content> id="${id}"</#if>><#rt/>
    </#if>
        <#nested />
    <#if !nocells></div></#if>
</#macro>


<#-- 
*************
* Grid list
************
Since this is very foundation specific, this function may be dropped in future installations

    Usage example:  
    <@grid>
        <li>Text or <a href="">Anchor</a></li>
    </@grid>            
                    
   * General Attributes *
    class           = Adds classes - please use "${styles.grid_block_prefix!}(small|medium|large)${styles.grid_block_postfix!}#"
                      (FIXME: prepend with "+" to append only, i.e. never replace non-essential defaults)
    columns         = Number of columns (default 5)
    type            = (tiles|) default:empty
    
-->
<#macro grid type="" class=true columns=4>
    <#if type=="tiles" || type="freetiles">
        <#local freewallNum = getRequestVar("catoFreewallIdNum")!0>
        <#local freewallNum = freewallNum + 1 />
        <#local dummy = setRequestVar("catoFreewallIdNum", freewallNum)>
        <#local id="freewall_id_${freewallNum!0}">
        <#-- FIXME: the "class" arg is not even used... 
        <#local classes = makeClassesArg(class, "...")>
        -->
        <div class="${styles.tile_container!}" id="${id!}">
            <#nested>
        </div>
        <script type="text/javascript">
        //<![CDATA[
         $(function() {
            $('#${id}').freetile({
                selector: '.${styles.tile_wrap!}'
            });
            <#--
            Alternative implementation of gridster.js
            $('#${id}').gridster({
                widget_selector: '.${styles.tile_wrap!}',
                min_cols:${columns},
                autogenerate_stylesheet:false
            }).disable();
            -->
         });
        //]]>
        </script>
    <#else>
        <#-- FIXME: the "class" arg is not even used...
        <#local classes = makeClassesArg(class, "...")>
        -->
        <#local defaultClass="${styles.grid_block_prefix!}${styles.grid_small!}${styles.grid_block_postfix!}2 ${styles.grid_block_prefix!}${styles.grid_medium!}${styles.grid_block_postfix!}4 ${styles.grid_block_prefix!}${styles.grid_large!}${styles.grid_block_postfix!}5">
            <#if (columns-2 > 0)>
                <#local class="${styles.grid_block_prefix!}${styles.grid_small!}${styles.grid_block_postfix!}${columns-2} ${styles.grid_block_prefix!}${styles.grid_medium!}${styles.grid_block_postfix!}${columns-1} ${styles.grid_block_prefix!}${styles.grid_large!}${styles.grid_block_postfix!}${columns}"/>
            <#else>
                <#local class="${styles.grid_block_prefix!}${styles.grid_large!}${styles.grid_block_postfix!}${columns}"/>
            </#if>
          <ul class="${class!defaultClass!}">
              <#nested>
          </ul>
    </#if>
</#macro>


<#-- 
*************
* Tile
************
Creates a very basic wrapper for tiles (can be used in metro designs).
Please be aware that this is neither based on standard bootstrap, nor foundation. 
It is loosely based on http://metroui.org.ua/tiles.html 

    Usage example:  
    <@tile type="small">
       // content
    </@tile>
                    
   * General Attributes *
    type            = (small|normal|wide|large|big|super) (default:normal)
    title           = Title
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    link            = Link URL
    id              = field id
    color           = (0|1|2|3|4|5|6|7) defaul:0 (empty)   
    icon            = Set icon code (http://zurb.com/playground/foundation-icon-fonts-3)
    image           = Set a background image-url (icon won't be shown if not empty)
-->
<#macro tile type="normal" title="" class=true id="" link="" color=0 icon="" image="">
    <#local classes = makeClassesArg(class, "")>
    <#local nested><#nested></#local>
    <div class="${styles.tile_wrap!} ${styles.tile_wrap!}-${type!}<#if classes?has_content> ${classes}</#if> ${styles.tile_color!}${color!}"<#if id?has_content> id="${id!}"</#if> data-sizex="${calcTileSize("x",type!)}" data-sizey="${calcTileSize("y",type!)}">
        <#if image?has_content><div class="${styles.tile_image!}" style="background-image: url(${image!})"></div></#if>
        <div class="${styles.tile_content!}">
            <#if link?has_content><a href="${link!}"></#if>
            <#if icon?has_content && !icon?starts_with("AdminTileIcon") && !image?has_content><span class="${styles.tile_icon!}"><i class="${icon!}"></i></span></#if>
            <#if nested?has_content><span class="${styles.tile_overlay!}"><#nested></span></#if>
            <#if title?has_content><span class="${styles.tile_title!}">${title!}</span></#if>
            <#if link?has_content></a></#if>
        </div>
    </div>  
</#macro>

<#function calcTileSize type="x" value="normal">
    <#local tileSizeX={"small":0,"normal":1,"wide":2,"large":2,"big":3,"super":4}/>
    <#local tileSizeY={"small":0,"normal":1,"wide":1,"large":2,"big":3,"super":4}/>
    <#if type="x">
        <#return tileSizeX[value]/>
    <#else>
        <#return tileSizeY[value]/>
    </#if>
</#function>

<#-- 
*************
* Section Macro
************
Creates a logical section with optional title and menu. Automatically handles heading sizes
and keeps track of section nesting for whole request, even across screens.render calls.

NOTE: use getCurrentHeadingLevel and getCurrentSectionLevel functions if need to get current
levels manually, but most often should let @section menu handle them.

IMPL NOTE: This has dependencies on some non-structural macros.

    Usage example:  
    <@section attr="">
        Inner Content
    </@section>            
                    
   * General Attributes *
    type                = [generic], default generic
    class               = css classes, on outer columns element (affects title)
                          (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
                          note: boolean false has no effect here
    id                  = set id
    title               = section title
    titleClass          = section title class (supports complex expressions; rarely needed, usually headingLevel enough)
    padded              = 
    autoHeadingLevel    = auto increase heading level when title present (enabled by default)
    headingLevel        = force this heading level for title. if autoHeadingLevel true, also influences nested elems (even if no title here, but if no title won't consume a size).
    relHeadingLevel     = increase heading level by this number
    defaultHeadingLevel = default heading level (same as headingLevel if autoHeadingLevel false)
                          if empty string, use getDefaultHeadingLevel()
    menuContent         = optional; a macro, hash/map of @menu args, or string html markup of li elements only defining a menu
                          as macro:
                            should accept one argument named "menuArgs" which is default @section menu args that can be passed
                            directly to @menu by caller, e.g.:
                            <#macro myMenuContent menuArgs>
                              <@menu args=menuArgs>
                                <@menuitem ... />
                                <@menuitem ... />
                              </@menu>
                            </#macro>
                            the caller can override the section menu args as needed.
                          as hash/map:
                            these will simply be passed as <@menu args=menuContent /> by @section, 
                            with defaults added underneath (but user-passed args override).
                          as string/html:
                            should be <li> elements only, generated manually or using <@menu type="section" ... inlineItems=true>.
                            WARN: if using @menu to pre-generate the menu as string/html, the menu arguments such as "type" are lost and 
                            assumed to be "section" or "section-inline".
    optional menu data or markup, li elements only (ul auto added)
    menuClass           = menu class, default is section menu (FIXME: doesn't really mean anything at the moment). "none" prevents class. this is a low-level control; avoid if possible (FIXME: currently impossible to avoid).
    menuLayout          = [post-title|pre-title|inline-title], default post-title. this is a low-level control; avoid if possible (FIXME: currently impossible to avoid).
    menuRole            = "nav-menu" (default), "paginate-menu"
    requireMenu         = if true, add menu elem even if empty
    forceEmptyMenu      = if true, always add menu and must be empty
    hasContent          = minor hint, optional, default true, when false, to add classes to indicate content is empty or treat as logically empty (workaround for no css :blank and possibly other)
-->
<#macro section type="" id="" title="" class=true padded=false autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel="" menuContent="" menuClass="" menuLayout="" menuRole="nav-menu" requireMenu=false forceEmptyMenu=false hasContent=true titleClass="" openOnly=false closeOnly=false wrapIf=true>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
    <#if !type?has_content>
        <#local type = "generic">
    </#if>
    <#local classes = makeClassesArg(class, "")>
    <#if id?has_content>
        <#local contentId = id + "_content">
        <#local menuId = id + "_menu">
    <#else>
        <#local contentId = "">
        <#local menuId = "">
    </#if>
<#else>
    <#-- section_impl has its own stack, don't need to preserve these for now-->
    <#local class = "">
    <#local contentId = "">
    <#local menuId = "">    
</#if>
    <#-- note: addClass logic is only partially implemented (doesn't support booleans and "" means use default; otherwise may conflict with stock API?), but good enough for now -->
    <#-- note: autoHeadingLevel logic now implemented in renderScreenletBegin -->
    <@section_impl id=id collapsibleAreaId=contentId title=title classes=classes padded=padded menuContent=menuContent fromWidgets=false menuClass=menuClass menuId=menuId menuLayout=menuLayout menuRole=menuRole requireMenu=requireMenu 
        forceEmptyMenu=forceEmptyMenu hasContent=hasContent autoHeadingLevel=autoHeadingLevel headingLevel=headingLevel relHeadingLevel=relHeadingLevel defaultHeadingLevel=defaultHeadingLevel titleStyle=titleClass 
        openOnly=openOnly closeOnly=closeOnly wrapIf=wrapIf>
        <#nested />
    </@section_impl>
</#macro>

<#-- 
    migrated from @renderScreenletBegin/End screen widget macro
    DEV NOTE: section_impl and similar macros ARE NOT a final implementation pattern. 
        it was created initially strictly to remove dependency of cato libs on ofbiz macro library,
        and to head toward separating macro logic and markup.
    TODO: refinement, clean up macro arguments and dissect further
    FIXME?: menuClass does not obey class arg rules, not sure if should here      
          
    fromWidgets: hint of whether called by renderer or ftl macros
    hasContent: hint to say there will be content, workaround for styling -->
<#macro section_impl id="" title="" classes="" collapsible=false saveCollapsed=true collapsibleAreaId="" expandToolTip=true collapseToolTip=true fullUrlString="" padded=false menuContent="" showMore=true collapsed=false 
    javaScriptEnabled=true fromWidgets=true menuClass="" menuId="" menuLayout="" menuRole="" requireMenu=false forceEmptyMenu=false hasContent=true titleStyle="" titleContainerStyle="" titleConsumeLevel=true 
    autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel="" openOnly=false closeOnly=false wrapIf=true>

<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>

<#-- level logic begin -->
    <#-- note: request obj only available because of macro renderer initial context mod -->
    <#local sLevel = getCurrentSectionLevel()>
    <#local prevSectionLevel = sLevel>
    <#local dummy = setCurrentSectionLevel(sLevel+1)>
<#-- level logic end -->

<#-- title-style parsing begin -->
    <#-- titleContainerStyle can be inlined as prefix in titleStyle, separated by ;, as workaround 
         full string can look like:
           e.g. titleStyle="div:+containerClass;h4:+titleClass", titleStyle="div;h5"
                titleStyle="div;h+1;consumeLevel=true"-->
  <#if titleStyle?has_content>
    <#local titleStyleArgs = getHeadingElemSpecFromStyleStr(titleStyle, titleContainerStyle,
        "h|heading","div|span|p|raw", "div", "widget-screenlet")>

    <#-- overrides (so style from screen affects heading calc and consume) -->
    <#if titleStyleArgs.level?has_content>
      <#local headingLevel = titleStyleArgs.level?number>
    </#if>
    <#if titleStyleArgs.relLevel?has_content>
      <#local relHeadingLevel = titleStyleArgs.relLevel?number>
    </#if>
    <#local titleConsumeLevel = translateStyleStrBoolArg(titleStyleArgs.consumeLevel!"")!true>
  <#else>
    <#local titleStyleArgs = {}>
  </#if>
    <#local titleElemType = translateStyleStrClassesArg(titleStyleArgs.elemType!"")!true>
    <#local titleClass = translateStyleStrClassesArg(titleStyleArgs.elemClass!"")!true>
    <#local titleContainerElemType = translateStyleStrClassesArg(titleStyleArgs.containerElemType!"")!false>
    <#local titleContainerClass = translateStyleStrClassesArg(titleStyleArgs.containerElemClass!"")!true>

<#-- title-style parsing end -->

<#-- auto-heading-level logic begin -->
    <#if !defaultHeadingLevel?is_number>
      <#local defaultHeadingLevel = getDefaultHeadingLevel()>
    </#if>

    <#local explicitHeadingLevel = false>
    <#local updatedHeadingLevel = false> <#-- just so consistent -->
    <#local prevHeadingLevel = "">
    <#if autoHeadingLevel>
        <#local prevHeadingLevel = getCurrentHeadingLevel(false)!"">
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
      <#if titleConsumeLevel>
        <#if title?has_content>
            <#local dummy = setCurrentHeadingLevel(hLevel + 1)>
            <#local updatedHeadingLevel = true>
        <#elseif explicitHeadingLevel>
            <#-- set here but don't increase if none title -->
            <#local dummy = setCurrentHeadingLevel(hLevel)>
            <#local updatedHeadingLevel = true>
        </#if>
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
    <#local dummy = pushRequestStack("renderScreenletStack", {"autoHeadingLevel":autoHeadingLevel, "updatedHeadingLevel":updatedHeadingLevel, "prevHeadingLevel":prevHeadingLevel, "prevSectionLevel":prevSectionLevel})>
<#-- auto-heading-level logic end -->

    <#-- Cato: we support menuContent as string (html), macro or hash definitions
        when string, menuContent is not wrapped in UL when it's received here from macro renderer... 
        note: with recent patch, menuContent passed by renderer is rendered by macro renderer (was not the case before - used old html renderer). -->
    <#if isObjectType("string", menuContent)> <#-- dev note warn: ?is_string would not always work here -->
      <#local menuContent = menuContent?trim>
    </#if>
    <#local hasMenu = (menuContent?is_directive || menuContent?has_content || requireMenu || forceEmptyMenu)>
    <#local hasTitle = title?has_content>

  <#if showMore>
    <#if hasMenu>
      <#local menuMarkup>
      <#-- temporarily (?) unnecessary; all use styles.button_group and hacks moved
           menuRole and widgetRender were mostly to figure out the context to apply defaults at the time.
           don't remove.
      <#local screenletPaginateMenu = (menuRole == "paginate-menu") && widgetRender>
      <#local screenletNavMenu = (menuRole == "nav-menu") && widgetRender>
      <#local ftlNavMenu = (menuRole == "nav-menu") && !widgetRender>
      -->

      <#if menuLayout == "inline-title">
        <#local defaultMenuType = "section-inline">
      <#else>
        <#local defaultMenuType = "section">
      </#if>

      <#if forceEmptyMenu>
        <#local preMenuItems = []>
        <#local postMenuItems = []>
      <#else>
        <#-- cato: TODO: translate this into vars below if/once needed again (as @menuitem args maps within lists)
        <#local preMenuItems></#local>
        <#local postMenuItems>
          <#if menuLayout != "pre-title" && menuLayout != "inline-title">
          <#if collapsible>
          <li class="<#rt/>
          <#if collapsed>
          collapsed"><a <#if javaScriptEnabled>onclick="javascript:toggleScreenlet(this, '${collapsibleAreaId}', '${saveCollapsed?string}', '${expandToolTip}', '${collapseToolTip}');"<#else>href="${fullUrlString}"</#if><#if expandToolTip?has_content> title="${expandToolTip}"</#if>
          <#else>
          expanded"><a <#if javaScriptEnabled>onclick="javascript:toggleScreenlet(this, '${collapsibleAreaId}', '${saveCollapsed?string}', '${expandToolTip}', '${collapseToolTip}');"<#else>href="${fullUrlString}"</#if><#if collapseToolTip?has_content> title="${collapseToolTip}"</#if>
          </#if>
          >&nbsp;</a></li>
          </#if>
          </#if>
        </#local>
        -->
        <#local preMenuItems = []>
        <#local postMenuItems = []>        
      </#if>

      <#if !menuId?has_content && id?has_content>
        <#local menuId = "${id}_menu">
      </#if>
    
      <#if menuId?has_content>
        <#local menuIdArg = menuId>
      <#else>
        <#local menuIdArg = "">
      </#if>
      <#if menuClass?has_content> <#-- note: don't use defaultMenuClass here; @menu will figure it out instead -->
        <#if menuClass == "none">
          <#local menuClassArg = false>
        <#else>
          <#local menuClassArg = menuClass>
        </#if>
      <#else>
        <#local menuClassArg = true>
      </#if>
    
      <#if menuContent?is_directive || isObjectType("map", menuContent)>
        <#-- as callback macro, or menu definition in map format -->
         
        <#-- inlineItems false; let caller's macro produce the wrapper (must because we don't know the real menu type from here), 
             or if map def, produce the wrapper through our call -->
        <#local menuArgs = {"type":defaultMenuType, "inlineItems":false, 
            "preItems":preMenuItems, "postItems":postMenuItems, 
            "id":menuIdArg, "class":menuClassArg}>
        
        <#local overrideArgs = "">
        <#if forceEmptyMenu>
          <#local overrideArgs = {"items":false, "preItems":false, "postItems":false}>
        </#if>
        
        <#if menuContent?is_directive>
          <#if overrideArgs?has_content>
            <#local menuArgs = concatMaps(menuArgs, overrideArgs)>
          </#if>
          <#-- menuArgs: caller macro can simply pass these through using <@menu args=menuArgs />, or override/modify as desired -->
          <@menuContent menuArgs=menuArgs /> 
        <#else>
          <#-- simply concat user defs over our default args -->
          <#local menuArgs = concatMaps(menuArgs, menuContent)>
          <#if overrideArgs?has_content>
            <#local menuArgs = concatMaps(menuArgs, overrideArgs)>
          </#if>
          <@menu args=menuArgs />  
        </#if>
      <#elseif isObjectType("string", menuContent)>
        <#-- legacy menuString; these have limitations but must support because used by screen widgets (e.g. renderScreenletPaginateMenu) 
             and our code -->

        <#-- note: menuContent shouldn't contain <ul because li may be added here (traditionally), but check anyway, may have to allow 
             FIXME: this check is hardcoded to detect <li> elems... should not assume this is what starts menu elems, but this is getting difficult... -->
        <#local menuItemsInlined = menuContent?matches(r'(\s*<!--((?!<!--).)*?-->\s*)*\s*<li(\s|>).*', 'rs')>

        <#local menuItemsMarkup>
          <#if !forceEmptyMenu>
            ${menuContent}
          </#if>
        </#local>
    
        <#if !menuContent?has_content || menuItemsInlined>
          <#-- WARN: we have to assume the menu type here (especially for pre/postMenuItems); inherently limited -->
          <@menu type=defaultMenuType inlineItems=false id=menuIdArg class=menuClassArg preMenuItems=preMenuItems postMenuItems=postMenuItems>
            ${menuItemsMarkup}
          </@menu>
        <#else>
          <#-- menuContent already contains UL (or other wrapper); this is for compatibility only; should be avoided 
               WARN: preMenuItems and postMenuItems can't be applied here (without more ugly parsing) -->
          ${menuItemsMarkup}
        </#if>
      </#if>
    </#local>
    </#if>
    
    <#if hasTitle>
      <#local titleMarkup>
        <@heading level=hLevel elemType=titleElemType class=titleClass containerElemType=titleContainerElemType containerClass=titleContainerClass>${title}</@heading>
      </#local>
    </#if> 
  </#if>

    <#local contentFlagClasses>section-level-${sLevel} heading-level-${hLevel}<#if hasTitle> has-title<#else> no-title</#if><#if hasMenu> has-menu<#else> no-menu</#if><#if hasContent> has-content<#else> no-content</#if></#local>
</#if> <#-- /#(if open) -->

    <#-- TODO: in this whole section I have for now only passed the args to markup macros that were needed and 
         were in acceptable form as cato args; they should probably receive more; but avoid ofbiz-isms -->

    <#if open && !close>
        <#-- save stack of all the args passed to markup macros that have open/close 
            so they don't have to remember a stack themselves -->
        <#local dummy = pushRequestStack("renderScreenletMarkupStack", {"classes":classes, "contentFlagClasses":contentFlagClasses, 
            "id":id, "title":title, "collapsed":collapsed, "collapsibleAreaId":collapsibleAreaId,
            "sLevel":sLevel, "hLevel":hLevel})>
    <#elseif close && !open>
        <#-- these _must_ override anything passed to this macro call (shouldn't be any) -->
        <#local stackValues = popRequestStack("renderScreenletMarkupStack")!{}>
        <#local classes = stackValues.classes>
        <#local contentFlagClasses = stackValues.contentFlagClasses>
        <#local id = stackValues.id>
        <#local title = stackValues.title>
        <#local collapsed = stackValues.collapsed>
        <#local collapsibleAreaId = stackValues.collapsibleAreaId>
        <#local sLevel = stackValues.sLevel>
        <#local hLevel = stackValues.hLevel>    
    </#if>

    <@section_markup_outer open=open close=close sectionLevel=sLevel headingLevel=hLevel classes=classes contentFlagClasses=contentFlagClasses id=id title=title collapsed=collapsed>

    <#if open>
      <#if showMore>
        <@section_markup_menutitle sectionLevel=sLevel headingLevel=hLevel menuLayout=menuLayout hasMenu=hasMenu menuMarkup=menuMarkup hasTitle=hasTitle titleMarkup=titleMarkup contentFlagClasses=contentFlagClasses />
      </#if>
    </#if> <#-- /#(if open) -->

        <@section_markup_inner open=open close=close sectionLevel=sLevel headingLevel=hLevel classes="" contentFlagClasses=contentFlagClasses collapsibleAreaId=collapsibleAreaId>
            
            <#-- nested content -->
            <#nested>

        </@section_markup_inner>

    </@section_markup_outer>

<#if close>
<#-- auto-heading-level logic begin -->
    <#local stackValues = popRequestStack("renderScreenletStack")!{}>
    
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
</#if> <#-- /#(if close) -->
</#macro>

<#-- @section outer container markup - may be overridden -->
<#macro section_markup_outer open=true close=true sectionLevel=1 headingLevel=1 classes="" contentFlagClasses="" id="" title="" collapsed=false extraArgs...>
  <#if open>
    <div class="section-screenlet<#if contentFlagClasses?has_content> ${contentFlagClasses}</#if><#if collapsed> toggleField</#if>">
        <#if collapsed><p class="alert legend">[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] ${title}</p></#if>
        <div class="${styles.grid_row!}"<#if id?has_content> id="${id}"</#if>>
            <div class="section-screenlet-container<#if classes?has_content> ${classes}<#else> ${styles.grid_large!}12</#if> ${styles.grid_cell!}<#if contentFlagClasses?has_content> ${contentFlagClasses}</#if>">
  </#if>
    <#nested>
  <#if close>
            </div>
        </div>
    </div>
  </#if>
</#macro>

<#macro section_markup_menutitle sectionLevel=1 headingLevel=1 menuLayout="" hasMenu=false menuMarkup="" hasTitle=false titleMarkup="" contentFlagClasses="" extraArgs...>
    <#-- Currently supports only one menu. could have one for each layout (with current macro
         args as post-title), but tons of macro args needed and complicates. -->
    <#if menuLayout == "pre-title">
      <#if hasMenu>
        ${menuMarkup}
      </#if>
      <#if hasTitle>
        ${titleMarkup}
      </#if>
    <#elseif menuLayout == "inline-title">
      <div class="${styles.float_clearfix!}">
        <div class="${styles.float_left!}">
          <#if hasTitle>
            ${titleMarkup}
          </#if>
        </div>
        <div class="${styles.float_right!}">
          <#if hasMenu>
            ${menuMarkup}
          </#if>
        </div>
      </div>
    <#else>
      <#if hasTitle>
        ${titleMarkup}
      </#if>
      <#if hasMenu>
        ${menuMarkup}
      </#if>
    </#if>
</#macro>

<#-- @section inner container markup - may be overridden -->
<#macro section_markup_inner open=true close=true sectionLevel=1 headingLevel=1 classes="" contentFlagClasses="" collapsibleAreaId="" extraArgs...>
  <#if open>
    <#-- note: may need to keep this div free of foundation grid classes (for margins collapse?) -->
    <div<#if collapsibleAreaId?has_content> id="${collapsibleAreaId}"</#if> class="section-screenlet-content<#if classes?has_content> ${classes}</#if><#if contentFlagClasses?has_content> ${contentFlagClasses}</#if>">
  </#if>
    <#nested>
  <#if close>
    </div>
  </#if>
</#macro>



