<#--
* 
* The Grid
*
* Displaying content within a (flexible) grid is a way of structuring content on a website.
* The grid itself works as a layer on top of the basic html structure. Everything visible is grouped inside 
* columns and individual cells. The SCIPIO framework uses the standard 12 column grid that is used by  
* modern CSS frameworks, such as foundation or bootstrap.
* 
* This template contains structural code (grid, tile, etc.) that is used by all SCIPIO standard HTML markup.
* Included by htmlTemplate.ftl.
*
* NOTES: 
* * May have implicit dependencies on other parts of Scipio API.
*
-->

<#-- 
*************
* Container
************
Simple container (whether used as grid element or not); basically macro version of HTML div element.
This serves to add various hooks that may be needed around regular containers.

For grid usage, @row/@cell should be preferred.

Currently, this will scan classes for @cell-related grid sizes. this macro should always be
used instead of div if the div contains grid size classes (but conversely, @cell should then be preferred 
to this one.
                    
  * Parameters *
    class                   = ((css-class)) CSS classes
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
                              NOTE: This type of element is expected to have no default classes. 
    id                      = Container ID
    attribs                 = ((map)) Other attributes for div 
                              Needed for names containing dashes.
    inlineAttribs...        = ((inline-args)) Extra attributes for div
                              NOTE: camelCase names are automatically converted to dash-separated-lowercase-names.
-->
<#assign container_defaultArgs = {
  "class":"", "id":"", "open":true, "close":true, "elem":"", "attribs":{}, "passArgs":{}
}>
<#macro container args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.container_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if open>
    <#local attribs = makeAttribMapFromArgMap(args)>
    <#if !elem?has_content || elem == "container">
      <#local elem = "div">
    </#if>
    <#if !close>
      <#local dummy = pushRequestStack("scipioContainerStack", {
        "class":class, "elem":elem, "id":id, "attribs":attribs, "origArgs":origArgs, "passArgs":passArgs
      })>
    </#if>
  <#elseif close>
    <#local stackValues = popRequestStack("scipioContainerStack")!{}>
    <#local dummy = localsPutAll(stackValues)>
  </#if>
  <#if open>
    <#-- NOTE: currently, no stack needed; simple -->
    <#-- save grid sizes (can simply assume this is a cell; saveCurrentContainerSizesFromStyleStr will be okay with it) -->
    <#local dummy = saveCurrentContainerSizesFromStyleStr(class)>
    <${elem}<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=["class", "id"]/></#if>><#rt>
  </#if>
      <#nested><#t>
  <#if close>
    </${elem}><#lt>
    <#-- pop grid sizes -->
    <#local dummy = unsetCurrentContainerSizes()>
  </#if>
</#macro>

<#--
*************
* Row
************
Creates a grid row.

  * Usage Examples *  
    <@row attr="" >
        <@cell attr=""/>
    </@row>              
                    
  * Parameters *
    class                   = ((css-class)) CSS classes 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    alt                     = ((boolean), default: false) If true alternate row (odd), if false regular (even)
    id                      = Row ID
    style                   = Legacy HTML {{{style}}} attribute
    selected                = ((boolean), default: false) If true row is marked selected
    attribs                 = ((map)) Extra  attributes
                              Needed for names containing dashes.
    inlineAttribs...        = ((inline-args)) Extra  attributes
                              NOTE: camelCase names are automatically converted to dash-separated-lowercase-names.
-->
<#assign row_defaultArgs = {
  "class":"", "id":"", "style":"", "collapse":false, "norows":false, "alt":"", "selected":"", "open":true, "close":true, 
  "attribs":{}, "passArgs":{}
}>
<#macro row args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.row_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  
  <#local open = open && !norows>
  <#local close = close && !norows>
  <#if open>
    <#local attribs = makeAttribMapFromArgMap(args)>
    <#if alt?is_boolean>
      <#local class = addClassArg(class, alt?string(styles.row_alt!, styles.row_reg!))>
    </#if>
    <#if selected?is_boolean && selected == true>
      <#local class = addClassArg(class, styles.row_selected!)>
    </#if>
    <#local class = addClassArg(class, styles.grid_row!)>
    <#if collapse>
      <#local class = addClassArg(class, styles.collapse!)>
    </#if>
  </#if>

  <#if open && !close>
    <#local dummy = pushRequestStack("scipioRowMarkupStack", {
      "class":class, "collapse":collapse, "id":id, "style":style, "alt":alt, "selected":selected, 
      "attribs":attribs, "origArgs":origArgs, "passArgs":passArgs
    })>
  <#elseif close && !open>
    <#local stackValues = popRequestStack("scipioRowMarkupStack")!{}>
    <#local dummy = localsPutAll(stackValues)>
  </#if>
  <@row_markup open=open close=close class=class collapse=collapse id=id style=style alt=alt selected=selected 
    attribs=attribs origArgs=origArgs passArgs=passArgs><#nested /></@row_markup>
</#macro>

<#-- @row container markup - theme override -->
<#macro row_markup open=true close=true class="" collapse=false id="" style="" alt="" selected="" 
    attribs={} excludeAttribs=[] origArgs={} passArgs={} catchArgs...>
  <#if open>
    <div<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#rt>
        <#lt><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>><#rt/>
  </#if>
      <#nested />
  <#if close>
    </div>
  </#if> 
</#macro>

<#-- 
*************
* Cell
************
Creates a grid cell.

  * Usage Examples * 
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
        <@cell columns=3 small=2 class="+my-cell">
            cell content goes in here!
        </@cell>
    </@row>            
                    
  * Parameters *
    class                   = ((css-class)) CSS classes
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    id                      = Cell ID
    style                   = Legacy HTML {{{style}}} attribute
    columns                 = ((int), default: 12) Expected number of columns to be rendered 
                              Default only used if class empty and no column sizes specified.
    small                   = ((int)) Specific number of small columns, overrides small part general columns value above
    large                   = ((int)) Specific number of large columns, overrides large part of general columns value above
    medium                  = ((int)) Specific number of medium columns, overrides medium part of general columns value above
    offset                  = ((int)) Offset in number of columns
    smallOffset             = ((int)) Specific offset for small columns
    mediumOffset            = ((int)) Specific offset for medium columns
    largeOffset             = ((int)) Specific offset for large columns
    last                    = ((boolean)) If true indicates last cell in row 
                              NOTE: This is often optional in CSS frameworks; affects float alignment.
    attribs                 = ((map)) Extra  attributes
                              Needed for names containing dashes.
    inlineAttribs...        = ((inline-args)) Extra  attributes
                              NOTE: camelCase names are automatically converted to dash-separated-lowercase-names.
-->
<#assign cell_defaultArgs = {
  "columns":-1, "small":-1, "medium":-1, "large":-1, "offset":-1, "smallOffset":-1, "mediumOffset":-1, 
  "largeOffset":-1, "class":"", "id":"", "style":"", "collapse":false, "nocells":false, "last":false, 
  "attribs":{}, "open":true, "close":true, "passArgs":{}
}>
<#macro cell args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.cell_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  
  <#local open = open && !nocells>
  <#local close = close && !nocells>
  <#if open>
    <#local attribs = makeAttribMapFromArgMap(args)>
    <#local columns = columns?number>
    <#local small = small?number>
    <#local medium = medium?number>
    <#local large = large?number>
    <#local offset = offset?number>
    <#local smallOffset = smallOffset?number>
    <#local mediumOffset = mediumOffset?number>
    <#local largeOffset = largeOffset?number>
    
    <#local specColsClasses><#if (small > 0)> ${styles.grid_small!}${small}</#if><#if (medium > 0)> ${styles.grid_medium!}${medium}</#if><#if (large > 0)> ${styles.grid_large!}${large}<#elseif (large != 0) && (columns > 0)> ${styles.grid_large!}${columns}</#if></#local>
    <#-- special case: use Replacing so that if spec col classes present, will turn into replacing class string that will prevent next default -->
    <#local class = addClassArgReplacing(class, specColsClasses)>
    <#local class = addClassArgDefault(class, "${styles.grid_cell_default!}")>

    <#local specOffsetClassesStr><#if (smallOffset > 0)> ${styles.grid_small_offset!}${smallOffset}</#if><#if (mediumOffset > 0)> ${styles.grid_medium_offset!}${mediumOffset}</#if><#if (largeOffset > 0)> ${styles.grid_large_offset!}${largeOffset}<#elseif (largeOffset != 0) && (offset > 0)> ${styles.grid_large_offset!}${offset}</#if></#local>
    <#local class = addClassArg(class, specOffsetClassesStr)>
    <#local class = addClassArg(class, styles.grid_cell!)>
    <#if last>
      <#local class = addClassArg(class, styles.grid_end!)>
    </#if>
    <#if collapse>
      <#local class = addClassArg(class, styles.collapse!)>
    </#if>
    <#-- save grid sizes -->
    <#local dummy = saveCurrentContainerSizesFromStyleStr(class)>
  </#if>

  <#if open && !close>
    <#local dummy = pushRequestStack("scipioCellMarkupStack", {
      "class":class, "id":id, "style":style, "last":last, "collapse":collapse, 
      "attribs":attribs, "origArgs":origArgs, "passArgs":passArgs
    })>
  <#elseif close && !open>
    <#local stackValues = popRequestStack("scipioCellMarkupStack")!{}>
    <#local dummy = localsPutAll(stackValues)>
  </#if>
  <@cell_markup open=open close=close class=class id=id last=last collapse=collapse attribs=attribs 
    origArgs=origArgs passArgs=passArgs><#nested></@cell_markup>
  <#if close>
    <#-- pop grid sizes -->
    <#local dummy = unsetCurrentContainerSizes()>
  </#if>
</#macro>

<#-- @cell container markup - theme override -->
<#macro cell_markup open=true close=true class="" id="" style="" last=false collapse=false 
    attribs={} excludeAttribs=[] origArgs={} passArgs={} catchArgs...>
  <#if open>
    <div<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if><#rt>
        <#lt><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>><#rt>
  </#if>
      <#nested><#t>
  <#if close>
    </div><#lt>
  </#if>
</#macro>

<#-- 
*************
* parseContainerSizesFromStyleStr
************   
OVERRIDE. Parses container sizes from a class/style string.

* Container size method overrides * 

These provide framework-/theme-specific overriding implementations of the placeholder functions of the
saveCurrentContainerSizes and related utilities lib functions.

NOTES: 
* Implementation of evalAbsContainerSizeFactors assumes all max sizes for large/medium/small are the same.
* catchArgs... is in case of interface changes. Not used.

TODO: 
* These could also parse style for tile classes and calculate approximate corresponding grid sizes from tiles.
* These should be general enough to work for both foundation and bootstrap, but should be confirmed...
    
  * Related *
    utilities#parseContainerSizesFromStyleStr
-->
<#function parseContainerSizesFromStyleStr style catchArgs...>
  <#if !scipioContainerSizesPrefixMap??>
    <#global scipioContainerSizesPrefixMap = {
      "${styles.grid_large!}" : "large", "${styles.grid_medium!}" : "medium", "${styles.grid_small!}" : "small"
    }>
  </#if>
  <#return extractPrefixedStyleNamesWithInt(getPlainClassArgNames(style), scipioContainerSizesPrefixMap)>
</#function>

<#-- 
*************
* evalAbsContainerSizeFactors
************   
OVERRIDE. Calculates nested container size factors. 

TODO: Reimplement in java in com.ilscipio.scipio.ce.webapp.ftl.template.standard.StdTemplateFtlUtil.evalAbsContainerSizeFactors 
        and have this delegate to it
        
  * Related *
    utilities#evalAbsContainerSizeFactors
-->
<#function evalAbsContainerSizeFactors sizesList maxSizes=0 cachedFactorsList=[] catchArgs...>
  <#local maxSize = 12>
  <#if maxSizes?is_number>
    <#if (maxSizes > 0)>
      <#local maxSize = maxSizes>
    </#if>  
  <#else>
    <#-- per-size maxes are not really supported here, this is only to satisfy interface -->
    <#local maxSize = maxSizes.small!maxSizes.medium!maxSizes.large!maxSize>
  </#if>

  <#local large = maxSize>
  <#local medium = maxSize>
  <#local small = maxSize>
  <#-- beginning of sizesList is outermost container.
       to optimize, we will iterate from the INNERMOST container first, which works because the multiplications
       are commutative and nothing carries over.
       this way we can exploit the cached factors better (though might add some other overhead).
  -->
  <#list sizesList?reverse as sizes>
    <#local cachedFactors = cachedFactorsList[(cachedFactorsList?size - 1) - sizes_index]>
    <#if !cachedFactors?is_boolean>
      <#-- if we find a cached factors we already calculated, we're done early; simply multiply and stop  -->
      <#local large = large * (cachedFactors.large / maxSize)>
      <#local medium = medium * (cachedFactors.medium / maxSize)>
      <#local small = small * (cachedFactors.small / maxSize)>
      <#break>
    <#elseif sizes?has_content> <#-- these may be empty; simplifies pushing/popping down the line -->
      <#-- need to use same logic as framework when figuring out defaultSize... e.g., small has priority... 
          assuming all max sizes are same simplifies this -->
      <#local defaultSize = maxSize>
      
      <#if sizes.small??>
        <#local defaultSize = sizes.small>
        <#local small = small * (sizes.small / maxSize)>
      <#-- always 1 
      <#else>
        <#local small = small * (defaultSize / maxSize)>-->
      </#if>
  
      <#if sizes.medium??>
        <#local defaultSize = sizes.medium>
        <#local medium = medium * (sizes.medium / maxSize)>
      <#else>
        <#local medium = medium * (defaultSize / maxSize)>
      </#if>
        
      <#if sizes.large??>
        <#local large = large * (sizes.large / maxSize)>    
      <#else>
        <#local large = large * (defaultSize / maxSize)>
      </#if> 
    </#if> 
  </#list>
  <#return {"large":large, "medium":medium, "small":small}>
</#function>

<#-- 
*************
* Grid List
************
Creates a grid list.

Since this is very foundation specific, this function may be dropped in future installations

  * Usage Examples *  
    <@grid>
        <li>Text or <a href="">Anchor</a></li>
    </@grid>            
                    
  * Parameters *
    class                   = ((css-class)) CSS classes
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    columns                 = ((int), default: 5) Number of columns
    type                    = (tiles|list|, default: list)
    tilesType               = Default tiles type for tiles inside this element
                              This same value is used as default for the @tile macro "type" attrib.
                              However, in addition, type when specified here may also influence tile arrangement.
                              See @tile macro "type" attrib for possible values.
-->
<#assign grid_defaultArgs = {
  "type":"", "tilesType":"", "class":"", "columns":4, "id":"", "passArgs":{}
}>
<#macro grid args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.grid_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>

  <#if !type?has_content>
    <#local type = "list">
  </#if>
  <#if !tilesType?has_content>
    <#local tilesType = "default">
  </#if>

  <#local gridInfo = {"type":type, "tilesType":tilesType, "columns":columns}>
  <#local dummy = pushRequestStack("scipioGridInfoStack", gridInfo)>
  <#-- here, use the number of greater ("page") columns to estimate corresponding grid sizes for heuristics -->
  <#local dummy = saveCurrentContainerSizes({"large":12/columns, "medium":12/columns, "small":12/columns})>
  <#if type == "tiles" || type == "freetiles">
    <#local freewallNum = getRequestVar("scipioFreewallIdNum")!0>
    <#local freewallNum = freewallNum + 1 />
    <#local dummy = setRequestVar("scipioFreewallIdNum", freewallNum)>
    <#if !id?has_content>
      <#local id = "freewall_id_${freewallNum!0}">
    </#if>

    <#local tileStyleName = tilesType>
    <#local tileStylePrefix = "tile_" + tileStyleName>
    <#local defaultTileStylePrefix = "tile_default">

    <#local class = addClassArg(class, styles.tile_container!)>
    <#local class = addClassArgDefault(class, styles[tileStylePrefix + "_containerclass"]!styles[defaultTileStylePrefix + "_containerclass"]!"")>
 
    <@grid_tiles_markup_container class=class id=id columns=columns tylesType=tylesType origArgs=origArgs passArgs=passArgs><#nested></@grid_tiles_markup_container>
  <#elseif type=="list">
    <@grid_list_markup_container class=class id=id columns=columns origArgs=origArgs passArgs=passArgs><#nested></@grid_list_markup_container>
  </#if>
  <#local dummy = unsetCurrentContainerSizes()>
  <#local dummy = popRequestStack("scipioGridInfoStack")>
</#macro>

<#-- @grid tiles container markup - theme override -->
<#macro grid_tiles_markup_container class="" id="" tylesType="" columns=1 origArgs={} passArgs={} catchArgs...>
  <@container class=class id=id>
    <#nested>
  </@container>
  <@script>
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
  </@script>
</#macro>

<#-- @grid list container markup - theme override -->
<#macro grid_list_markup_container class="" id="" columns=1 origArgs={} passArgs={} catchArgs...>
  <#-- this never takes effect
  <#local defaultClass="${styles.grid_block_prefix!}${styles.grid_small!}${styles.grid_block_postfix!}2 ${styles.grid_block_prefix!}${styles.grid_medium!}${styles.grid_block_postfix!}4 ${styles.grid_block_prefix!}${styles.grid_large!}${styles.grid_block_postfix!}5">-->
  <#if ((columns-2) > 0)>
    <#local class = addClassArgDefault(class, "${styles.grid_block_prefix!}${styles.grid_small!}${styles.grid_block_postfix!}${columns-2} ${styles.grid_block_prefix!}${styles.grid_medium!}${styles.grid_block_postfix!}${columns-1} ${styles.grid_block_prefix!}${styles.grid_large!}${styles.grid_block_postfix!}${columns}")/>
  <#else>
    <#local class = addClassArgDefault(class, "${styles.grid_block_prefix!}${styles.grid_large!}${styles.grid_block_postfix!}${columns}")/>
  </#if>
  <#local dummy = saveCurrentContainerSizesFromStyleStr(class)>
  <ul<@compiledClassAttribStr class=class />>
    <#nested>
  </ul>
  <#local dummy = unsetCurrentContainerSizes()>
</#macro>

<#-- 
*************
* Tile
************
Creates a very basic wrapper for tiles (can be used in metro designs).

Please be aware that this is neither based on standard bootstrap, nor foundation. 
It is loosely based on http://metroui.org.ua/tiles.html 

  * Usage Examples *  
    <@tile size="small">
       // content
    </@tile>
                    
  * Parameters *
    type                    = (default|generic|(theme-specific), default: -inherited from tilesType on @grid element-)
                              Provides a set of defaults for the other parameters (size, color, etc.) via global styles hash (as "tile_xxx" where xxx is type).
                              Scipio standard adds the following types by default:
                              * {{{gallery1}}}: tile with settings tailored to image gallery of normal-sized thumb images.
    size                    = (small|normal|wide|large|big|super, default: normal) Tile size specifier
    title                   = Title
    titleType               = (|default|..., default: default) Title type. Used to determine style
                              Currently only "default" supported.
                              Type style is looked up as: styles["type_overlay_" + titleType?replace("-","_")].
    titleBgColor            = (none|0|1|2|3|4|5|6|7|..., default: -from global styles-, fallback default: 0)
                              * {{{none}}}: Prevents color class.
    class                   = ((css-class)) CSS classes 
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    link                    = Link URL around nested content
                              WARN: can only use if no other links inside nested content
    linkTarget              = (|_blank|(boolean)|..., default: -from global styles-, fallback default: -empty-) Target for link element
                              If boolean, false prevents any; true will allow global styles hash lookup.
    id                      = Field ID
    color                   = (none|0|1|2|3|4|5|6|7|..., default: -from global styles-, fallback default: 0)
                              * {{{none}}}: Prevents color class.
    icon                    = Icon code (http://zurb.com/playground/foundation-icon-fonts-3)
    image                   = Background image URL (icon won't be shown if not empty)
    imageType               = (|default|..., default: default) Image type for styling
                              Scipio standard supported types (extensible by theme):
                              * {{{cover}}}: This is currently the default. fills tile.
                              * {{{contain}}}: Show whole image in tile.
                              Type style is looked up as: styles["type_image_" + imageType?replace("-","_")].
    imageBgColor            = (none|0|1|2|3|4|5|6|7|..., default: -from global styles-, fallback default: none)
                              * {{{none}}}: prevents color class.
    overlayType             = (|default|..., default: default) Overlay type
                              Scipio standard supported types (extensible by theme):
                              * {{{slide-up}}}: This is currently the default.
                              Type style is looked up as: styles["type_overlay_" + overlayType?replace("-","_")].
    overlayBgColor          = (none|0|1|2|3|4|5|6|7|..., default: -from global styles-, fallback default: 0)
                              * {{{none}}}: prevents color class.
-->
<#assign tile_defaultArgs = {
  "type":"", "size":"", "title":"", "titleType":"", "titleBgColor":"", "class":"", "id":"", "link":"", 
  "linkTarget":true, "color":"", "icon":"", "image":"", "imageType":"", "imageBgColor":"", "overlayType":"", 
  "overlayBgColor":"", "passArgs":{}
}>
<#macro tile args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.tile_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>

  <#local gridInfo = readRequestStack("scipioGridInfoStack")!{}>
  <#if !type?has_content>
    <#local type = (gridInfo.tilesType)!"">
  </#if>
  <#if !type?has_content>
    <#local type = "default">
  </#if>
  
  <#-- find tile-type-based arg defaults -->
  <#local styleName = type?replace("-","_")>
  <#local stylePrefix = "tile_" + styleName>
  <#local defaultStylePrefix = "tile_default">
  <#-- NOTE: _class is handled further below -->
  <#if !size?has_content>
    <#local size = styles[stylePrefix + "_size"]!styles[defaultStylePrefix + "_size"]!"normal">
  </#if>
  <#if !color?has_content>
    <#local color = styles[stylePrefix + "_color"]!styles[defaultStylePrefix + "_color"]!"0">
  </#if>
  <#if !icon?has_content>
    <#local icon = styles[stylePrefix + "_icon"]!styles[defaultStylePrefix + "_icon"]!"0">
  </#if>
  <#if !imageType?has_content>
    <#local imageType = styles[stylePrefix + "_imagetype"]!styles[defaultStylePrefix + "_imagetype"]!"">
  </#if>
  <#if !imageBgColor?has_content>
    <#local imageBgColor = styles[stylePrefix + "_imagebgcolor"]!styles[defaultStylePrefix + "_imagebgcolor"]!"">
  </#if>
  <#if !overlayType?has_content>
    <#local overlayType = styles[stylePrefix + "_overlaytype"]!styles[defaultStylePrefix + "_overlaytype"]!"">
  </#if>
  <#if !overlayBgColor?has_content>
    <#local overlayBgColor = styles[stylePrefix + "_overlaybgcolor"]!styles[defaultStylePrefix + "_overlaybgcolor"]!"">
  </#if>
  <#if !titleType?has_content>
    <#local titleType = styles[stylePrefix + "_titletype"]!styles[defaultStylePrefix + "_titletype"]!"">
  </#if>
  <#if !titleBgColor?has_content>
    <#local titleBgColor = styles[stylePrefix + "_titlebgcolor"]!styles[defaultStylePrefix + "_titlebgcolor"]!"">
  </#if>
  <#if linkTarget?is_boolean && linkTarget == false>
    <#local linkTarget = "">
  <#elseif (linkTarget?is_boolean && linkTarget == true) || !linkTarget?has_content>
    <#local linkTarget = styles[stylePrefix + "_linktarget"]!styles[defaultStylePrefix + "_linktarget"]!"">
  </#if>

  <#-- lookup styles -->
  <#local class = addClassArg(class, styles.tile_wrap!)>
  <#local class = addClassArg(class, "${styles.tile_wrap!}-${size!}")>
  <#local color = color?string>
  <#if color?has_content && color != "none">
    <#local colorClass = "${styles.tile_color_prefix!}${color!}">
  <#else>
    <#local colorClass = "">
  </#if>
  <#local class = addClassArg(class, colorClass)>
  <#local dataSizex = calcTileSize("x",size)>
  <#local dataSizey = calcTileSize("y",size)>
  <#if !overlayType?has_content || overlayType == "default">
    <#local overlayClass = styles["tile_overlay_default"]!"">
  <#else>
    <#local overlayClass = styles["tile_overlay_" + overlayType?replace("-","_")]!styles["tile_overlay_default"]!"">
  </#if>
  <#local overlayClass = addClassArg(overlayClass, styles.tile_overlay!)>
  <#local overlayBgColor = overlayBgColor?string>
  <#if overlayBgColor?has_content && overlayBgColor != "none">
    <#local overlayBgColorClass = "${styles.tile_color_prefix!}${overlayBgColor!}">
  <#else>
    <#local overlayBgColorClass = "">
  </#if>
  <#if !imageType?has_content || imageType == "default">
    <#local imageClass = styles["tile_image_default"]!"">
  <#else>
    <#local imageClass = styles["tile_image_" + imageType?replace("-","_")]!styles["tile_image_default"]!"">
  </#if>
  <#local imageClass = addClassArg(imageClass, styles.tile_image!)>
  <#local imageBgColor = imageBgColor?string>
  <#if imageBgColor?has_content && imageBgColor != "none">
    <#local imageBgColorClass = "${styles.tile_color_prefix!}${imageBgColor!}">
  <#else>
    <#local imageBgColorClass = "">
  </#if>
  <#if !titleType?has_content || titleType == "default">
    <#local titleClass = styles["tile_title_default"]!"">
  <#else>
    <#local titleClass = styles["tile_title_" + titleType?replace("-","_")]!styles["tile_title_default"]!"">
  </#if>
  <#local titleClass = addClassArg(titleClass, styles.tile_title!)>
  <#local titleBgColor = titleBgColor?string>
  <#if titleBgColor?has_content && titleBgColor != "none">
    <#local titleBgColorClass = "${styles.tile_color_prefix!}${titleBgColor!}">
  <#else>
    <#local titleBgColorClass = "">
  </#if>

  <#local class = addClassArgDefault(class, styles[stylePrefix + "_class"]!styles[defaultStylePrefix + "_class"]!"")>
  
  <@tile_markup class=class id=id dataSizex=dataSizex dataSizey=dataSizey image=image imageClass=imageClass imageBgColorClass=imageBgColorClass 
    link=link linkTarget=linkTarget icon=icon 
    overlayClass=overlayClass overlayBgColorClass=overlayBgColorClass title=title titleClass=titleClass titleBgColorClass=titleBgColorClass origArgs=origArgs passArgs=passArgs><#nested></@tile_markup>
</#macro>

<#function calcTileSize orientation="x" value="normal">
  <#if orientation="x">
    <#if !scipioTileSizeMapX??>
      <#-- global: optimization only (doesn't have to be setRequestVar) -->
      <#global scipioTileSizeMapX = {"small":0,"normal":1,"wide":2,"large":2,"big":3,"super":4}/>
    </#if>
    <#return scipioTileSizeMapX[value]/>
  <#else>
    <#if !scipioTileSizeMapY??>
      <#global scipioTileSizeMapY={"small":0,"normal":1,"wide":1,"large":2,"big":3,"super":4}/>
    </#if>
    <#return scipioTileSizeMapY[value]/>
  </#if>
</#function>

<#-- @tile main markup - theme override -->
<#macro tile_markup class="" id="" dataSizex="" dataSizey="" image="" imageClass="" imageBgColorClass="" link="" linkTarget="" icon="" 
  overlayClass="" overlayBgColorClass="" title="" titleClass="" titleBgColorClass="" origArgs={} passArgs={} catchArgs...>
  <#-- main markup (TODO: factor out into @tile_markup) -->
  <#-- TODO: need to calc-convert tile x-size to approximate grid sizes and pass in large-medium-small below,
      OR modify parseContainerSizesFromStyleStr to do it automatically from class string (HOWEVER
      note that parseContainerSizesFromStyleStr would have to call calcTileSize type="x" again, and it's
      not clear how precise this will be)
  <#local dummy = saveCurrentContainerSizes({"large":12, "medium":12, "small":12})> -->
  <#-- NOTE: dataSizex gets automatically translated to data-sizex (FTL: no dashes allowed in arg names) -->
  <@container class=class id=id dataSizex=dataSizex dataSizey=dataSizey>
    <div class="${styles.tile_content!}">
      <#-- DEV NOTE: I think the image div belongs INSIDE the tile_content container? -->
      <#if image?has_content>
        <div class="${imageClass}<#if imageBgColorClass?has_content> ${imageBgColorClass}</#if>" style="background-image: url(${escapeFullUrl(image, 'style')});"></div>
      </#if>
      <#if link?has_content><a href="${escapeFullUrl(link, 'html')}"<#if linkTarget?has_content> target="${escapePart(linkTarget, 'html')}"</#if>></#if>
      <#if icon?has_content && !icon?starts_with("AdminTileIcon") && !image?has_content><span class="${styles.tile_icon!}"><i class="${icon!}"></i></span></#if>
      <#local nestedContent><#nested></#local>
      <#if nestedContent?has_content><span class="${overlayClass}<#if overlayBgColorClass?has_content> ${overlayBgColorClass}</#if>">${nestedContent}</span></#if>
      <#if title?has_content><span class="${titleClass}<#if titleBgColorClass?has_content> ${titleBgColorClass}</#if>">${escapePart(title, 'html')}</span></#if>
      <#if link?has_content></a></#if>
    </div>
  </@container>
  <#--<#local dummy = unsetCurrentContainerSizes()>-->
</#macro>

<#-- 
*************
* Section
************
Creates a logical section with optional title and menu.

Automatically handles heading sizes and keeps track of section nesting for the whole rendering request, even across screens.render calls.

NOTE: Use getCurrentHeadingLevel and getCurrentSectionLevel functions if need to get current
levels manually, but most often should let @section menu handle the level calculations.

IMPL NOTE: This has dependencies on some non-structural macros.

FIXME: The title and menu rendering are captured, should not be capturing like this anymore...

  * Usage Examples *  
    <@section attr="">
        Inner Content
    </@section>            

  * Parameters *
    type                    = (default|generic|..., default: default)
                              Scipio standard currently only defines {{{default}}} and {{{generic}}} - almost the same except
                              {{{generic}}} explicitly expects themes not to change defaults (unless absolutely necessary) whereas it's more acceptable
                              to change {{{default}}} defaults. Custom types may be defined.
                              TODO: Many of the parameters currently not configurable in themes.
    id                      = Section ID, on outermost container and used as prefix for other element IDs
                              NOTE: By convention (and for compability) this will always be on the outermost container.
    containerId             = Section ID, on outermost container only (special case of {{{id}}})    
                              Unlike {{{id}}}, {{{containerId}}} is not used to generate any other IDs, although
                              it may still be used by the macro where an ID is needed to reference the section (such as any javascript).
                              NOTE (2016-09-09): If both present, {{{containerId}}} now overrides {{{id}}} on the outer container (previously was the opposite).
    contentId               = Explicit ID for inner content container
    containerClass          = ((css-class)) CSS classes, on outermost container
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    cellClass               = ((css-class)) CSS classes, on outer cell element (affects title) (NOT on outermost container)
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
                              NOTE: boolean false has no effect here
                              NOTE: Grid size classes may be specified here to limit dimensions. 
    class                   = ((css-class)) CSS classes, on outer cell element (alias for {{{cellClass}}})
    contentClass            = ((css-class)) CSS classes, on inner content container
    containerStyle          = Legacy HTML style attribute, on outermost container
    style                   = Legacy HTML style attribute, on outermost container (alias for {{{containerStyle}}})
    contentStyle            = Legacy HTML style attribute, on inner content container
    containerAttribs        = ((map)) Extra attributes, on outer container
    attribs                 = ((map)) Extra attributes, on outer container (alias for {{{containerAttribs}}})
    contentAttribs          = ((map)) Extra attributes, on inner content container
    title                   = Section title
    titleClass              = ((css-class)) Section title class 
                              Supports complex expressions (rarely needed; usually headingLevel enough).
    padded                  = ((boolean), default: false)
    autoHeadingLevel        = ((boolean), default: true) Auto increase heading level when title present
    headingLevel            = ((int)) Force this heading level for title. if autoHeadingLevel true, also influences nested elems (even if no title here, but if no title won't consume a size)
    relHeadingLevel         = ((int)) Increase heading level by this number
    defaultHeadingLevel     = ((int)) Default heading level (same as headingLevel if autoHeadingLevel false)
                              If empty string, use getDefaultHeadingLevel()
    menuContent             = ((macro)|(map)|(string)) (optional) A macro, map of @menu args, or string html markup of li elements only defining a menu
                              * {{{macro}}}: Should accept one argument named "menuArgs" which is default @section menu args that can be passed
                                directly to @menu by caller, e.g.:
                                  <#macro myMenuContent menuArgs>
                                    <@menu args=menuArgs>
                                      <@menuitem ... />
                                      <@menuitem ... />
                                    </@menu>
                                  </#macro>
                                The caller can override the section menu args as needed.
                              * {{{map}}}: These will simply be passed as <@menu args=menuContent /> by @section, 
                                with defaults added underneath (but user-passed args override).
                              * {{{string}}} (HTML markup): Should be <li> elements only, generated manually or using <@menu type="section" ... inlineItems=true>.
                                WARN: if using @menu to pre-generate the menu as string/html, the menu arguments such as "type" are lost and 
                                    assumed to be "section" or "section-inline".
    menuLayoutTitle         = (post-title|pre-title|inline-title, default: -from global styles-, fallback default: post-title) 
                              This is a low-level control; avoid where possible.   
    menuLayoutGeneral       = (top|bottom|top-bottom, default: -from global styles-, fallback default: top)  
    titleContainerClass     = ((css-class), default: -from global styles-) (optional) CSS classes, for title container
                              NOTE: if the argument results in no class specified, the container is omitted.
    menuContainerClass      = ((css-class), default: -from global styles-) (optional) CSS classes, for menu container
                              NOTE: if the argument results in no class specified, the container is omitted.
    menuTitleContainerClass = ((css-class), default: -from global styles-) (optional) CSS classes, for menu and title (combined) container
                              NOTE: if the argument results in no class specified, the container is omitted.                        
    menuRole                = (nav-menu|paginate-menu, default: nav-menu)
    menuClass               = ((css-class)) (optional) CSS classes, extra menu classes
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
                              This is a low-level control; avoid where possible.
    menuId                  = Explicit ID for menu
    requireMenu             = ((boolean), default: false) If true, add menu elem even if empty
    forceEmptyMenu          = ((boolean), default: false) If true, always add menu and must be empty
    menuItemsInlined        = ((boolean), default: -auto-) Override flag to say if menuContent string contains menu items only or also the menu wrapper
                              If this is omitted (or empty string), it tries to determine automatically from the string.
                              NOTE: This only applies if menuContent is a string.
    hasContent              = ((boolean), default: true) Optional content hint
                              When false, will add classes to indicate content is empty or treat as logically empty 
                              (workaround for lack of CSS :blank and possibly other browser limitations)
-->
<#assign section_defaultArgs = {
  "type":"", "id":"", "title":"", "style":"", "cellClass":-1, "class":-1, "padded":false, "autoHeadingLevel":true, "headingLevel":"", 
  "relHeadingLevel":"", "defaultHeadingLevel":"", "menuContent":"", "menuClass":"", "menuLayoutTitle":"", "menuLayoutGeneral":"", 
  "titleContainerClass":"", "menuContainerClass":"", "menuTitleContainerClass":"", "menuRole":"", 
  "requireMenu":false, "forceEmptyMenu":false, "menuItemsInlined":"", "hasContent":true, "titleClass":"", 
  "containerClass":"", "containerId":"", "containerStyle":"", 
  "menuId":"", "contentId":"", "contentClass":"", "contentStyle":"",
  "attribs":{}, "containerAttribs":{}, "contentAttribs":{},
  "open":true, "close":true, "passArgs":{}
}>
<#macro section args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.section_defaultArgs)>
  <#local dummy = localsPutAll(args)>

  <#if open>
    <#if !type?has_content>
      <#local type = "default">
    </#if>
  <#else>
    <#-- section_core has its own stack; don't need to preserve these -->
    <#local class = "">  
  </#if>
  <@section_core id=id contentId=contentId title=title cellClass=cellClass class=class style=style padded=padded menuContent=menuContent 
    fromScreenDef=false menuClass=menuClass menuId=menuId menuLayoutTitle=menuLayoutTitle menuLayoutGeneral=menuLayoutGeneral 
    titleContainerClass=titleContainerClass menuContainerClass=menuContainerClass menuTitleContainerClass=menuTitleContainerClass menuRole=menuRole requireMenu=requireMenu 
    forceEmptyMenu=forceEmptyMenu menuItemsInlined=menuItemsInlined hasContent=hasContent autoHeadingLevel=autoHeadingLevel headingLevel=headingLevel 
    relHeadingLevel=relHeadingLevel defaultHeadingLevel=defaultHeadingLevel titleStyle=titleClass 
    containerClass=containerClass containerId=containerId containerStyle=containerStyle contentClass=contentClass contentStyle=contentStyle
    attribs=attribs containerAttribs=containerAttribs contentAttribs=contentAttribs
    open=open close=close passArgs=passArgs><#nested /></@section_core>
</#macro>

<#-- Core implementation of @section. 
    More options than @section, but raw and less friendly interface; not meant for template use, but can be called from other macro implementations.
     
    Migrated from @renderScreenletBegin/End screen widget macro.

  * Parameters *     
    fromScreenDef     = hint of whether called from Ofbiz screen renderer/xml (true) or FTL macros (false)
    hasContent        = hint to say there will be content; workaround for not being able to assume that all browsers have the CSS support to check if content present -->
<#assign section_core_defaultArgs = {
  "type":"", "id":"", "title":"", "cellClass":-1, "class":-1, "style":"", "collapsible":false, "saveCollapsed":true, "contentId":"", 
  "expandToolTip":true, "collapseToolTip":true, "fullUrlString":"", "padded":false, "menuContent":"", 
  "showMore":true, "collapsed":false, "javaScriptEnabled":true, "fromScreenDef":false, "menuClass":"", "menuId":"", 
  "menuLayoutTitle":"", "menuLayoutGeneral":"", "titleContainerClass":"", "menuContainerClass":"", "menuTitleContainerClass":"",
  "menuRole":"", "requireMenu":false, "forceEmptyMenu":false, "menuItemsInlined":"", "hasContent":true, "titleStyle":"", 
  "titleContainerStyle":"", "titleConsumeLevel":true, "autoHeadingLevel":true, "headingLevel":"", "relHeadingLevel":"", 
  "containerClass":"", "containerId":"", "containerStyle":"", "contentClass":"", "contentStyle":"",
  "attribs":{}, "containerAttribs":{}, "contentAttribs":{},
  "defaultHeadingLevel":"", "open":true, "close":true, "passArgs":{}
}>
<#macro section_core args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.section_core_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>

  <#if open>
    <#if !type?has_content>
      <#local type = "default">
    </#if>
    <#local styleName = type?replace("-","_")>

    <#local idPrefix = id><#-- NOTE: must be before containerId = id assign -->
    <#if containerId?has_content>
      <#-- NOTE: we can use containerId as the basic ID, but we don't set idPrefix to it because don't want
          to generate other IDs from it -->
      <#local id = containerId>
    <#elseif id?has_content>
      <#local containerId = id><#-- this was an error: + "_container" -->
    </#if>

    <#if idPrefix?has_content>
      <#if !contentId?has_content>
        <#local contentId = idPrefix + "_content">
      </#if>
      <#if !menuId?has_content>
        <#local menuId = idPrefix + "_menu">
      </#if>
    </#if>

    <#-- uses special -1 value for detecting presence. because explicitly specified empty value "" on class=""
        must be recognized as well as booleans -->
    <#if cellClass?is_number>
      <#if !class?is_number>
        <#local cellClass = class>
      <#else>
        <#local cellClass = "">
      </#if>
    </#if>
  
  <#-- level logic begin -->
    <#-- NOTE: request obj only available because of macro renderer initial context mod -->
    <#local sLevel = getCurrentSectionLevel()>
    <#local prevSectionLevel = sLevel>
    <#local dummy = setCurrentSectionLevel(sLevel+1)>
  <#-- level logic end -->
  
  <#-- title-style parsing begin -->
    <#-- titleContainerStyle can be inlined as prefix in titleStyle, separated by ;, as workaround 
        full string can look like:
          titleStyle="div:+containerClass;h4:+titleClass", titleStyle="div;h5"
          titleStyle="div;h+1;consumeLevel=true" -->
    <#if titleStyle?has_content>
      <#local titleStyleArgs = getHeadingElemSpecFromStyleStr(titleStyle, titleContainerStyle,
        "h|heading","container|div|span|p|raw", "container|div", "widget-screenlet")>
  
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
    <#local titleClass = translateStyleStrClassesArg(titleStyleArgs.elemClass!"")!"">
    <#local titleContainerElemType = translateStyleStrClassesArg(titleStyleArgs.containerElemType!"")!false>
    <#local titleHeadingContainerClass = translateStyleStrClassesArg(titleStyleArgs.containerElemClass!"")!"">
  
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
    <#local dummy = pushRequestStack("scipioSectionStack", 
        {"autoHeadingLevel":autoHeadingLevel, "updatedHeadingLevel":updatedHeadingLevel, 
         "prevHeadingLevel":prevHeadingLevel, "prevSectionLevel":prevSectionLevel})>
  <#-- auto-heading-level logic end -->
  
    <#if !menuLayoutTitle?has_content>
      <#local menuLayoutTitle = styles["section_" + type + "_menulayouttitle"]!styles["section_default_menulayouttitle"]!"post-title">
      <#if !menuLayoutTitle?has_content>
        <#local menuLayoutTitle = "post-title">
      </#if>
    </#if>
    <#if !menuLayoutGeneral?has_content>
      <#local menuLayoutGeneral = styles["section_" + type + "_menulayoutgeneral"]!styles["section_default_menulayoutgeneral"]!"top">
    </#if>

    <#local menuLayoutTitleStyleName = menuLayoutTitle?replace("-","_")>
    <#local menuTitleContainerClass = addClassArgDefault(menuTitleContainerClass, styles["section_" + styleName + "_" + menuLayoutTitleStyleName + "_menutitlecontainerclass"]!styles["section_default_" + menuLayoutTitleStyleName + "_menutitlecontainerclass"]!)>
    <#local titleContainerClass = addClassArgDefault(titleContainerClass, styles["section_" + styleName + "_" + menuLayoutTitleStyleName + "_titlecontainerclass"]!styles["section_default_" + menuLayoutTitleStyleName + "_titlecontainerclass"]!)>
    <#local menuContainerClass = addClassArgDefault(menuContainerClass, styles["section_" + styleName + "_" + menuLayoutTitleStyleName + "_menucontainerclass"]!styles["section_default_" + menuLayoutTitleStyleName + "_menucontainerclass"]!)>
  
    <#-- Scipio: we support menuContent as string (html), macro or hash definitions.
        When string, menuContent is not wrapped in UL when it's received here from macro renderer... 
        NOTE: with recent patch, menuContent passed by renderer is rendered by macro renderer (was not the case before - used old html renderer). -->
    <#if isObjectType("string", menuContent)> <#-- DEV NOTE: WARN: ?is_string would not always work here -->
      <#local menuContent = menuContent?trim>
    </#if>
    <#if !menuRole?has_content>
      <#local menuRole = "nav-menu">
    </#if>
    <#local hasMenu = (menuContent?is_directive || menuContent?has_content || requireMenu || forceEmptyMenu)>
    <#local hasTitle = title?has_content>
    <#local contentFlagClasses = makeSectionContentFlagClasses(type, sLevel, hLevel, hasMenu, hasTitle, hasContent, 
      menuLayoutTitle, menuLayoutGeneral, menuRole, collapsible, collapsed, javaScriptEnabled, fromScreenDef)>
  
    <#local menuMarkup = "">
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
    
          <#if menuLayoutTitle == "inline-title">
            <#local defaultMenuType = "section-inline">
          <#else>
            <#local defaultMenuType = "section">
          </#if>
    
          <#if forceEmptyMenu>
            <#local preMenuItems = []>
            <#local postMenuItems = []>
          <#else>
            <#local extraMenuItemsMap = makeSectionExtraMainMenuItems(type, sectionLevel, headingLevel, menuLayoutTitle, menuLayoutGeneral, menuRole, hasMenu, 
              contentFlagClasses, collapsible, collapsed, javaScriptEnabled, contentId, saveCollapsed, expandToolTip, 
              collapseToolTip, fullUrlString, fromScreenDef)!{}>
            <#local preMenuItems = extraMenuItemsMap.preMenuItems![]>
            <#local postMenuItems = extraMenuItemsMap.postMenuItems![]>        
          </#if>
    
          <#if !menuId?has_content>
            <#if idPrefix?has_content>
              <#local menuId = idPrefix + "_menu">
            </#if>
          </#if>
        
          <#if menuId?has_content>
            <#local menuIdArg = menuId>
          <#else>
            <#local menuIdArg = "">
          </#if>
          <#if menuClass?has_content> <#-- NOTE: don't use defaultMenuClass here; @menu will figure it out instead -->
            <#if menuClass == "none">
              <#local menuClassArg = "=">
            <#else>
              <#local menuClassArg = menuClass>
            </#if>
          <#else>
            <#local menuClassArg = "">
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
            <#if !menuItemsInlined?is_boolean>
              <#local menuItemsInlined = isMenuMarkupItemsInline(menuContent)>
            </#if>

            <#-- FIXME: These calls should not be captured, but run at the correct time... -->

            <#local menuItemsMarkup>
              <#if !forceEmptyMenu>
                ${menuContent}
              </#if>
            </#local>
        
            <#if !menuContent?has_content || menuItemsInlined>
              <#-- WARN: we have to assume the menu type here (especially for pre/postMenuItems); inherently limited -->
              <@menu type=defaultMenuType inlineItems=false id=menuIdArg class=menuClassArg preItems=preMenuItems postItems=postMenuItems>
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
          <@heading level=hLevel elemType=titleElemType class=titleClass containerElemType=titleContainerElemType 
            containerClass=titleHeadingContainerClass passArgs=passArgs>${escapePart(title, 'html')}</@heading>
        </#local>
      </#if> 
    </#if>
  
  </#if> <#-- /#(if open) -->

  <#local menuTitleMarkup = "">
  <#-- render menu + title combo; for now, only need to do at open and then save the markup -->
  <#if open>
    <#if showMore>
      <#-- FIXME: This call should not be captured, but run at the correct time... -->
      <#local menuTitleMarkup><@section_markup_menutitle sectionType=type sectionStyleName=styleName sectionLevel=sLevel headingLevel=hLevel menuLayoutTitle=menuLayoutTitle menuLayoutGeneral=menuLayoutGeneral
        titleContainerClass=titleContainerClass menuContainerClass=menuContainerClass menuTitleContainerClass=menuTitleContainerClass
        menuRole=menuRole hasMenu=hasMenu menuContent=menuMarkup menuContentArgs={} hasTitle=hasTitle titleContent=titleMarkup titleContentArgs={}
        contentFlagClasses=contentFlagClasses fromScreenDef=fromScreenDef position="top" origArgs=origArgs passArgs=passArgs/></#local>
    </#if>
  </#if> 

  <#if open && !close>
    <#-- save stack of all the args passed to markup macros that have open/close 
        so they don't have to remember a stack themselves -->
    <#local dummy = pushRequestStack("scipioSectionMarkupStack", {
      "type":type, "styleName":styleName, "cellClass":cellClass, "contentFlagClasses":contentFlagClasses, 
      "id":id, "idPrefix":idPrefix, "title":title, "style":style, "sLevel":sLevel, "hLevel":hLevel, "menuTitleMarkup":menuTitleMarkup, "menuMarkup":menuMarkup,
      "containerClass":containerClass, "containerId":containerId, "containerStyle":containerStyle, 
      
      "collapsed":collapsed, "contentId":contentId, "collapsible":collapsible, "saveCollapsed":saveCollapsed, 
      "expandToolTip":expandToolTip, "collapseToolTip":collapseToolTip, "padded":padded, "showMore":showMore, "fullUrlString":fullUrlString,
      "javaScriptEnabled":javaScriptEnabled, "fromScreenDef":fromScreenDef, "hasContent":hasContent, 
      "menuLayoutTitle":menuLayoutTitle, "menuLayoutGeneral":menuLayoutGeneral, "menuRole":menuRole, "requireMenu":requireMenu, "forceEmptyMenu":forceEmptyMenu,
      "titleContainerClass":titleContainerClass, "menuContainerClass":menuContainerClass, "menuTitleContainerClass":menuTitleContainerClass,
      "contentClass":contentClass, "contentStyle":contentStyle,
      
      "attribs":attribs, "containerAttribs":containerAttribs, "contentAttribs":contentAttribs,
      
      "origArgs":origArgs, "passArgs":passArgs
    })>
  <#elseif close && !open>
    <#-- these _must_ override anything passed to this macro call (shouldn't be any) -->
    <#local stackValues = popRequestStack("scipioSectionMarkupStack")!{}>
    <#local dummy = localsPutAll(stackValues)>
  </#if>

  <#if !containerAttribs?has_content && attribs?has_content>
    <#local containerAttribs = attribs>
  </#if>

  <#-- DEV NOTE: when adding params to this call, remember to update the stack above as well! -->
  <#-- TODO: collapsibleAreaId was renamed to contentId, so should remove; still passing it for _markup macro compatibility for now -->
  <@section_markup_container type=type styleName=styleName open=open close=close
    sectionLevel=sLevel headingLevel=hLevel menuTitleContent=menuTitleMarkup menuTitleContentArgs={} menuContent=menuMarkup menuContentArgs={} class=cellClass contentClass=contentClass contentStyle=contentStyle
    contentFlagClasses=contentFlagClasses id=id title=title style=style collapsed=collapsed contentId=contentId collapsibleAreaId=contentId
    collapsible=collapsible saveCollapsed=saveCollapsed expandToolTip=expandToolTip collapseToolTip=collapseToolTip 
    padded=padded showMore=showMore fullUrlString=fullUrlString javaScriptEnabled=javaScriptEnabled 
    fromScreenDef=fromScreenDef hasContent=hasContent menuLayoutTitle=menuLayoutTitle menuLayoutGeneral=menuLayoutGeneral menuRole=menuRole requireMenu=requireMenu 
    containerClass=containerClass containerId=containerId containerStyle=containerStyle
    containerAttribs=containerAttribs containerExcludeAttribs=[] contentAttribs=contentAttribs contentExcludeAttribs=[]
    forceEmptyMenu=forceEmptyMenu origArgs=origArgs passArgs=passArgs><#nested></@section_markup_container>
  
  <#if close>
  <#-- auto-heading-level logic begin -->
    <#local stackValues = popRequestStack("scipioSectionStack")!{}>
    
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

<#-- @section content classes markup - theme override
  * Return Value *
    string of classes (plain; no starting/trailing spaces or special class arg syntax) -->
<#function makeSectionContentFlagClasses type sectionLevel=1 headingLevel=1hasMenu=false hasTitle=false hasContent=true
     menuLayoutTitle="" menuLayoutGeneral="" menuRole="" collapsible=false collapsed=false javaScriptEnabled=false fromScreenDef=false catchArgs...>
  <#local contentFlagClasses>section-level-${sectionLevel} heading-level-${headingLevel}<#if hasTitle> has-title<#else> no-title</#if><#if hasMenu> has-menu<#else> no-menu</#if><#if hasContent> has-content<#else> no-content</#if></#local>
  <#return contentFlagClasses>
</#function>

<#-- @section extra main menu items markup - theme override
  * Return Value *
    map of lists, in the format {"preMenuItems":[...], "postMenuItems":[...]} -->
<#function makeSectionExtraMainMenuItems type sectionLevel=1 headingLevel=1 menuLayoutTitle="" menuLayoutGeneral="" menuRole="" hasMenu=false contentFlagClasses="" 
    collapsible=false collapsed=false javaScriptEnabled=false contentId="" saveCollapsed=false expandToolTip="" 
    collapseToolTip="" fullUrlString="" fromScreenDef=false catchArgs...>
  <#return {"preMenuItems":[], "postMenuItems":[]}>
  <#-- Scipio: TODO: translate this into vars above if/once needed again (as @menuitem args maps within lists)
  <#local preMenuItems></#local>
  <#local postMenuItems>
    <#if menuLayoutTitle != "pre-title" && menuLayoutTitle != "inline-title">
    <#if collapsible>
    <li class="<#rt/>
    <#if collapsed>
    collapsed"><a <#if javaScriptEnabled>onclick="javascript:toggleScreenlet(this, '${contentId}', '${saveCollapsed?string}', '${expandToolTip}', '${collapseToolTip}');"<#else>href="${fullUrlString}"</#if><#if expandToolTip?has_content> title="${expandToolTip}"</#if>
    <#else>
    expanded"><a <#if javaScriptEnabled>onclick="javascript:toggleScreenlet(this, '${contentId}', '${saveCollapsed?string}', '${expandToolTip}', '${collapseToolTip}');"<#else>href="${fullUrlString}"</#if><#if collapseToolTip?has_content> title="${collapseToolTip}"</#if>
    </#if>
    >&nbsp;</a></li>
    </#if>
    </#if>
  </#local>
  -->
</#function>

<#-- @section container markup - theme override 
    NOTE: class refers to cellClass (class for outer cell container) -->
<#macro section_markup_container type="" styleName="" open=true close=true sectionLevel=1 headingLevel=1 menuTitleContent="" menuTitleContentArgs={} menuContent="" menuContentArgs={} class="" 
    contentClass="" contentStyle="" contentFlagClasses="" id="" idPrefix="" title="" style="" collapsed=false contentId="" collapsible=false saveCollapsed=true 
    expandToolTip=true collapseToolTip=true padded=false showMore=true fullUrlString="" containerClass="" containerId="" containerStyle=""
    javaScriptEnabled=true fromScreenDef=false hasContent=true menuLayoutTitle="" menuLayoutGeneral="" menuRole="" requireMenu=false forceEmptyMenu=false 
    containerAttribs={} containerExcludeAttribs=[] contentAttribs={} contentExcludeAttribs=[]
    origArgs={} passArgs={} catchArgs...>
  <#if open>
    <#local containerClass = addClassArg(containerClass, "section-screenlet")>
    <#local containerClass = addClassArg(containerClass, contentFlagClasses)>
    <#if collapsed>
      <#local containerClass = addClassArg(containerClass, "toggleField")>
    </#if>
    <#-- NOTE: The ID should always be on the outermost container for @section -->
    <div<@compiledClassAttribStr class=containerClass /><#if containerId?has_content> id="${containerId}"</#if><#rt>
        <#lt><#if style?has_content> style="${style}"<#elseif containerStyle?has_content> style="${containerStyle}"</#if><#rt>
        <#lt><#if containerAttribs?has_content><@commonElemAttribStr attribs=containerAttribs exclude=containerExcludeAttribs/></#if>>
      <#-- TODO?: Is this still needed? Nothing uses collapsed and title is already used below.
      <#if collapsed><p class="alert legend">[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] ${title}</p></#if>
      -->
      <@row open=true close=false />
        <#local class = addClassArg(class, "section-screenlet-container")>
        <#local class = addClassArg(class, contentFlagClasses)>
        <#local class = addClassArgDefault(class, (styles.grid_large!"") + "12")>
        <#-- NOTE: this is same as calling class=("=" + compileClassArg(class)) to override non-essential @cell class defaults -->
        <@cell open=true close=false class=compileClassArg(class) />
          <#-- FIXME: This should not be prerendered like this, should be delegated, due to container heuristic issues and other -->
          <@contentArgRender content=menuTitleContent args=menuTitleContentArgs />
          <#-- NOTE: may need to keep this div free of foundation grid classes (for margins collapse?) -->
          <#local contentClass = addClassArg(contentClass, "section-screenlet-content")>
          <#local contentClass = addClassArg(contentClass, contentFlagClasses)>
          <div<#if contentId?has_content> id="${contentId}"</#if><@compiledClassAttribStr class=contentClass /><#if contentStyle?has_content> style="${contentStyle}"</#if><#rt>
          <#lt><#if contentAttribs?has_content><@commonElemAttribStr attribs=contentAttribs exclude=contentExcludeAttribs/></#if>>
  </#if>
            <#nested>
  <#if close>
          </div>
          
          <#if menuLayoutGeneral == "bottom" || menuLayoutGeneral == "top-bottom">
            <@contentArgRender content=menuContent args=menuContentArgs />
          </#if>
        <@cell close=true open=false />
      <@row close=true open=false />
    </div>
  </#if>
</#macro>

<#-- @section menu and title arrangement markup - theme override -->
<#macro section_markup_menutitle sectionType="" sectionStyleName="" sectionLevel=1 headingLevel=1 menuLayoutTitle="" menuLayoutGeneral="" menuRole="" 
    hasMenu=false menuContent="" menuContentArgs={} hasTitle=false titleContent="" titleContentArgs={} 
    contentFlagClasses="" fromScreenDef=false position="top" 
    menuTitleContainerClass="" titleContainerClass="" menuContainerClass="" origArgs={} passArgs={} catchArgs...>
  <#-- Currently supports only one menu. could have one for each layout (with current macro
       args as post-title), but tons of macro args needed and complicates. -->
  <#if position == "top" && menuLayoutGeneral == "bottom">
    <#local hasMenu = false>
  </#if>
  <#if menuLayoutTitle == "pre-title">
    <#if menuTitleContainerClass?has_content><div<@compiledClassAttribStr class=menuTitleContainerClass />></#if>
      <#if hasMenu>
        <#if menuContainerClass?has_content><div<@compiledClassAttribStr class=menuContainerClass  />></#if>
        <@contentArgRender content=menuContent args=menuContentArgs />
        <#if menuContainerClass?has_content></div></#if>
      </#if>
      <#if hasTitle>
        <#if titleContainerClass?has_content><div<@compiledClassAttribStr class=titleContainerClass />></#if>
        <@contentArgRender content=titleContent args=titleContentArgs />
        <#if titleContainerClass?has_content></div></#if>
      </#if>
    <#if menuTitleContainerClass?has_content></div></#if>
  <#elseif menuLayoutTitle == "inline-title">
    <#if menuTitleContainerClass?has_content><div<@compiledClassAttribStr class=menuTitleContainerClass />></#if>
      <#if hasTitle>
        <#if titleContainerClass?has_content><div<@compiledClassAttribStr class=titleContainerClass />></#if>
        <@contentArgRender content=titleContent args=titleContentArgs />
        <#if titleContainerClass?has_content></div></#if>
      </#if>
      <#if hasMenu>
        <#if menuContainerClass?has_content><div<@compiledClassAttribStr class=menuContainerClass />></#if>
        <@contentArgRender content=menuContent args=menuContentArgs />
        <#if menuContainerClass?has_content></div></#if>
      </#if>
    <#if menuTitleContainerClass?has_content></div></#if>
  <#else>
    <#if menuTitleContainerClass?has_content><div<@compiledClassAttribStr class=menuTitleContainerClass />></#if>
      <#if hasTitle>
        <#if titleContainerClass?has_content><div<@compiledClassAttribStr class=titleContainerClass />></#if>
        <@contentArgRender content=titleContent args=titleContentArgs />
        <#if titleContainerClass?has_content></div></#if>
      </#if>
      <#if hasMenu>
        <#if menuContainerClass?has_content><div<@compiledClassAttribStr class=menuContainerClass />></#if>
        <@contentArgRender content=menuContent args=menuContentArgs />
        <#if menuContainerClass?has_content></div></#if>
      </#if>
    <#if menuTitleContainerClass?has_content></div></#if>
  </#if>
</#macro>
