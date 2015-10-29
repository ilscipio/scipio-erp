<#--
* 
* Master HTML template include, default Cato markup.
*
* A set of HTML templating macros, part of standard Cato Freemarker API.
* Automatically included at all times, unless overridden by properties or themes.
* Intended to be swappable.
*
* NOTE: currently targeted toward Foundation CSS.
*
* NOTE: In general, macros expect to be called using named arguments (not supported for functions),
*     except where otherwise noted.
*
* IMPL NOTE: Macros should avoid using "request" directly (use setRequestVar/getRequestVar/other).
*
* DEV NOTE: some macros use attribs and inlineAttribs args to specify extra HTML attribs.
*     even though it would be convenient, we can't allow a "attribString" arg because no way
*     for macro to get attribs out of it if it needs them, cause problems.
*     FIXME: not all macros currently properly check attribMap for duplicate attribs
*       of args and inlineAttribs (priority should be: args - inlineAttribs - attribMap).
-->

<#-- 
*************************************
* EXTERNAL IMPORTS AND INCLUDES *
*************************************
* NOTE: Assumes utilities.ftl included.
-->

<#-- (currently none) -->

<#-- As of dependencies rework, cato libs should have no dependencies on the stock macro libraries.
    The stock macros are implemented using cato libraries.
    There should be no circular dependencies.
<#if !(screenlib?? && screenlib?is_hash)>
  <#import "component://widget/templates/htmlScreenMacroLibrary.ftl" as screenlib>
</#if>
<#if !(formlib?? && formlib?is_hash)>
  <#import "component://widget/templates/htmlFormMacroLibrary.ftl" as formlib>
</#if>
<#if !(menulib?? && menulib?is_hash)>
  <#import "component://widget/templates/htmlMenuMacroLibrary.ftl" as menulib>
</#if>
<#if !(treelib?? && treelib?is_hash)>
  <#import "component://widget/templates/htmlTreeMacroLibrary.ftl" as treelib>
</#if>-->


<#-- 
*************************************
* API TEMPLATE MACROS AND INCLUDES *
*************************************
* Intended for use in production templates.
-->

<#include "htmlScript.ftl">

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
* Heading
************
    Usage example:  
    <@heading>My Title</@heading>         
                                 
   * General Attributes *
    elemType       = [heading|h|p|span|div|raw], default heading (note: do not specify h1-h6 here - use level)
                     boolean true means use default, false same as raw (none)
    level          = specific level (1-6). If not specified, current heading level returned by
                     getCurrentHeadingLevel() function is used. 
                     note: does not consume a level.
    relLevel       = for level, uses level of current heading returned by getCurrentHeadingLevel()
                     plus this number of levels. default: 0 (current level)
    class          = heading elem classes (simple)
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    levelClassPrefix = default "heading-level-", prefix for class that will be appended level number
    id             = heading id
    consumeLevel   = boolean, default currently always false (DEV NOTE: could be made to depend on calculated level).
                     if true, the global heading level is set to (calculated level for this heading) + 1.
                     note this is better handled through use of the @section macro. mostly useful for h1.
    containerElemType = [div|] default empty. if present, adds container around title or this elem type
    containerClass    = container elem classes (simple)       
    containerId       = container id  
    attribs              = hash of other legacy h1-h6 attributes (mainly for those with dash in name)
    [inlineAttribs...]   = other legacy h1-h6 attributes, inlined
-->
<#macro heading elemType=true level="" relLevel="" class=true id="" levelClassPrefix=true consumeLevel="" 
    containerElemType=false containerClass=true containerId="" attribs={} inlineAttribs...>
  <#if !level?has_content>
    <#local level = getCurrentHeadingLevel()>
  </#if>
  <#if relLevel?has_content>
    <#local level = level + relLevel>
  </#if>
  <#if levelClassPrefix?is_boolean>
    <#local levelClassPrefix = levelClassPrefix?string(styles.heading_level_prefix!"", "")>
  </#if>
  <#if levelClassPrefix?has_content>
    <#local headingLevelClass = levelClassPrefix + level?string>
  <#else>
    <#local headingLevelClass = "">
  </#if>
  <#local classes = makeClassesArg(class, headingLevelClass)>
  <#local containerClasses = makeClassesArg(containerClass, headingLevelClass)>
  <#if (consumeLevel?is_boolean && consumeLevel == true)>
    <#local dummy = setCurrentHeadingLevel(level + 1)>
  </#if>
  <#if (level < 1)>
    <#local level = 1>
  </#if>
  <#if elemType?is_boolean>
    <#if elemType>
      <#local hElem = "h">
    <#else>
      <#local hElem = "">
    </#if>
  <#elseif elemType == "heading" || elemType == "h">
    <#local hElem = "h">
  <#elseif elemType == "raw" || !elemType?has_content>
    <#local hElem = "">
  <#else>
    <#local hElem = elemType>
  </#if>
  <#if containerElemType?is_boolean>
    <#local cElem = "">
  <#elseif containerElemType == "raw" || !containerElemType?has_content>
    <#local cElem = "">
  <#else>
    <#local cElem = containerElemType> 
  </#if>
  <@heading_markup level=level elem=hElem classes=classes id=id attribs=concatMaps(attribs, inlineAttribs) excludeAttribs=["class", "id"] 
      containerElem=cElem containerClasses=containerClasses containerId=containerId><#nested></@heading_markup>
</#macro>

<#-- Main markup for @heading (minimal logic; a little needed) - may be overridden
     This may be overridden by themes to change markup without changing logic.
     Here, elem will contain either the value "h" or a valid html element.
     NOTE: wherever this is overridden, should include "extraArgs..." for compatibility (new args won't break old overrides; remove to identify) -->
<#macro heading_markup level=1 elem="" classes="" id="" attribs={} excludeAttribs=[] containerElem="" containerClasses="" containerId="" extraArgs...>
  <#local elemLevel = level>
  <#if (elemLevel > 6)>
    <#local elemLevel = 6>
  </#if>
  <#if elem == "h">
    <#local elem = "h" + elemLevel?string>
  </#if>
  <#if containerElem?has_content>
    <${containerElem}<#if containerClasses?has_content> class="${containerClasses}"</#if><#if containerId?has_content> id="${containerId}"</#if>>
  </#if>
  <#if elem?has_content><${elem}<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#rt>
    <#lt><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if><#nested><#if elem?has_content></${elem}></#if>
  <#if containerElem?has_content>
    </${containerElem}>
  </#if>
</#macro>

<#-- 
*************
* Code Block
************
Creates a very basic wrapper for code blocks
    Usage example:  
    <@code type="java">
       //Some java content
    </@code>
                    
   * General Attributes *
    type            = (html|java|css|javascript|log) (default:html) 
-->
<#macro code type="html">
    <pre><code data-language="${type!}"><#compress>
    <#nested>
    </#compress>
    </code></pre>
</#macro>

<#-- 
*************
* Modal Macro
************
    Usage example:  
    <@modal id="dsadsa" attr="" >
    modal Content 
    </@modal>                
   * General Attributes *
    id              = set id (required)
    label           = set anchor text (required)
    icon            = generates icon inside the link (Note: has to be the full set of classes, e.g. "fa fa-fw fa-info")
-->
<#macro modal id label href="" icon="">
    <a href="#" data-reveal-id="${id}_modal" <#if href?has_content>data-reveal-ajax="${href!}"</#if>><#if icon?has_content><i class="${icon!}"></i> </#if>${label}</a>
    <div id="${id}_modal" class="${styles.modal_wrap!}" data-reveal>
        <#nested>
        <a class="close-reveal-modal">&#215;</a>
    </div>
</#macro>

<#-- 
*************
* Alert box
************
Alert box for messages that should grab user attention.
NOTE: Should avoid using this for regular, common inlined message results such as "no records found" unless
it's an unexpected result, error or one that requires user action. See other macros such as @resultMsg and @errorMsg.

    Usage example:  
    <@alert type="info">
        <#nested>
    </@alert>            
                    
   * General Attributes *
    type           = (info|success|warning|secondary|alert|error), default info
    class          = classes or additional classes for nested container
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro alert type="info" class=true id="">
<#local classes = makeClassesArg(class, "${styles.grid_large!}12")>
<#local typeClass = "alert_type_${type!}"/>
<#if type="error"><#local type = "alert"></#if>
<div class="${styles.grid_row!}"<#if id?has_content> id="${id}"</#if>>
   <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
       <div data-alert class="${styles.alert_wrap!} ${styles[typeClass]!}">
           <div class="${styles.grid_row!}">
              <div class="<#if classes?has_content>${classes} </#if>${styles.grid_cell!}">
                  <a href="#" class="close" data-dismiss="alert">&times;</a>
                  <#nested>
                  </div>
              </div>
           </div>
       </div>
   </div>
</#macro>

<#--
*************
* Panel box
************
    Usage example:  
    <@panel type="">
        <#nested>
    </@panel>            
                    
   * General Attributes *
    type           = (callout|) default:empty
    title          = Title
-->
<#macro panel type="" title="">
<div class="${styles.panel_wrap!} ${type}">
  <div class="${styles.panel_head!}"><#if title?has_content><h5 class="${styles.panel_title!}">${title!}</h5></#if></div>
  <div class="${styles.panel_body!}"><p><#nested></p></div>
</div>
</#macro>

<#-- 
*************
* Table
************
Helps define an HTML table. Required wrapper for all @table sub-element macros.

    Usage example:  
    <@table type="data-list" id="my-table">
      <@thead>
        <@tr>
          <@th width="15%">col 1</@th>
          <@th width="85%">col 2</@th>
        </@tr>
      </@thead>
      <@tbody>
        <@tr class="my-row-class" valign="middle">
          <@td>data value 1</@td>
          <@td>data value 2</@td>
        </@tr>
      </@tbody>
    </@table>
                    
   * General Attributes *
    type            = [generic|(theme-specific)], default generic
                      * STANDARD TYPES *
                      These types must always be recognized by all styles themes:
                      generic: generic html table (free-form, complex); no features enabled by default.
                               similar to defining an html <table> manually, but more powerful.
                      * DEFAULT STYLES TYPES *
                      WARN: these are WIP types, may not be enough (important part is to label things for easy search)
                      The following are currently recognized by the default cato styles (NOTE: targeted for backend):
                      data-list: record-containing table, one data record per row (but row cells may be complex and may have tfoot)
                                 similar to a form widget "list" or "multi" table; intended to resemble these, to unify them.
                      data-complex: record-containing table, but with complex structure (more than one row per record, separators, etc.)
                                    there is no form widget equivalent of these and usually need some custom alt-row work.
                      summary: usually table with one or a few set rows of summary totals
                               e.g. order grand totals. 
                               TODO? review need for this type (should be converted?)
                      fields: label-value pairs for display, side-by-side, usually no header, roughly
                              this is especially for legacy Ofbiz code. it is somewhat still valid for display-only fields.
                              legacy Ofbiz code tables may be assigned this for input forms formatted with tables, but they
                              ultimately belong as @field and @row/@cell.
                              TODO: many of these in current templates involving forms and inputs should be converted to @row/@cell (WIP)
    class           = manual classes to add, as string, default depends on table type
                      if specified as string, replaces defaults (class=false prevents class), unless prefixed with "+"
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
                      defaults are looked up in the styles hash using:
                      styles["table_" + type?replace("-","_")]
                      where type is the table type above. if the given hash entry does not exist, the default is instead determined by:
                      styles["table_default"]
    id              = table id
    autoAltRows     = defaults specified in styles hash, but otherwise false
    firstRowAlt     = default false
    inheritAltRows  = only for nested tables: if true, all rows in nested tables will inherit alt from parent table row
    useFootAltRoots = whether use alt row logic in foot or not
    cellspacing     = cellspacing, defaults specified in styles hash, set to "" to prevent setting.
    wrapIf/openOnly/closeOnly = advanced structure control, for esoteric cases
    attribs         = hash of other legacy <table attributes (mainly for those with dash in name)
    
    * Responsive options *
    scrollable      = will rely on the jquery plugin datatables.js (www.datatables.net) to generate responsive table. Can be combined with fixed column type
    fixedColumnsLeft  = int value; number of columns that are fixed on the left-hand side
    fixedColumnsRight = int value;number of columns that are fixed on the right hand side
    [inlineAttribs...]    = other legacy <table attributes and values, inlined
-->
<#macro table type="" class=true id="" cellspacing=true scrollable=false autoAltRows="" firstRowAlt="" inheritAltRows=false useFootAltRows=false wrapIf=true openOnly=false closeOnly=false fixedColumnsLeft=0 fixedColumnsRight=0 attribs={} inlineAttribs...>
<#local fieldIdNum = getRequestVar("catoFieldIdNum")!0>
<#local fieldIdNum = fieldIdNum + 1 />
<#local dummy = setRequestVar("catoFieldIdNum", fieldIdNum)>
<#if !id?has_content><#local id="table_"+fieldIdNum/></#if>
<#if !type?has_content>
  <#local type = "generic">
</#if>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#-- save previous globals, for nesting -->
  <#local prevTableInfo = getRequestVar("catoCurrentTableInfo")!{}>
  <#local prevSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local prevRowAltFlag = getRequestVar("catoCurrentTableRowAltFlag")!""> <#-- used to keep track of state (always boolean) -->
  <#local prevCurrentRowAlt = getRequestVar("catoCurrentTableCurrentRowAlt")!""> <#-- the actual alt value of current row (may be empty) -->
  <#local prevLastRowAlt = getRequestVar("catoCurrentTableLastRowAlt")!""> <#-- the actual alt value of "last" row (may be empty) -->
  <#local styleName = type?replace("-","_")>
  <#if (!styleName?has_content) || (!(styles["table_" + styleName]!false)?is_string)>
    <#local styleName = "default">
  </#if>
  <#if !autoAltRows?is_boolean>
    <#if inheritAltRows>
      <#local autoAltRows = true>
    <#else>
      <#local autoAltRows = styles["table_" + styleName + "_autoaltrows"]!styles["table_default_autoaltrows"]!false>
    </#if>
  </#if>
  <#local defaultClass = styles["table_" + styleName]!styles["table_default"]!"">
  <#local classes = makeClassesArg(class, defaultClass)>
  <#if cellspacing?is_boolean>
    <#if cellspacing>
      <#local cellspacing = styles["table_" + styleName + "_cellspacing"]!styles["table_default_cellspacing"]!"">
    <#else>
      <#local cellspacing = "">
    </#if>
  </#if>
  <#local catoCurrentTableInfo = {"type": type, "styleName": styleName, "autoAltRows": autoAltRows,
    "inheritAltRows": inheritAltRows, "parentRowAlt": prevCurrentRowAlt, "useFootAltRows": useFootAltRows}>
  <#local dummy = setRequestVar("catoCurrentTableInfo", catoCurrentTableInfo)!>
  <#local catoCurrentTableSectionInfo = {"type": "body", "cellElem": "td"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- note: catoCurrentTableRowAltFlag should always be boolean
       note: catoCurrentTableCurrentRowAlt probably doesn't need to be set here, but playing it safe -->
  <#if firstRowAlt?is_boolean>
    <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", firstRowAlt)!>
    <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", firstRowAlt)!>
  <#elseif inheritAltRows>
    <#if prevCurrentRowAlt?is_boolean>
      <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", prevCurrentRowAlt)!>
    <#else>
      <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", false)!>
    </#if>
    <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", prevCurrentRowAlt)!>
  <#else>
    <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", false)!>
    <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", false)!>
  </#if>
  <#-- note: this var may be empty string (none) -->
  <#local dummy = setRequestVar("catoCurrentTableLastRowAlt", prevCurrentRowAlt)!>
  <#local style = "">
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableStack", 
        {"prevTableInfo":prevTableInfo, "prevSectionInfo":prevSectionInfo, "prevRowAltFlag":prevRowAltFlag, 
         "prevCurrentRowAlt":prevCurrentRowAlt, "prevLastRowAlt":prevLastRowAlt, "scrollable":scrollable})>
  </#if>
  <table<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#rt>
    <#lt><#if cellspacing?has_content> cellspacing="${cellspacing}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id", "cellspacing"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableStack")!{}>
    <#local prevTableInfo = stackValues.prevTableInfo>
    <#local prevSectionInfo = stackValues.prevSectionInfo>
    <#local prevRowAltFlag = stackValues.prevRowAltFlag>
    <#local prevCurrentRowAlt = stackValues.prevCurrentRowAlt>
    <#local prevLastRowAlt = stackValues.prevLastRowAlt>
    <#local scrollable = stackValues.scrollable>
  </#if>
  </table>
  <#if scrollable>
  <script type="">
    $(document).ready(function() {
        var ${id!} = $('#${id}').DataTable( {
            fixedHeader: true,
            scrollX: true,
            info: false,
            paging: false,
            searching : false
            <#if fixedColumnsLeft&gt;0 || fixedColumnsRight&gt;0>,fixedColumns:   {
            leftColumns: ${fixedColumnsLeft!0},
            rightColumns: ${fixedColumnsRight!0}
            }
            </#if>
            
        } );
    } );
  </script>
  </#if>
  <#local dummy = setRequestVar("catoCurrentTableInfo", prevTableInfo)!>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevSectionInfo)!>
  <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", prevRowAltFlag)!>
  <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", prevCurrentRowAlt)!>
  <#local dummy = setRequestVar("catoCurrentTableLastRowAlt", prevLastRowAlt)!>
</#if>
</#macro>

<#macro thead class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#local classes = makeClassesArg(class, "")>
  <#local prevTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local catoCurrentTableSectionInfo = {"type": "head", "cellElem": "th"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableHeadStack", 
        {"prevTableSectionInfo":prevTableSectionInfo})>
  </#if>
  <thead<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableHeadStack")!{}>
    <#local prevTableSectionInfo = stackValues.prevTableSectionInfo>
  </#if>
  </thead>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevTableSectionInfo)!>
</#if>
</#macro>

<#macro tbody class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#local classes = makeClassesArg(class, "")>
  <#local prevTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local catoCurrentTableSectionInfo = {"type": "body", "cellElem": "td"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableBodyStack", 
        {"prevTableSectionInfo":prevTableSectionInfo})>
  </#if>
  <tbody<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableBodyStack")!{}>
    <#local prevTableSectionInfo = stackValues.prevTableSectionInfo>
  </#if>
  </tbody>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevTableSectionInfo)!>
</#if>
</#macro>

<#macro tfoot class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
  <#local classes = makeClassesArg(class, "")>
  <#local prevTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
  <#local catoCurrentTableSectionInfo = {"type": "foot", "cellElem": "td"}>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", catoCurrentTableSectionInfo)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableFootStack", 
        {"prevTableSectionInfo":prevTableSectionInfo})>
  </#if>
  <tfoot<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableFootStack")!{}>
    <#local prevTableSectionInfo = stackValues.prevTableSectionInfo>
  </#if>
  </tfoot>
  <#local dummy = setRequestVar("catoCurrentTableSectionInfo", prevTableSectionInfo)!>
</#if>
</#macro>

<#-- 
*************
* Table row
************
Helps define table rows. takes care of alt row styles. must have a parent @table wrapper. 
                     
   * General Attributes *
    type            = [generic|content|meta|util], default depends on table type and styles hash; 
                          in complete absence of styles hash, default is "generic";
                          in default cato styles, default is "generic" for "generic" tables, and "content" for all other table types.
                      generic: free-form row with no assumptions on content.
                      content: normal data or content row. exact meaning depends on table type.
                               note that for "data-complex" this definition is currently relaxed.
                      meta: indicates this is a special info/status row (e.g. "No Records Found" message), not an actual content row.
                            meta rows are treated differently by default as are thead and tfoot rows.
                            exact meaning depends on table type.
                      util: indicates this is a special utility-only row meant to hold no real data, 
                            such as: spacer rows (<@tr type="util"><@td colspan=3><hr /></@td></@tr>)
                            TODO: this isn't handled yet but SHOULD be used in templates anyhow.
    class           = css classes
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    id              = row id
    useAlt          = boolean, if specified, can manually enable/disable whether alternate row code runs per-row
    alt             = boolean, if specified, override the automatic auto-alt styling to specific value true or false (manual mode)
                      note: at current time, alt on non-body rows (except foot rows if enabled in @table) does not affect
                            next row's alt (unless groupLast used explicit on next) logic
    groupLast       = boolean, if specified, considers row logically grouped with last row;
                      sets alt to exact same as last row
    groupParent     = boolean, nested tables only, if specified, considers row logically grouped with parent row;
                      sets alt to exact same as parent row
    selected        = boolean, if specified and true marked as selected
    wrapIf/openOnly/CloseOnly = advanced structure control, for esoteric cases (should omit nested for openOnly/closeOnly)
    attribs               = hash of other legacy <tr attributes (mainly for those with dash in name)
    [inlineAttribs...]    = other legacy <tr attributes and values, inlined
-->
<#macro tr type="" class=true id="" useAlt="" alt="" groupLast="" groupParent="" selected="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#local catoCurrentTableInfo = getRequestVar("catoCurrentTableInfo")!{}>
<#local catoCurrentTableSectionInfo = getRequestVar("catoCurrentTableSectionInfo")!{}>
<#local catoCurrentTableRowAltFlag = getRequestVar("catoCurrentTableRowAltFlag")!false>
<#local catoCurrentTableLastRowAlt = getRequestVar("catoCurrentTableLastRowAlt")!"">
<#if open>
  <#local tableType = (catoCurrentTableInfo.type)!"generic">
  <#local tableStyleName = (catoCurrentTableInfo.styleName)!tableType>
  <#local sectionType = (catoCurrentTableSectionInfo.type)!"body">
  <#if !type?has_content>
    <#local type = styles["table_" + tableStyleName + "_rowtype"]!styles["table_default_rowtype"]!"generic">
  </#if>
  <#local metaRow = (type == "meta")>
  <#local isRegAltRow = !metaRow && ((sectionType == "body") || (sectionType == "foot" && ((catoCurrentTableInfo.useFootAltRows)!)==true))>
  <#if !(useAlt?is_boolean && useAlt == false)>
    <#if !alt?is_boolean>
      <#if groupLast?is_boolean && groupLast == true>
        <#local alt = catoCurrentTableLastRowAlt!""> <#-- may be empty string (none) -->
      <#elseif groupParent?is_boolean && groupParent == true>
        <#local alt = (catoCurrentTableInfo.parentRowAlt)!"">
      <#elseif (isRegAltRow && ((catoCurrentTableInfo.autoAltRows)!false)==true)>
        <#if ((catoCurrentTableInfo.inheritAltRows)!false)==true>
          <#local alt = (catoCurrentTableInfo.parentRowAlt)!"">
        <#else>
          <#local alt = catoCurrentTableRowAltFlag!false> <#-- always boolean -->
        </#if>
      <#elseif useAlt?is_boolean && useAlt == true>
        <#-- forced -->
        <#local alt = catoCurrentTableRowAltFlag!false>
      </#if>
    </#if>
  </#if>
  <#-- save the "effective" or "real" current row alt -->
  <#local catoCurrentTableCurrentRowAlt = alt>
  <#local dummy = setRequestVar("catoCurrentTableCurrentRowAlt", catoCurrentTableCurrentRowAlt)!>
  <#-- need to save values on a stack if open-only! -->
  <#if !close>
    <#local dummy = pushRequestStack("catoCurrentTableRowStack", 
        {"type":type, "useAlt":useAlt, "alt":alt, "isRegAltRow":isRegAltRow})>
  </#if>
  <#local classes = makeClassesArg(class, "")>
  <#if alt?is_boolean>
    <#local classes = (classes + " " + alt?string(styles.row_alt!, styles.row_reg!))?trim>
  </#if>
  <#if selected?is_boolean && selected == true>
    <#local classes = (classes + " " + styles.row_selected!)?trim>
  </#if>
  <tr<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
</#if>    
    <#nested>
<#if close>
  <#-- need to get values back from stack if close-only! -->
  <#if !open>
    <#local stackValues = popRequestStack("catoCurrentTableRowStack")!{}>
    <#local type = stackValues.type>
    <#local useAlt = stackValues.useAlt>
    <#local alt = stackValues.alt>
    <#local isRegAltRow = stackValues.isRegAltRow>
  </#if>
  </tr>
  <#if !(useAlt?is_boolean && useAlt == false)>
    <#-- note: isRegAltRow check here could be removed but maybe better to keep? only auto-toggle for regular rows... -->
    <#if alt?is_boolean && isRegAltRow> <#-- not needed:  && ((catoCurrentTableInfo.inheritAltRows)!)==false -->
      <#local catoCurrentTableRowAltFlag = !alt>
      <#local dummy = setRequestVar("catoCurrentTableRowAltFlag", catoCurrentTableRowAltFlag)!>
    </#if>
  </#if>
  <#-- note: may be empty string, that's ok, will record if last was disabled so groupLast always makes sense -->
  <#local catoCurrentTableLastRowAlt = alt>
  <#local dummy = setRequestVar("catoCurrentTableLastRowAlt", catoCurrentTableLastRowAlt)!>
</#if>
</#macro>

<#-- 
*************
* Table cell
************
Helps define table cells.
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    id              = cell id
    wrapIf/openOnly/CloseOnly = advanced structure control, for esoteric cases (should omit nested for openOnly/closeOnly)
    attribs               = hash of other legacy <th and <td attributes (mainly for those with dash in name)
    [inlineAttribs...]    = other legacy <th and <td attributes and values
-->
<#macro th class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
  <#local classes = makeClassesArg(class, "")>
  <#if open><th<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>></#if><#nested><#if close></th></#if>
</#macro>

<#macro td class=true id="" wrapIf=true openOnly=false closeOnly=false attribs={} inlineAttribs...>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
  <#local classes = makeClassesArg(class, "")>
  <#if open><td<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>></#if><#nested><#if close></td></#if>
</#macro>

<#-- 
*************
* Table row class string
************
Helps build common data/table row class string (odd, even, etc.). Common pattern.
In general, use @table, @tr macros instead.

    Usage example:  
    <tr<@tableRowClassStr class="myClass" alt=false/>>
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    alt             = boolean, if true is alternate row (odd), if false regular (even)
    selected        = boolean, if true marked as selected
-->
<#macro tableRowClassStr class=true alt="" selected="">
  <#local classes = makeClassesArg(class, "")>
  <#if alt?is_boolean>
    <#local classes = (classes + " " + alt?string(styles.row_alt!, styles.row_reg!))?trim>
  </#if>
  <#if selected?is_boolean && selected == true>
    <#local classes = (classes + " " + styles.row_selected!)?trim>
  </#if>
  <#if classes?has_content> class="${classes}"</#if>
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
* Pricing table
************
Since this is very foundation specific, this function may be dropped in future installations.

    Usage example:  
    <@pul >
        <@pli>Text or <a href="">Anchor</a></@pli>
    </@pul>            
                    
   * General Attributes *
    title           = fieldset-title
    
-->
<#macro pul title="">
          <ul class="${styles.pricing_wrap!}">
              <@pli type="title">${title!}</@pli>
              <#nested>
          </ul>
</#macro>

<#macro pli type="">
    <#switch type>
          <#case "price">
              <li class="${styles.pricing_price!}"><#nested></li>
          <#break>
          <#case "description">
              <li class="${styles.pricing_description!}"><#nested></li>
          <#break>
          <#case "title">
              <li class="${styles.pricing_title!}"><#nested></li>
          <#break>
          <#case "button">
              <li class="${styles.pricing_cta!}"><#nested></li>
          <#break>        
          <#default>
              <li class="${styles.pricing_bullet!}"><#nested></li>
          <#break>
    </#switch>
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
* Chart Macro
************
Foundation Pizza: http://zurb.com/playground/pizza-amore-charts-and-graphs (customization through _base.scss)
Chart.js: http://www.chartjs.org/docs/ (customization through _charsjs.scss)

    Usage example:  
    <@chart type="bar" >
        <@chartdata value="36" title="Peperoni"/> 
    </@chart>              
                    
   * General Attributes *
    type           = (pie|bar|line) (default:pie)
    library        = (foundation|chart) (default:foundation)
    title          = Data Title  (default:empty)
-->
<#macro chart type="pie" library="foundation" title="">
    <#global chartLibrary = library!"foundation"/>
    <#local nestedContent><#nested><#t></#local>
    <#local fieldIdNum = getRequestVar("catoFieldIdNum")!0>
    <#local fieldIdNum = fieldIdNum + 1 />
    <#local dummy = setRequestVar("catoFieldIdNum", fieldIdNum)>
    <#if chartLibrary=="foundation">
        <@row>
        <@cell columns=3>    
        <ul data-${type!}-id="chart_${renderSeqNumber!}_${fieldIdNum!}" class="${styles.chart_legend!}">
            <#nested/>
            <#--<#if !nestedContent?has_content><@chartdata value="0" title=""/></#if>-->
        </ul>
        </@cell>
        <@cell columns=9><div id="chart_${renderSeqNumber!}_${fieldIdNum!}" style="height:300px;"></div></@cell>
        </@row>
    <#else>
        <#global chartId = "chart_${renderSeqNumber!}_${fieldIdNum!}"/>
        <#global chartType = type/>
        <canvas id="${chartId!}" class="${styles.grid_large!}12 chart-data" height="300" style="height:300px;"></canvas>
        <script type="text/javascript">
        //<![CDATA[
            $(function(){
                var chartDataEl = $('.chart-data:first-of-type');
                var chartData = chartDataEl.sassToJs({pseudoEl:":before", cssProperty: "content"});
                var options ={
                    animation: false,
                    responsive: true,
                    maintainAspectRatio: true,
                    scaleLineColor: chartData.scaleLineColor,
                    scaleFontFamily: chartData.scaleFontFamily,
                    scaleFontSize: chartData.scaleFontSize,
                    scaleFontColor: chartData.scaleFontColor,
                    scaleShowLabels: chartData.scaleShowLabels,
                    scaleShowLabels: chartData.scaleShowLabels,
                    scaleShowLine : chartData.scaleShowLine,
                    angleShowLineOut : chartData.angleShowLineOut,
                    scaleBeginAtZero : chartData.scaleBeginAtZero,
                    showTooltips: chartData.showTooltips,
                    tooltipFillColor: chartData.tooltipFillColor,
                    tooltipFontFamily: chartData.tolltipFontFamily,
                    tooltipFontSize: chartData.tooltipFontSize,
                    tooltipFontStyle: chartData.tooltipFontStyle,
                    tooltipFontColor: chartData.tooltipFontColor,
                    tooltipTitleFontFamily: chartData.tooltipTitleFontFamily,
                    tooltipTitleFontSize: chartData.tooltipTitleFontSize,
                    tooltipTitleFontStyle: chartData.tooltipTitleFontStyle,
                    tooltipTitleFontColor: chartData.tooltipTitleFontColor,
                    tooltipYPadding: chartData.tooltipYPadding,
                    tooltipXPadding: chartData.tooltipXPadding,
                    tooltipCaretSize: chartData.tooltipCaretSize,
                    tooltipCornerRadius: chartData.tooltipCornerRadius,
                    datasetFill : chartData.datasetFill,
                    tooltipTemplate: "<%if (label){%><%=label%>: <%}%><%= value %>",
                    multiTooltipTemplate: "<%= value %>",
                    pointDot : chartData.pointDot,
                    pointHitDetectionRadius : chartData.pointHitDetectionRadius,
                    pointDotRadius : chartData.pointDotRadius,
                    pointDotStrokeWidth : chartData.pointDotStrokeWidth,
                    <#if type=="line">
                    bezierCurve : chartData.bezierCurve,
                    bezierCurveTension : chartData.bezierCurveTension,
                    legendTemplate : "<ul class=\"legend <%=name.toLowerCase()%>-legend\"><% for (var i=0; i<datasets.length; i++){%><li style=\"color:<%=datasets[i].strokeColor%> !important \"><span style=\"color:#efefef !important \"><%=datasets[i].value%>  <%if(datasets[i].label){%><%=datasets[i].label%><%}%></span></li><%}%></ul>",
                    dataLabels: chartData.dataLabels
                    <#elseif type=="pie">
                    legendTemplate : "<ul class=\"legend <%=name.toLowerCase()%>-legend\"><% for (var i=0; i<segments.length; i++){%><li style=\"color:<%=segments[i].fillColor%> !important \"><span style=\"color:#efefef !important \"><%=segments[i].value%>  <%if(segments[i].label){%><%=segments[i].label%><%}%></span></li><%}%></ul>"
                    <#else>
                    legendTemplate : "<ul class=\"<%=name.toLowerCase()%>-legend\"><% for (var i=0; i<datasets.length; i++){%><li><span style=\"background-color:<%=datasets[i].strokeColor%>\"></span><%if(datasets[i].label){%><%=datasets[i].label%><%}%></li><%}%></ul>"
                    </#if>
                    };
                var ctx_${renderSeqNumber!}_${fieldIdNum!} = $('#${chartId!}').get(0).getContext("2d");
                <#if type=="pie">
                var data = [];
                <#else>
                var data = {
                        labels :[],
                        datasets: [
                            {
                              fillColor: chartData.fillColor,
                              strokeColor: chartData.strokeColor,
                              pointColor: chartData.pointColor,
                              pointStrokeColor: chartData.pointStrokeColor,
                              pointHighlightFill: chartData.pointHighlightFill,
                              pointHighlightStroke: chartData.pointHighlightStroke,
                              label: "",
                              data: []
                            }
                            ]
                    };
                </#if>
                var ${chartId!} = new Chart(ctx_${renderSeqNumber!}_${fieldIdNum!})<#if type=="bar">.Bar(data,options);</#if><#if type=="line">.Line(data,options);</#if><#if type=="pie">.Pie(data,options);</#if>
                <#nested/>
            });
        //]]>
        </script>
    </#if>
</#macro>

<#macro chartdata title value value2="">
    <#if !chartLibrary?has_content> <#local chartLibrary = "foundation"/></#if>
    <#if chartLibrary=="foundation">
        <li <#if value2?has_content>data-y="${value!}" data-x="${value2!}"<#else>data-value="${value!}"</#if>>${title!}</li>
    <#else>
        <#if chartType="line" || chartType="bar">
            ${chartId!}.addData([<#if value?has_content>${value!}</#if>]<#if title?has_content>,"${title!}"</#if>);
        <#else>
            ${chartId!}.addData({value:${value!},color:chartData.color,highlight: chartData.highlight<#if title?has_content>,label:"${title!}"</#if>});
        </#if>
    </#if>
</#macro>






<#include "htmlNav.ftl">






<#-- 
*************
* Section Macro
************
Creates a logical section with optional title and menu. Automatically handles heading sizes
and keeps track of section nesting for whole request, even across screens.render calls.

Note: use getCurrentHeadingLevel and getCurrentSectionLevel functions if need to get current
levels manually, but most often should let @section menu handle them.

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
    <#local addClass = parseAddClassArg(class)>
    <#local class = parseClassArg(class, "")>
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
    <@section_impl id=id collapsibleAreaId=contentId title=title classes=class padded=padded menuContent=menuContent fromWidgets=false menuClass=menuClass menuId=menuId menuLayout=menuLayout menuRole=menuRole requireMenu=requireMenu 
        forceEmptyMenu=forceEmptyMenu hasContent=hasContent autoHeadingLevel=autoHeadingLevel headingLevel=headingLevel relHeadingLevel=relHeadingLevel defaultHeadingLevel=defaultHeadingLevel titleStyle=titleClass addClasses=addClass
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
    autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel="" addClasses="" openOnly=false closeOnly=false wrapIf=true>

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

    <#local classes = (classes + " " + addClasses)?trim>

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
            <div class="section-screenlet-container<#if classes?has_content> ${classes}<#else> ${styles.grid_large!}12</#if><#if addClasses?has_content> ${addClasses}</#if> ${styles.grid_cell!}<#if contentFlagClasses?has_content> ${contentFlagClasses}</#if>">
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





<#include "htmlForm.ftl">




<#-- 
*************
* Query result message
************
Common query result message.
Note: this is ONLY for expected, non-error messages, such as no records found in a query.
Other messages such as for missing params/record IDs are usually errors.

    Usage example:  
    <@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>            
                    
   * General Attributes *
    class       = classes or additional classes for nested container
                  (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro resultMsg class=true id="">
  <#local classes = makeClassesArg(class, "result-msg")>
  <p<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if>><#nested></p>
</#macro>

<#-- 
*************
* Error result message
************
Common error result message.
Abstracts/centralizes method used to display error, since of no consequence to most
templates: currently @alert.

    Usage example:  
    <@errorMsg type="permission">${uiLabelMap.CommonNoPermission}.</@errorMsg>            
                    
   * General Attributes *
    type           = [permission|security|error], default error
    class          = classes or additional classes for nested container
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro errorMsg type="error" class=true id="">
  <@alert type="error" class=class id=id><#nested></@alert>
</#macro>

