<#--
* 
* Master HTML templating macros/templates include, default Cato markup.
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
* DEV NOTE: although intended as swappable, the way to reuse/extend this file needs clarified.
*     currently contains much logic code and includes all the widget macros (see FIXMEs). can #include this file
*     and override macros, but widget macro includes makes that very heavy, and still logic reuse problems.
*
* DEV NOTE: some macros use attribs and inlineAttribs args to specify extra HTML attribs.
*     even though it would be convenient, we can't allow a "attribString" arg because no way
*     for macro to get attribs out of it if it needs them, cause problems.
*     FIXME: not all macros currently properly check attribMap for duplicate attribs
*       of args and inlineAttribs (priority should be: args - inlineAttribs - attribMap).
*
* TODO?: there could be a template helpers file to help separate template macro-related logic from markup (less copy-paste).
*     Do not put functions closely related to these macros in utilities.ftl.
*     For now, try to keep markup generalized and parametrizable via htmlVariables.ftl as much as possible.
*
-->

<#-- 
*************************************
* IMPORTS AND INCLUDES *
*************************************
* NOTE: Assumes catoUtilities.ftl included.
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
* API TEMPLATE MACROS *
*************************************
* Intended for use in production templates.
-->

<#--
*************
* HTML Head open
************
Opens an HTML document and header section.

    Usage example:  
    <@htmlHeadOpen />            
                    
   * General Attributes *
    includeDocType      = boolean, default false (included by screen renderer, @renderScreenBegin)
-->
<#macro htmlHeadOpen includeDocType=false>
<#if includeDocType><!DOCTYPE html></#if><#t>
<#if locale??><#local docLangAttr = locale.toString()?replace("_", "-")></#if>
<#local langDir = "ltr">
<#if docLangAttr?? && "ar.iw"?contains(docLangAttr?substring(0, 2))>
    <#local langDir = "rtl">
</#if>
<!--[if IE 9]><html class="lt-ie10" lang="${docLangAttr!"en"}" <#if langDir??>dir="${langDir}"</#if>> <![endif]-->
<html class="no-js" lang="${doctLangAttr!"en"}"<#if langDir?has_content> dir="${langDir!}"</#if>>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</#macro>

<#-- 
*************
* Script
************
Inline script wrapper.

    Usage example:  
    <@script>
        jQuery(document).ready(function() {
            alert("Page loaded.");
        });
    </@script>         
                    
   * General Attributes *
    type            = script type identifier
    language        = language identifier
    src             = source
    forceInline     = if true, the script must be inlined in the markup where the macro is used
                      and should never be delegated. in most cases this should be omitted.
-->
<#macro script type="text/javascript" language="" src="" ofbizContentSrc="" forceInline=false>
<#if ofbizContentSrc?has_content>
  <script type="${type}"<#if language?has_content> language="${language}"</#if> src="<@ofbizContentUrl>${ofbizContentSrc}</@ofbizContentUrl>"></script>
<#elseif src?has_content>
  <script type="${type}"<#if language?has_content> language="${language}"</#if> src="${src}"></script>
<#else>
  <script type="${type}"<#if language?has_content> language="${language}"</#if>>
  //<![CDATA[
    <#nested>
  //]]>
  </script>
</#if>
</#macro>

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
* Nav list
************
Since this is very foundation specific, this function may be dropped in future installations

    Usage example:  
    <@nav type="">
        <li>Text or <a href="">Anchor</a></li>
    </@nav>
    
    Or:
    <@nav type="magellan">
        <@mli arrival="MyTargetAnchor">Text or <a href="">Anchor</a></@mli>
    </@nav>
    
    <h3 ${mtarget("id")}>Jump Destination</h3>           
                    
   * General Attributes *
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
                    
   * General Attributes *
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
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: make sure always do this from now on here... -->
  <#local type = inlineArgs.type!args.type!"generic">
  <#local inlineItems = inlineArgs.inlineItems!args.inlineItems!false>
  <#if inlineArgs.class??>
    <#local class = combineClassArgs(args.class!true, inlineArgs.class)>
  <#else>
    <#local class = args.class!true>
  </#if>
  <#local id = inlineArgs.id!args.id!"">
  <#local style = inlineArgs.style!args.style!"">
  <#local attribs = inlineArgs.attribs!args.attribs!"">
  <#local items = inlineArgs.items!args.items!true>
  <#local preItems = inlineArgs.preItems!args.preItems!true>
  <#local postItems = inlineArgs.postItems!args.postItems!true>
  <#local sort = inlineArgs.sort!args.sort!false>
  <#local sortBy = inlineArgs.sortBy!args.sortBy!"">
  <#local sortDesc = inlineArgs.sortDesc!args.sortDesc!false>
  <#local nestedFirst = inlineArgs.nestedFirst!args.nestedFirst!false>
  <#local htmlWrap  = inlineArgs.htmlWrap!args.htmlWrap!"ul"/>
  <#t>
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
  <#t>
  <#local classes = makeClassesArg(class, styles["menu_" + styleName]!styles["menu_default"]!"")>
  <#t>
  <@menu_markup classes=classes id=id style=style attribs=attribs excludeAttribs=["class", "id", "style"] inlineItems=inlineItems htmlWrap=htmlWrap>
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
  <#t>
  <#global catoCurrentMenuInfo = prevMenuInfo>
  <#global catoCurrentMenuItemIndex = prevMenuItemIndex>
  <#global catoLastMenuInfo = menuInfo>
</#macro>

<#-- Markup for @menu container, with minimal logic - may be overridden
     NOTE: inlineItems is included in case needs different effect per-theme (and ugly to factor out) -->
<#macro menu_markup classes="" id="" style="" attribs={} excludeAttribs=[] inlineItems=false htmlWrap="ul" extraArgs...>
  <#if !inlineItems>
    <${htmlWrap!}<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=excludeAttribs/></#if>>
  </#if>
  <#nested>
  <#if !inlineItems>
    </${htmlWrap!}>
  </#if>
</#macro>

<#-- 
*************
* Menu Item
************
Menu item macro. Must ALWAYS be enclosed in a @menu macro (see @menu options if need to generate items only).
             
   * General Attributes *
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
  <#local args = toSimpleMap(args)> <#-- DEV NOTE: make sure always do this from now on here... -->
  <#local type = inlineArgs.type!args.type!"generic">
  <#if inlineArgs.class??>
    <#local class = combineClassArgs(args.class!true, inlineArgs.class)>
  <#else>
    <#local class = args.class!true>
  </#if>
  <#local id = inlineArgs.id!args.id!"">
  <#local style = inlineArgs.style!args.style!"">
  <#local attribs = inlineArgs.attribs!args.attribs!"">
  <#if inlineArgs.contentClass??>
    <#local contentClass = combineClassArgs(args.contentClass!true, inlineArgs.contentClass)>
  <#else>
    <#local contentClass = args.contentClass!true>
  </#if>
  <#local contentId = inlineArgs.contentId!args.contentId!"">
  <#local contentStyle = inlineArgs.contentStyle!args.contentStyle!"">
  <#local contentAttribs = inlineArgs.contentAttribs!args.contentAttribs!"">
  <#local text = inlineArgs.text!args.text!"">
  <#local href = inlineArgs.href!args.href!true>
  <#local fullPath = inlineArgs.fullPath!args.fullPath!false>
  <#local secure = inlineArgs.secure!args.secure!false>
  <#local encode = inlineArgs.encode!args.encode!true>
  <#local onClick = inlineArgs.onClick!args.onClick!"">
  <#local disabled = inlineArgs.disabled!args.disabled!false>
  <#local selected = inlineArgs.selected!args.selected!false>
  <#local active = inlineArgs.active!args.active!false>
  <#local target = inlineArgs.target!args.target!"">
  <#local nestedContent = inlineArgs.nestedContent!args.nestedContent!true>
  <#local nestedMenu = inlineArgs.nestedMenu!args.nestedMenu!false>
  <#local wrapNested = inlineArgs.wrapNested!args.wrapNested!false>
  <#local nestedFirst = inlineArgs.nestedFirst!args.nestedFirst!false>
  <#local htmlWrap  = inlineArgs.htmlWrap!args.htmlWrap!"li"/>
  <#local inlineItem = inlineArgs.inlineItem!args.inlineItem!false>
  <#t>
  <#local menuType = (catoCurrentMenuInfo.type)!"">
  <#local menuStyleName = (catoCurrentMenuInfo.styleName)!"">
  <#t>
  <#local classes = makeClassesArg(class, styles["menu_" + menuStyleName + "_item"]!styles["menu_default_item"]!"")>
  <#t>
  <#if type == "link">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_link"]!styles["menu_default_item_link"]!"">
  <#elseif type == "text">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_text"]!styles["menu_default_item_text"]!"">
  <#elseif type == "submit">
    <#local defaultContentClass = styles["menu_" + menuStyleName + "_item_submit"]!styles["menu_default_item_submit"]!"">
  <#else>
    <#local defaultContentClass = "">
  </#if>
  <#local contentClasses = makeClassesArg(contentClass, defaultContentClass)>
  <#t>
  <#if disabled>
    <#local classes = (classes + " " + (styles["menu_" + menuStyleName + "_itemdisabled"]!styles["menu_default_itemdisabled"]!""))?trim>
    <#local contentClasses = (contentClasses + " " + (styles["menu_" + menuStyleName + "_item_contentdisabled"]!styles["menu_default_item_contentdisabled"]!""))?trim>
    <#local href = "javascript:void(0);">
  </#if>
  <#if selected>
    <#local classes = (classes + " " + (styles["menu_" + menuStyleName + "_itemselected"]!styles["menu_default_itemselected"]!""))?trim>
    <#local contentClasses = (contentClasses + " " + (styles["menu_" + menuStyleName + "_item_contentselected"]!styles["menu_default_item_contentselected"]!""))?trim>
  </#if>
  <#if active>
    <#local classes = (classes + " " + (styles["menu_" + menuStyleName + "_itemactive"]!styles["menu_default_itemactive"]!""))?trim>
    <#local contentClasses = (contentClasses + " " + (styles["menu_" + menuStyleName + "_item_contentactive"]!styles["menu_default_item_contentactive"]!""))?trim>
  </#if>
  <@menuitem_markup classes=classes id=id style=style attribs=attribs excludeAttribs=["class", "id", "style"] inlineItem=inlineItem htmlWrap=htmlWrap><#rt>
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
      <#t><@menuitem_link_markup href=href onclick=onClick classes=contentClasses id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","href","onclick","target","title"] target=target title=title><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_link_markup>
    <#elseif type == "text">
      <#t><@menuitem_text_markup classes=contentClasses id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","onclick"] onClick=onClick><#if wrapNested && nestedFirst>${nestedContent}</#if><#if text?has_content>${text}</#if><#if wrapNested && !nestedFirst>${nestedContent}</#if></@menuitem_text_markup>
    <#elseif type == "submit">
      <#t><#if wrapNested && nestedFirst>${nestedContent}</#if><@menuitem_submit_markup classes=contentClasses id=contentId style=contentStyle attribs=contentAttribs excludeAttribs=["class","id","style","value","onclick","disabled","type"] onClick=onClick disabled=disabled><#if text?has_content>${text}</#if></@menuitem_submit_markup><#if wrapNested && !nestedFirst> ${nestedContent}</#if>
    <#else>
      <#t><#if text?has_content>${text}</#if><#if wrapNested>${nestedContent}</#if>
    </#if>
    <#t><#if !wrapNested && !nestedFirst>${nestedContent}</#if>
  </@menuitem_markup><#lt>
  <#global catoCurrentMenuItemIndex = catoCurrentMenuItemIndex + 1>
</#macro>

<#-- Markup for @menuitem (outer item wrapper only) - may be overridden -->
<#macro menuitem_markup classes="" id="" style="" attribs={} excludeAttribs=[] inlineItem=false htmlWrap="li" extraArgs...>
  <#if !inlineItem>
    <${htmlWrap!}<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if>><#rt>
  </#if>
  <#nested><#t>
  <#if !inlineItem></${htmlWrap!}></#if><#lt>
</#macro>

<#-- Markup for @menuitem type="link" - may be overridden -->
<#macro menuitem_link_markup classes="" id="" style="" href="" onClick="" target="" title="" attribs={} excludeAttribs=[] extraArgs...>
  <#t><a href="${href}"<#if onClick?has_content> onclick="${onClick}"</#if><#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if target?has_content> target="${target}"</#if><#if title?has_content> title="${title}"</#if>><#nested></a>
</#macro>

<#-- Markup for @menuitem type="text" - may be overridden -->
<#macro menuitem_text_markup classes="" id="" style="" onClick="" attribs={} excludeAttribs=[] extraArgs...>
  <#t><span<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${onClick}"</#if>><#nested></span>
</#macro>

<#-- Markup for @menuitem type="submit" - may be overridden -->
<#macro menuitem_submit_markup classes="" id="" style="" text="" onClick="" disabled=false attribs={} excludeAttribs=[] extraArgs...>
  <#t><button type="submit"<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if><#if style?has_content> style="${style}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if onClick?has_content> onclick="${onClick}"</#if><#if disabled> disabled="disabled"</#if> /><#nested></button>
</#macro>

<#-- 
*************
* Pagination Macro
************
    Usage example:  
    <@paginate mode="single" ... />
    <@paginate mode="content">
      <@table>
        ...
      </@table>
    </@paginate>            
                    
   * General Attributes *
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
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
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
<#macro paginate mode="single" type="default" layout="default" noResultsMode="default" paginateOn=true url="" class=true viewIndex=0 listSize=0 viewSize=1 altParam=false 
    forcePost=false paramStr="" viewIndexFirst=0 showCount=true countMsg=""
    paginateToggle=false paginateToggleString="" paginateToggleOnValue="Y" paginateToggleOffValue="N">
    <#local classes = makeClassesArg(class, "nav-pager")>
    
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
  <#-- DEV NOTE: make sure all @paginate_impl calls same (DO NOT use #local capture; risks duplicate IDs) -->
  <#if mode == "single">
      <#if showNextPrev>
        <@paginate_impl ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateStyle=classes paginateFirstStyle="${styles.pagination_item_first!}" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextStyle="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnStyle="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffStyle="" paginateOffLabel=uiLabelMap.CommonPagingOff />
      </#if>
  <#else>
      <#if showNextPrev && layout != "bottom">
        <@paginate_impl ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateStyle=classes paginateFirstStyle="${styles.pagination_item_first!}" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextStyle="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnStyle="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffStyle="" paginateOffLabel=uiLabelMap.CommonPagingOff />
      </#if>
        <#nested>
      <#if showNextPrev && layout != "top">
        <@paginate_impl ajaxEnabled=false javaScriptEnabled=(javaScriptEnabled!true) paginateStyle=classes paginateFirstStyle="${styles.pagination_item_first!}" viewIndex=viewIndex highIndex=highIndex listSize=listSize viewSize=viewSize ajaxFirstUrl="" firstUrl=firstUrl paginateFirstLabel=uiLabelMap.CommonFirst paginatePreviousStyle="${styles.pagination_item_previous!}" ajaxPreviousUrl="" previousUrl=previousUrl paginatePreviousLabel=uiLabelMap.CommonPrevious pageLabel="" ajaxSelectUrl="" selectUrl=selectUrl ajaxSelectSizeUrl="" selectSizeUrl=selectSizeUrl commonDisplaying=showCount?string(countMsg,"") paginateNextStyle="${styles.pagination_item_next!}" ajaxNextUrl="" nextUrl=nextUrl paginateNextLabel=uiLabelMap.CommonNext paginateLastStyle="${styles.pagination_item_last!}" ajaxLastUrl="" lastUrl=lastUrl paginateLastLabel=uiLabelMap.CommonLast paginateViewSizeLabel="" forcePost=forcePost viewIndexFirst=viewIndexFirst paginate=paginateOn paginateToggle=paginateToggle ajaxPaginateOnUrl="" paginateOnUrl=paginateOnUrl paginateOnStyle="" paginateOnLabel=uiLabelMap.CommonPagingOn ajaxPaginateOffUrl="" paginateOffUrl=paginateOffUrl paginateOffStyle="" paginateOffLabel=uiLabelMap.CommonPagingOff />
      </#if>
  </#if>
</#macro>

<#function escapeUrlParamDelims url>
    <#if url?contains('&amp;')>
        <#return url>
    <#else>
        <#return url?replace('&', '&amp;')>
    </#if>
</#function>

<#-- DEV NOTE: see @section_impl for details on pattern
     migrated from @renderNextPrev form widget macro
     new params: paginate, forcePost, viewIndexFirst, listItemsOnly, paginateToggle*
     paginate is a display hint, does not seem to mean guarantee data wasn't paginated -->
<#macro paginate_impl paginateStyle paginateFirstStyle viewIndex highIndex listSize viewSize ajaxEnabled javaScriptEnabled ajaxFirstUrl firstUrl paginateFirstLabel paginatePreviousStyle ajaxPreviousUrl previousUrl paginatePreviousLabel pageLabel ajaxSelectUrl selectUrl ajaxSelectSizeUrl selectSizeUrl commonDisplaying paginateNextStyle ajaxNextUrl nextUrl paginateNextLabel paginateLastStyle ajaxLastUrl lastUrl paginateLastLabel paginateViewSizeLabel paginate=true forcePost=false viewIndexFirst=0 listItemsOnly=false paginateToggle=false ajaxPaginateOnUrl="" paginateOnUrl="" paginateOnStyle="" paginateOnLabel="" ajaxPaginateOffUrl="" paginateOffUrl="" paginateOffStyle="" paginateOffLabel="">
<#-- note: possible that data was paginated even if paginate false, but don't bother right now -->
<#if paginate>
  <#local availPageSizes = [10, 20, 30, 50, 100, 200]>
  <#local minPageSize = availPageSizes?first>
  <#local itemRange = 2/>
  <#local placeHolder ="..."/>
  
  <#-- these errors apparently happen a lot, enforce here cause screens never catch, guarantee other checks work -->
  <#if (!viewSize?is_number)>
      ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewSize was not a number type: " + viewSize!, "htmlFormMacroLibraryRenderNextPrev")!}<#t>
      <#local viewSize = viewSize?number>
  </#if>
  <#local viewSize = viewSize?floor>
  <#if (!viewIndex?is_number)>
      ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was not a number type: " + viewIndex!, "htmlFormMacroLibraryRenderNextPrev")!}<#t>
      <#local viewIndex = viewIndex?number>
  </#if>
  <#local viewIndex = viewIndex?floor>
  
  <#local viewIndexLast = viewIndexFirst + ((listSize/viewSize)?ceiling-1)>
  <#if (viewIndexLast < viewIndexFirst)>
    <#local viewIndexLast = viewIndexFirst>
  </#if>
  <#if (viewIndex < viewIndexFirst) || (viewIndex > viewIndexLast)>
      ${Static["org.ofbiz.base.util.Debug"].logError("pagination: viewIndex was out of bounds: " + viewIndex, "htmlFormMacroLibraryRenderNextPrev")!}<#t>
      <#if (viewIndex < viewIndexFirst)>
          <#local viewIndex = viewIndexFirst>
      <#else>
          <#local viewIndex = viewIndexLast>
      </#if>
  </#if>
  
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
  
  <#-- note: must use submitPaginationPost to force send as POST for some requests, because Ofbiz security feature prevents
       GET params passed to controller service event when request is https="true".
       note: submitPagination (new in stock Ofbiz 14) already sends as POST in some cases, but not based on controller.
       FIXME: POST/forcePost currently only supported when js enabled (non-js need extra markup for a form, ugly),
              currently non-js falls back to GET only, won't always work -->
  
  <#-- note: implies (listSize > 0), some cases this gets called with listSize zero -->
  <#if (listSize > minPageSize)>
    <#local multiPage = (listSize > viewSize)>
  
    <#-- DEV NOTE: duplicated below -->
    <#if !listItemsOnly>
    <div class="${styles.grid_row!}">
      <div class="${styles.grid_large!}2 ${styles.grid_cell!}">${commonDisplaying}</div>
      <div class="${styles.grid_large!}8 ${styles.grid_cell!}">
        <div class="${styles.pagination_wrap!} ${paginateStyle}">
          <ul class="${styles.pagination_list!}">
        </#if>

            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxFirstUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${firstUrl}')"</#if><#else>href="${firstUrl}"</#if></#local>
            <li class="${styles.pagination_item!} ${paginateFirstStyle}<#if (viewIndex> viewIndexFirst)>"><a ${actionStr}>${paginateFirstLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginateFirstLabel}</span></#if></li>
            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPreviousUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${previousUrl}')"</#if><#else>href="${previousUrl}"</#if></#local>
            <li class="${styles.pagination_item!} ${paginatePreviousStyle}<#if (viewIndex> viewIndexFirst)>"><a ${actionStr}>${paginatePreviousLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginatePreviousLabel}</span></#if></li>
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
              <#if displayDots>${placeHolder!}</#if>
              <#local displayDots = false/>
              </#if>
            </#list>
        </#if>
        
            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxNextUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${nextUrl}')"</#if><#else>href="${nextUrl}"</#if></#local>
            <li class="${styles.pagination_item!} ${paginateNextStyle}<#if (highIndex < listSize)>"><a ${actionStr}>${paginateNextLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginateNextLabel}</span></#if></li>
            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxLastUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${lastUrl}')"</#if><#else>href="${lastUrl}"</#if></#local>
            <li class="${styles.pagination_item!} ${paginateLastStyle}<#if (highIndex < listSize)>"><a ${actionStr}>${paginateLastLabel}</a><#else> ${styles.pagination_item_disabled!}"><span>${paginateLastLabel}</span></#if></li>
        </ul>           

    <#if !listItemsOnly>  
          </ul>
        </div>
      </div>
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
               
            </#if>
    
            <#if paginateToggle>
                <#local ajaxPaginateOffUrl = escapeUrlParamDelims(ajaxPaginateOffUrl)>
                <#local paginateOffUrl = escapeUrlParamDelims(paginateOffUrl)>
                <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPaginateOffUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${paginateOffUrl}')"</#if><#else>href="${paginateOffUrl}"</#if></#local>
                <span<#if paginateOffStyle?has_content> class="${styles.pagination_item!} ${paginateOffStyle}"</#if>><a ${actionStr}>${paginateOffLabel}</a></span>           
            </#if>
        </div>
    </div>
    </#if>
  </#if>
<#elseif paginateToggle>

  <#local ajaxPaginateOnUrl = escapeUrlParamDelims(ajaxPaginateOnUrl)>
  <#local paginateOnUrl = escapeUrlParamDelims(paginateOnUrl)>

    <#if !listItemsOnly>
    <div class="${styles.grid_row!}">
      <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
        <div class="${styles.pagination_wrap!} ${paginateStyle}">
          <ul class="${styles.pagination_list!}">
    </#if>

            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPaginateOnUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${paginateOnUrl}')"</#if><#else>href="${paginateOnUrl}"</#if></#local>
            <li<#if paginateOnStyle?has_content> class="${paginateOnStyle}"</#if>><a ${actionStr}>${paginateOnLabel}</a></li>  

    <#if !listItemsOnly>  

        </div>
      </div>
    </div>
    </#if>
</#if>
</#macro>

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

<#-- 
*************
* Progress Script Macro
************
Generates script data and markup needed to make an instance to initialize upload progress 
javascript anim for a form, with progress bar and/or text.

Server-side upload event for the form must register a FileUploadProgressListener in session
for getFileUploadProgressStatus AJAX calls.

TODO: document better if needed
                    
   * General Attributes *
    options = elem IDs and options passed to CatoUploadProgress javascript class
              in addition, supports: 
                submitHook - one of: "formSubmit" (default), "validate" (jquery validate), "none" (caller does manually) 
                validateObjScript - if submitHook is "validate", add this script text to jquery validate({...}) object body.
-->
<#macro progressScript options={} htmlwrap=false>
  <#if options?has_content && options.formSel?has_content>
    <#if htmlwrap>
    <script type="text/javascript">
    //<![CDATA[
    </#if>
    
    <#-- This belongs here, but due to Ofbiz bug, moved to commonScripts.ftl
    <@requireScriptOfbizUrl uri="getFileUploadProgressStatus" />
    -->
    
    (function() {
        var uploadProgress = null;
    
        jQuery(document).ready(function() {
          <#if options.successRedirectUrl??>
            <#-- shouldn't have &amp; in script tag... but code may escape and should support... -->
            <#local options = concatMaps(options, {"successRedirectUrl":options.successRedirectUrl?replace("&amp;", "&")})>
          </#if>
            uploadProgress = new CatoUploadProgress(<@objectAsScript lang="js" object=options />);
            uploadProgress.reset();
        });
        
      <#if (options.submitHook!) == "validate">
        jQuery("${options.formSel}").validate({
            submitHandler: function(form) {
                var goodToGo = uploadProgress.initUpload();
                if (goodToGo) {
                    form.submit();
                }
            },
            ${options.validateObjScript!""}
        });
      <#elseif (options.submitHook!) != "none" >
        jQuery("${options.formSel}").submit(function(event) {
            var goodToGo = uploadProgress.initUpload();
            if (!goodToGo) {
                event.preventDefault();
            }
        });
      </#if>
    })();
    
    <#if htmlwrap>
    //]]>
    </script>
    </#if>
  </#if>
</#macro>

<#-- 
*************
* Progress Bar Macro
************

    Usage example:  
    <@progress value=40/>             
    
   Can be animated using js, example: 
   
   $('#${id}_meter').css("width", "78%");
    
   Can also be animated automatically using progressOptions which activates use of CatoUploadProgress
   script for this progress bar by linking it to a form submit.
                    
   * General Attributes *
    value          = Percentage done
    id             = custom id; can also be specified as progressOptions.progBarId instead
                     if omitted will not make a progress bar, but script still generated for progressOptions.progTextBoxId
    type           = (warning|info|success) default: success
    class          = Adds classes - please use "(small|medium|large)-block-grid-#"
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    showValue      = Display value inside bar
    wrapClass      = classes on outer wrapper only
    progressOptions = if present, attaches progress bar to an upload form with javascript-based progress and 
                      attaches results to page using elem IDs and options in this map - 
                      see CatoUploadProgress javascript class for options; mostly same
-->
<#macro progress value=0 id="" type="" class=true showValue=false wrapperClass=true progressOptions={}>
  <#local explicitId = id?has_content>
  <#if !id?has_content>
    <#local id = (progressOptions.progBarId)!"">
  </#if>

    <#switch type>
      <#case "alert">
        <#local color=styles.color_alert!/>
      <#break>
      <#case "info">
        <#local color=styles.color_info!/>
      <#break>
      <#case "warning">
        <#local color=styles.color_warning!/>
      <#break>
      <#default>
        <#local color=styles.color_success!/>
    </#switch>
    <#local classes = makeClassesArg(class, "")>
    <#local wrapperClasses = makeClassesArg(wrapperClass, "")>
    <div class="${styles.progress_container}<#if !styles.progress_wrap?has_content && classes?has_content> ${classes}</#if><#if color?has_content> ${color!}</#if><#if wrapperClasses?has_content> ${wrapperClasses}</#if>"<#if id?has_content> id="${id}"</#if>>
      <#if styles.progress_wrap?has_content><div class="${styles.progress_wrap!}<#if classes?has_content> ${classes}</#if>"<#if id?has_content> id="${id!}_meter"</#if> role="progressbar" aria-valuenow="${value!}" aria-valuemin="0" aria-valuemax="100" style="width: ${value!}%"></#if>
            <span class="${styles.progress_bar!}"<#if !styles.progress_wrap?has_content> style="width: ${value!}%"<#if id?has_content> id="${id!}_meter"</#if></#if>><#if showValue>${value!}</#if></span>
      <#if styles.progress_wrap?has_content></div></#if>
    </div>
    
  <#if progressOptions?has_content>
    <#local opts = progressOptions>
    <#if explicitId>
      <#local opts = concatMaps(opts, {"progBarId":"${id}"})>
    </#if>
    <@progressScript options=opts htmlwrap=true />
  </#if>
</#macro>

<#-- 
*************
* Form Macro
************
HTML form.

    Usage example:  
    <@form name="myform">
      <@fields>
        <input type="hidden" ... />
        <@field ... />
        <@field ... />
      </@fields>
    </@form>            
                    
   * General Attributes *
    type                = [input|display], default input
                          DEV NOTE: "display" is special for time being, probably rare or unused;
                                    maybe it should cause to omit <form> element
    class               = classes on form element itself 
    attribs             = hash of attributes for HTML <form> element (needed for names with dashes)
    inlineAttribs       = other attributes for HTML <form> element
-->
<#macro form type="input" name="" id="" class=true attribs={} inlineAttribs...>
    <#local formInfo = {"type":type, "name":name, "id":id}>
    <#local dummy = pushRequestStack("catoCurrentFormInfo", formInfo)>
    <#local classes = makeClassesArg(class, "")>
    <form<#if classes?has_content> class="${classes}</#if><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if attribs?has_content><@elemAttribStr attribs=attribs exclude=["class", "name", "id"]/></#if><#if inlineAttribs?has_content><@elemAttribStr attribs=inlineAttribs /></#if>>
      <#nested>
    </form>
    <#local dummy = popRequestStack("catoCurrentFormInfo")>
</#macro>

<#-- 
*************
* Fieldset Macro
************
A visible fieldset, including the HTML element.

    Usage example:  
    <@fieldset title="">
        Inner Content
    </@fieldset>            
                    
   * General Attributes *
    class           = css classes 
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    id              = set id
    title           = fieldset-title
    collapsed       = show/hide the fieldset
-->
<#macro fieldset id="" title="" class=true collapsed=false>
    <#local classes = makeClassesArg(class, "")>
    <@fieldset_impl style=classes id=id title=title collapsed=collapsed collapsibleAreaId="" collapsible=false expandToolTip="" collapseToolTip="">
        <#nested />
    </@fieldset_impl>
</#macro>

<#-- DEV NOTE: see @section_impl for details on pattern 
     migrated from @renderFieldGroupOpen/Close form widget macro -->
<#macro fieldset_impl style="" id="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false openOnly=false closeOnly=false wrapIf=true>
<#local open = wrapIf && !closeOnly>
<#local close = wrapIf && !openOnly>
<#if open>
<div class="${styles.grid_row!}">
  <div class="fieldgroup ${styles.grid_large!}12 ${styles.grid_cell!}<#if style?has_content> ${style}</#if><#if collapsible || collapsed> toggleField<#if collapsed> ${styles.collapsed!}</#if></#if>"<#if id?has_content> id="${id}"</#if>>
    <fieldset<#if style?has_content> class="${style!}"</#if>>
      <#--<#if collapsible>
        <ul>
          <li class="<#if collapsed>${styles.collapsed!}">
                      <a onclick="javascript:toggleCollapsiblePanel(this, '${collapsibleAreaId}', '${expandToolTip}', '${collapseToolTip}');">
                    <#else>expanded">
                      <a onclick="javascript:toggleCollapsiblePanel(this, '${collapsibleAreaId}', '${expandToolTip}', '${collapseToolTip}');">
                    </#if>
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<#if title?has_content>${title}</#if></a>
          </li>
        </ul>
      <#else>
        <#if title?has_content>${title}</#if>
      </#if><#rt/>
    </div>
    <div id="${collapsibleAreaId}" class="fieldgroup-body" <#if collapsed && collapsible> style="display: none;"</#if>>
    -->
    <#if title?has_content><legend><#if collapsible || collapsed>[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] </#if>${title}</legend></#if>
</#if>
    <#nested>
<#if close>
    </fieldset>
    </div>
</div>
</#if>
</#macro>

<#-- 
*************
* Fields Macro
************ 
Fields helper that helps modify a set of @field definitions, or group of fields.
Not associated with a visible element, as is @fieldset.
Can be omitted.
May sometimes need multiple of these per form (so @form insufficient for this purpose),
or even multiple per fieldset. 

    Usage example: 
    <@fields>
      <@field attr="" />
      <@field attr="" />
    </@field>
     
    <@fields type="default-nolabels">
      <@field attr="" />
      <@field attr="" />
    </@field>
    
    <@fields type="generic" labelArea=false>
      <@field attr="" />
      <@field attr="" />
    </@field>
    
    * General Attributes *
    type            = [default|default-nolabels|generic], default default. the type of fields arrangement.
                      default: default cato field arrangement. this is the type assumed when no @fields element is present.
                          currently, it mostly influences the label area (present for all @field types except submit).
                      default-nolabels: default cato field arrangement for common sets of fields with no labels.
                          it expects that @field entries won't be passed any labels.
                      generic: generic field arrangement of no specific pattern. means field arrangement is custom and field macro should not
                          make any assumptions except where a default is required. caller determines arrangement/layout/label type/etc.
    labelType       = [gridarea|none], defaults specified in styles variables based on fields type. override for type of the field labels themselves.
                      gridarea: a grid area reserved for label.
                      none: no labels or label areas. expects the @field macro won't be passed any.
    labelLayout     = [left|right|none], defaults specified in styles variables based on fields type. override for layout/positioning of the labels.
    labelArea       = boolean, defaults specified in styles variables based on fields type. overrides whether fields are expected to have a label area or not, mainly when label omitted. 
                      logic is influenced by other arguments.
                      note that this is weaker than labelArea arg of @field macro, but stronger than other args of this macro.
    labelAreaExceptions = string of space-delimited @field type names or list of names, defaults specified in styles variables based on fields type  
    formName            = the form name the child fields should assume  
    formId              = the form ID the child fields should assume             
-->
<#macro fields type="default" labelType="" labelLayout="" labelArea="" labelAreaExceptions=true formName="" formId="">
    <#local fieldsInfo = makeFieldsInfo(type, labelType, labelLayout, labelArea, labelAreaExceptions, formName, formId)>
    <#local dummy = pushRequestStack("catoCurrentFieldsInfo", fieldsInfo)>
    <#nested>
    <#local dummy = popRequestStack("catoCurrentFieldsInfo")>
</#macro>

<#function makeFieldsInfo type labelType="" labelLayout="" labelArea="" labelAreaExceptions=true formName="" formId="">
    <#local stylesType = type?replace("-","_")>
    <#local stylesPrefix = "fields_" + stylesType + "_">
    <#if !styles[stylesPrefix + "labeltype"]??>
      <#local stylesType = "default">
      <#local stylesPrefix = "fields_default_">
    </#if>

    <#if !labelArea?is_boolean>
      <#local stylesLabelArea = styles[stylesPrefix + "labelarea"]!"">
      <#if stylesLabelArea?is_boolean>
        <#local labelArea = stylesLabelArea>
      </#if>
    </#if>
    <#if !labelType?has_content>
      <#local labelType = styles[stylesPrefix + "labeltype"]!"gridarea">
    </#if>
    <#if !labelLayout?has_content>
      <#local labelLayout = styles[stylesPrefix + "labellayout"]!"left">
    </#if>
    <#if !labelArea?is_boolean>
      <#local labelArea = (labelType != "none" && labelLayout != "none")>
    </#if>

    <#if !labelAreaExceptions?is_sequence && !labelAreaExceptions?is_string>
      <#if labelAreaExceptions?is_boolean && labelAreaExceptions == false>
        <#local labelAreaExceptions = []>
      <#else>
        <#local labelAreaExceptions = styles[stylesPrefix + "labelareaexceptions"]!"">
      </#if>
    </#if>
    <#if labelAreaExceptions?is_string>
      <#if labelAreaExceptions?has_content>
        <#local labelAreaExceptions = labelAreaExceptions?split(" ")>
      <#else>
        <#local labelAreaExceptions = []>
      </#if>
    </#if>
    <#return {"type":type, "labelType":labelType, "labelLayout":labelLayout, 
        "labelArea":labelArea, "labelAreaExceptions":labelAreaExceptions, "formName":formName, "formId":formId}>
</#function>

<#-- 
*************
* Field Macro
************ 
    Usage example:  
    <@field attr="" />
    
    * General Attributes *
    type            = form element of type [input,textarea,datetime,select,checkbox,radio,display,password,generic],
                      default generic meaning input defined manually with #nested
                      (discouraged; prefer specific; but sometimes required and useful
                      for transition)
    label           = field label
                      note: label area behavior may also be influenced by containing macros such as @fields
    labelDetail     = extra content (HTML) inserted with (after) label
    labelType       = explicit label type (see @fields)
    labelLayout     = explicit label layout (see @fields)
    labelArea       = boolean, default empty string (use @fields type default).
                      if true, forces a label area.
                      if false, prevents a label area.
    tooltip         = Small field description - to be displayed to the customer
    description     = alternative to tooltip
    name            = field name
    value           = field value
    columns         = int value for columns for field (overrides classes)
    class           = css classes for the field element (NOT the cell container!)
                      (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
    maxlength       = maxLength
    id              = field id
    onClick         = JS Event
    disabled        = field disabled
    placeholder     = field placeholder
    alert           = adds additional css alert class
    mask            = toggles jQuery mask plugin
    size            = size attribute (default: 20)
    collapse        = should the field be collapsing? (default: false)
    norows          = render without the rows-container
    nocells         = render without the cells-container
    required        = required input
        
    * input *
    autoCompleteUrl = if autocomplete function exists, specification of url will make it available
    postfix          = if set to true, attach submit button (default:false)
    
    * textArea *
    readonly        = readonly
    rows            = number of rows
    cols            = number of columns
    
    * dateTime *
    dateType        = type of datetime [date,time] (default: date)
    
    * select *
    multiple        = allow multiple select true/false
    currentValue    = currently selected value
    
    * lookup *
    formName        = The name of the form that contains the lookup field.
    fieldForName    = Contains the lookup window form name.
    
    * Checkbox *
    value           = Y/N
    checked         = checked (true/false)
    
    * radio (single mode) *
    value           = Y/N, only used if single radio item mode (items not specified)
    checked         = determines if checked (true/false)
    
    * radio (multi mode) *
    items           = if specified, multiple-items radio generated; list of {"key": value, "description": label, "event": html-dom-event-attrib, "action": event-js} hashes
    currentValue    = current value, determines checked
    defaultValue    = default value, determines checked
    
    * file *
    autocomplete    = true/false, default true (false to prevent)
    
    * password *
    autocomplete    = true/false, default true (false to prevent)
    
    * submitarea *
    <#nested>       = button(s) (<@field type="submit"> or manual <input, <a, <button) to include
    progressOptions = if this is an upload form, specify progress upload options, enables progress next to buttons. 
                      see @progress[Script] macro[s]. should specify formSel, (progBarId and/or progTextBoxId), and others.
                      
    * submit *
    submitType      = [submit|link|button|image], default submit (<input type="submit">)  
    text            = display text (also value for submitType=="submit")                
    href            = href for submitType=="link"  
    src             = image url for submitType=="image"    
    confirmMsg      = confirmation message            
                      
    * display *
    valueType       = [image|text|currency|date|date-time|accounting-number|generic], default generic (treated as text)
                      TODO: currently all are handled as text/generic (because formatting done in java in stock ofbiz)
    value           = display value or image URL
    description     = for image type: image alt
-->
<#macro field type="" label="" labelDetail="" name="" value="" valueType="" currentValue="" defaultValue="" class=true size=20 maxlength="" id="" onClick="" 
        disabled=false placeholder="" autoCompleteUrl="" mask=false alert="false" readonly=false rows="4" 
        cols="50" dateType="date" multiple="" checked=false collapse=false tooltip="" columns="" norows=false nocells=false nocontainer=""
        fieldFormName="" formName="" formId="" postfix=false postfixSize=1 required=false items=[] autocomplete=true progressOptions={} 
        labelType="" labelLayout="" labelArea="" description=""
        submitType="input" text="" href="" src="" confirmMsg="">
<#if !type?has_content>
  <#local type = "generic">
</#if>
<#if !valueType?has_content>
  <#local valueType = "generic">
</#if>
<#-- treat these as synonyms for now -->
<#if tooltip?has_content>
  <#if !description?has_content>
    <#local description = tooltip>
  </#if>
<#else>
  <#if description?has_content>
    <#local tooltip = description>
  </#if>
</#if>

<#-- parent @fields group elem info (if any; may be omitted) -->
<#local fieldsInfo = readRequestStack("catoCurrentFieldsInfo")!{}>
<#if !fieldsInfo.type??>
  <#if !catoDefaultFieldsInfo?has_content>
    <#-- optimization -->
    <#global catoDefaultFieldsInfo = makeFieldsInfo("default")>
  </#if>
  <#local fieldsInfo = catoDefaultFieldsInfo>
</#if>

<#-- parent @field elem info (if any; is possible) -->
<#local parentFieldInfo = readRequestStack("catoCurrentFieldInfo")!{}>
<#local hasParentField = ((parentFieldInfo.type)!"")?has_content>
<#local isTopLevelField = !hasParentField>
<#local isChildField = hasParentField>

<#local formInfo = readRequestStack("catoCurrentFormInfo")!{}>

<#-- this field's info (popped at end) -->
<#local dummy = pushRequestStack("catoCurrentFieldInfo", 
    {"type":type})>

<#-- get form name and id -->
<#if !formName?has_content>
  <#if fieldsInfo.formName?has_content>
    <#local formName = fieldsInfo.formName>
  <#elseif formInfo.name?has_content>
    <#local formName = formInfo.formName>
  </#if>
</#if>
<#if !formId?has_content>
  <#if fieldsInfo.formId?has_content>
    <#local formId = fieldsInfo.formName>
  <#elseif formInfo.id?has_content>
    <#local formId = formInfo.id>
  </#if>
</#if>

<#-- fieldIdNum will always increment throughout the page 
     now stored in request attributes so survived screens.render though still accessible as a global -->
<#local fieldIdNum = getRequestVar("catoFieldIdNum")!0>
<#local fieldIdNum = fieldIdNum + 1 />
<#local dummy = setRequestVar("catoFieldIdNum", fieldIdNum)>

<#local radioSingle = (type=="radio" && !items?has_content)>

<#if !id?has_content>
    <#-- FIXME? renderSeqNumber usually empty... where come from? should be as request attribute also? -->
    <#local id = "field_id_${renderSeqNumber!}_${fieldIdNum!0}">
</#if>

<#-- NOTE: here, "classes" is for @cell container; "class" is for input elem (not same as other macros)! 
     can't allow specify @cell container classes as-is because can't calculate from it, would need columns param as int -->
<#local classes = "${styles.grid_large!}12"/>
<#local columnspostfix = 0/>
<#if postfix>
    <#local columnspostfix = postfixSize/>
    <#local collapse = true/>
    <#local classes = "${styles.grid_small!}${12-columnspostfix} ${styles.grid_large!}${12-columnspostfix}"/>
</#if>

<#local class = makeClassesArg(class, "")>
<#if required && (!containsStyleName(class, "required"))>
    <#local class = (class + " required")?trim>
</#if>

<#if !catoFieldNoContainerChildren??>
  <#global catoFieldNoContainerChildren = {
   <#-- "submit":true --> <#-- only if parent is submitarea -->
  }>
  <#global catoFieldNoContainerParent = {
    "submitarea":true
  }>
</#if>
<#if !nocontainer?is_boolean>
  <#if nocontainer?has_content>
    <#local nocontainer = nocontainer?boolean>
  <#elseif isChildField && (catoFieldNoContainerChildren[type]?? || catoFieldNoContainerParent[parentFieldInfo.type!]??)>
    <#local nocontainer = true>
  <#else> 
    <#local nocontainer = false>
  </#if>
</#if>

<#local fieldEntryTypeClass = "field-entry-type-" + mapCatoFieldTypeToStyleName(type)>
<@row collapse=collapse!false norows=(norows || nocontainer) class=("+form-field-entry "+fieldEntryTypeClass)>

    <#-- TODO: right now most of the fieldsInfo parameters are not fully exploited.
         assumes labelType=="gridarea" (unless "none" which influences labelArea) and 
         labelLayout="left" (unless "none" which influences labelArea). -->
    <#if labelArea?is_boolean>
      <#local labelAreaDefault = labelArea>
    <#elseif labelType == "none" || labelLayout == "none">
      <#local labelAreaDefault = false>
    <#elseif isChildField>
      <#-- based on current usage, a child field should never really have a label area by default (requires explicit)... -->
      <#local labelAreaDefault = false>
    <#else>
      <#local labelAreaDefault = (fieldsInfo.labelArea)!false>
      <#if (fieldsInfo.labelAreaExceptions)?has_content>
        <#if fieldsInfo.labelAreaExceptions?seq_contains(type)>
          <#local labelAreaDefault = !labelAreaDefault>
        </#if>
      </#if>
    </#if>

    <#if labelType?has_content>
      <#local effLabelType = labelType>
    <#else>
      <#local effLabelType = (fieldsInfo.labelType)!"">
    </#if>
    <#if labelLayout?has_content>
      <#local effLabelLayout = labelLayout>
    <#else>
      <#local effLabelLayout = (fieldsInfo.labelLayout)!"">
    </#if>

    <#if (labelArea?is_boolean && labelArea == true) || 
         (!(labelArea?is_boolean && labelArea == false) && (label?has_content || labelDetail?has_content || labelAreaDefault))>
        <#local subclasses="${styles.grid_small!}3 ${styles.grid_large!}2"/>
        <#local classes="${styles.grid_small!}${9-columnspostfix} ${styles.grid_large!}${10-columnspostfix}"/>
        
        <#if columns?has_content>
            <#local subclasses="${styles.grid_small!}${12-columns+1} ${styles.grid_large!}${12-columns}"/>
            <#local classes="${styles.grid_small!}${columns-columnspostfix-1} ${styles.grid_large!}${columns-columnspostfix}"/>
        </#if>
        
        <#if !radioSingle>
            <@cell class=(subclasses+" field-entry-title "+fieldEntryTypeClass)?trim nocells=(nocells || nocontainer)>
              <#if label?has_content>
                <#if type=="checkbox" || collapse==false>
                    <label class="form-field-label"<#if id?has_content> for="${id}"</#if>>${label}</label>
                <#else>
                    <span class="${styles.prefix!} form-field-label">${label}</span>
                </#if>  
                <#-- TODO: probably remove asterix later; need for dev -->
                <#if required>*</#if>
              </#if> 
              <#if labelDetail?has_content>
                ${labelDetail}
              </#if>    
            </@cell>
        </#if>
    </#if>
    <@cell class=("${classes!}"+" field-entry-widget "+fieldEntryTypeClass)?trim nocells=(nocells || nocontainer)>
        <#switch type>
          <#case "input">
            <@field_input_widget_impl name=name 
                                  className=class 
                                  alert=alert 
                                  value=value 
                                  textSize=size 
                                  maxlength=maxlength 
                                  id=id 
                                  event="onClick" 
                                  action=onClick 
                                  disabled=disabled
                                  readonly=readonly 
                                  clientAutocomplete="" 
                                  ajaxUrl=autoCompleteUrl 
                                  ajaxEnabled="" 
                                  mask=mask 
                                  placeholder=placeholder 
                                  tooltip=tooltip/>
            <#break>
          <#case "textarea">
            <@field_textarea_widget_impl name=name 
                                  className=class 
                                  alert=alert 
                                  cols=cols 
                                  rows=rows 
                                  id=id 
                                  readonly=readonly 
                                  value=value 
                                  placeholder=placeholder
                                  tooltip=tooltip/>
            <#break>
          <#case "datetime">
            <#if dateType == "date"><#local shortDateInput=true/><#else><#local shortDateInput=false/></#if>
            <@field_datetime_widget_impl name=name 
                                  className=class 
                                  alert=alert 
                                  title=label 
                                  value=value 
                                  size=size 
                                  maxlength=maxlength 
                                  id=id 
                                  dateType=dateType 
                                  shortDateInput=shortDateInput 
                                  timeDropdownParamName="" 
                                  defaultDateTimeString="" 
                                  localizedIconTitle="" 
                                  timeDropdown="" 
                                  timeHourName="" 
                                  classString="" 
                                  hour1="" 
                                  hour2="" 
                                  timeMinutesName="" 
                                  minutes="" 
                                  isTwelveHour="" 
                                  ampmName="" 
                                  amSelected="" 
                                  pmSelected="" 
                                  compositeType="" 
                                  formName=""
                                  tooltip=tooltip/>                
            <#break>
          <#case "select">
            <#-- TODO: Currently, select only supports manual items, not auto-generated items
                 Must set manualItems to true especially, otherwise extra empty options generated -->
            <#local manualItems = true>
            <#local manualItemsOnly = true>
            
            <@field_select_widget_impl name=name
                                    className=class 
                                    alert=alert 
                                    id=id 
                                    multiple=multiple
                                    formName=""
                                    otherFieldName="" 
                                    event="onClick" 
                                    action=onClick  
                                    size=size
                                    firstInList="" 
                                    currentValue=currentValue 
                                    explicitDescription="" 
                                    allowEmpty=""
                                    options=[]
                                    fieldName=name
                                    otherFieldName="" 
                                    otherValue="" 
                                    otherFieldSize=0 
                                    dDFCurrent=""
                                    ajaxEnabled=false
                                    noCurrentSelectedKey=""
                                    ajaxOptions=""
                                    frequency=""
                                    minChars=""
                                    choices="" 
                                    autoSelect=""
                                    partialSearch=""
                                    partialChars=""
                                    ignoreCase=""
                                    fullSearch=""
                                    tooltip=tooltip
                                    manualItems=manualItems
                                    manualItemsOnly=manualItemsOnly><#nested></@field_select_widget_impl>
            <#break>
          <#case "lookup">
            <@field_lookup_widget_impl name=name formName=formName fieldFormName=fieldFormName className=class alert="false" value=value size=size?string maxlength=maxlength id=id event="onClick" action=onClick />
          <#break>
          <#case "checkbox">
                <@field_checkbox_widget_impl id=id currentValue=value checked=checked name=name action=action />
            <#break>
          <#case "radio">
                <#if radioSingle>
                    <#-- single radio button item mode -->
                    <#local items=[{"key":value, "description":label!""}]/>
                    <@field_radio_widget_impl items=items className=class alert=alert currentValue=(checked?string(value,"")) noCurrentSelectedKey="" name=name event="" action="" tooltip=tooltip />
                <#else>
                    <#-- multi radio button item mode -->
                    <div<@fieldClassStr class alert />>
                      <@field_radio_widget_impl items=items className="" alert=alert currentValue=currentValue noCurrentSelectedKey=defaultValue name=name event="" action="" tooltip=tooltip />
                    </div>
                </#if>
            <#break>
          <#case "file">
            <@field_file_widget_impl className=class alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete?string("", "off") id=id />
            <#break> 
          <#case "password">
            <@field_password_widget_impl className=class alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete?string("", "off") placeholder=placeholder tooltip=tooltip/>
            <#break> 
          <#case "submit">
          <#case "submitarea">
            <#local hasProgress = (progressOptions.formSel)?has_content>
            <#local content>
              <#if type == "submit">
                <#if !catoSubmitFieldTypeButtonMap??>
                  <#global catoSubmitFieldButtonTypeMap = {
                    "submit":"button", "button":"button", "link":"text-link", "image":"image"
                  }>
                  <#global catoSubmitFieldInputTypeMap = {
                    "submit":"submit", "button":"button", "link":"", "image":"image"
                  }>
                </#if>
                <#local buttonType = catoSubmitFieldButtonTypeMap[submitType]!"button">
                <#local inputType = catoSubmitFieldInputTypeMap[submitType]!"">
                <@field_submit_widget_impl buttonType=buttonType className=class alert=alert formName=formName name=name event="" action="" imgSrc=src confirmation=confirmMsg containerId="" ajaxUrl="" title=text showProgress=false onClick=onClick href=href inputType=inputType disabled=disabled />
              <#else>
                <#nested>
              </#if>
            </#local>
            <#if hasProgress>
              <@fieldSubmitAreaProgress progressOptions=progressOptions nestedContent=content />
            <#else>
              ${content}
            </#if>
            <#break> 
          <#case "display">
            <#-- TODO? may need formatting here based on valueType... not done by field_display_widget_impl... done in java OOTB... 
                 can also partially detect type of value with ?is_, but is not enough... -->
            <#if !valueType?has_content || (valueType=="generic")>
              <#local displayType = "text">
            <#else>
              <#local displayType = valueType>
            </#if>
            <#if !value?has_content>
              <#local value><#nested></#local>
            </#if>
            <#if displayType == "image">
              <#local imageLocation = value>
              <#local desc = "">
            <#else>
              <#local imageLocation = "">
              <#local desc = value>
            </#if>
                <@field_display_widget_impl type=displayType imageLocation=imageLocation idName="" description=desc title="" class=class alert=alert inPlaceEditorUrl="" inPlaceEditorParams="" imageAlt=description/>
                <#-- FIXME: tooltip too crappy -->
              <#if tooltip?has_content>
                <span class="tooltip">${tooltip}</span>
              </#if>
            <#break> 
          <#default> <#-- "generic", empty or unrecognized -->
            <#if value?has_content>
                <@field_generic_widget_impl text=value/>
            <#else>
                <#nested />
            </#if>
            <#-- FIXME: tooltip too crappy -->
            <#if tooltip?has_content>
              <span class="tooltip">${tooltip}</span>
            </#if>
        </#switch>
     </@cell>
     <#if postfix && !nocells>
         <@cell class="${styles.grid_small!}${postfixSize} ${styles.grid_large!}${postfixSize}">
                <span class="postfix"><input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/></span>
         </@cell>
     </#if>
</@row>
<#-- pop field info when done -->
<#local dummy = popRequestStack("catoCurrentFieldInfo")>
</#macro>

<#function mapCatoFieldTypeToStyleName fieldType>
  <#return fieldType>
</#function>

<#function mapWidgetFieldTypeToStyleName fieldType>
  <#if !widgetFieldTypeToStyleNameMap??>
    <#-- FIXME: these need to match cato types to unify css classes; they don't quite -->
    <#global widgetFieldTypeToStyleNameMap = {
        "display": "display",
        "hyperlink": "hyperlink",
        "text": "input",
        "textarea": "textarea",
        "date-time": "datetime",
        "drop-down": "select",
        "check": "checkbox",
        "radio": "radio",
        "submit": "submit",
        "reset": "reset",
        "hidden": "hidden",
        "ignored": "ignored",
        "text-find": "textfind",
        "date-find": "datefind",
        "range-find": "rangefind",
        "lookup": "lookup",
        "file": "file",
        "password": "password",
        "image": "image",
        "display-entity": "displayentity",
        "container": "container"
    }>
  </#if>
  <#return widgetFieldTypeToStyleNameMap[fieldType]!"other">
</#function>

<#-- migrated from @renderClass form widget macro -->
<#macro fieldClassStr className alert="false">
  <#if className?has_content || alert?string == "true"> class="${className!}<#if alert?string == "true"> alert</#if>" </#if>
</#macro>

<#-- migrated from @renderSubmitFieldAreaProgress form widget macro -->
<#macro fieldSubmitAreaProgress progressOptions nestedContent=true>
  <#if !nestedContent?is_string>
    <#if nestedContent?is_boolean && nestedContent == false>
      <#local nestedContent = "">
    <#else>
      <#local nestedContent><#nested></#local>
    </#if>
  </#if>

  <#local rowClass>submit-progress-row<#if buttonMarkup?has_content> has-submit-button<#else> no-submit-button</#if></#local>
  <@row class=("+" + rowClass)>
    <#if nestedContent?has_content>
      <@cell class="${styles.grid_small!}3 ${styles.grid_large!}2">
        ${nestedContent}
      </@cell>
    </#if>
    <#if progressOptions.progBarId?has_content>
      <#-- with progress bar, optional text -->
      <#local subclasses = progressOptions.progTextBoxId?has_content?string("${styles.grid_small!}6 ${styles.grid_large!}6", "${styles.grid_small!}9 ${styles.grid_large!}10 ${styles.grid_end!}")>
      <@cell class=subclasses>
        <@progress id=progressOptions.progBarId type="info" wrapperClass="+${styles.hidden!}" progressOptions=progressOptions/>
      </@cell>
      <#if progressOptions.progTextBoxId?has_content>
        <#local subclasses = "${styles.grid_small!}3 ${styles.grid_large!}4 ${styles.grid_end!}">
        <@cell class=subclasses id=progressOptions.progTextBoxId>
        </@cell>
      </#if>
    <#elseif progressOptions.progTextBoxId?has_content>
       <#-- text progress only -->
       <#local subclasses = "${styles.grid_small!}9 ${styles.grid_large!}10 ${styles.grid_end!}">
       <@cell class=subclasses id=progressOptions.progTextBoxId>
       </@cell>
       <@progressScript options=progressOptions htmlwrap=true />
    </#if>
  </@row>
</#macro>


<#-- INDIVIDUAL FIELD IMPLEMENTATIONS
     DEV NOTE: see @section_impl for details on current _impl pattern used below (transitory)
     These are not final implementation pattern and transitory to eliminate macro lib dependencies,
     and to head toward separating macro logic and markup.
     TODO: clean up macro arguments
     NOTE: "widget" here refers to the common meaning; not any specific ofbiz meaning -->

<#-- migrated from @renderTextField form widget macro -->
<#macro field_input_widget_impl name="" className="" alert="" value="" textSize="" maxlength="" id="" event="" action="" disabled=false ajaxUrl="" ajaxEnabled=false 
    mask=false clientAutocomplete="" placeholder="" tooltip="" collapse=false readonly=false fieldTitleBlank=false>
  <#if tooltip?has_content> 
     <#local className = (className+ " has-tip tip-right")/>  
  </#if>
  <#if mask?has_content && mask>
    <script type="text/javascript">
      jQuery(function($){jQuery("#${id}").mask("${mask!}");});
    </script>
  </#if>
  <input type="text" name="${name?default("")?html}"<#t/>
    <#if tooltip?has_content> 
     data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"<#rt/>
     </#if><#rt/>
    <@fieldClassStr className alert />
    <#if value?has_content> value="${value}"</#if><#rt/>
    <#if textSize?has_content> size="${textSize}"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    <#if disabled?has_content && disabled> disabled="disabled"</#if><#rt/>
    <#if readonly?has_content && readonly> readonly="readonly"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if event?has_content && action?has_content> ${event}="${action}"</#if><#rt/>
    <#if clientAutocomplete?has_content && clientAutocomplete=="false"> autocomplete="off"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#rt/>
    <#if className?has_content> class="${className}"</#if><#rt/>
  /><#t/>
  <#if ajaxUrl?has_content>
    <#local defaultMinLength = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")>
    <#local defaultDelay = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")>
    <script language="JavaScript" type="text/javascript">ajaxAutoCompleter('${ajaxUrl}', false, ${defaultMinLength!2}, ${defaultDelay!300});</script><#lt/>
  </#if>
</#macro>

<#-- migrated from @renderTextareaField form widget macro -->
<#macro field_textarea_widget_impl name="" className="" alert="" cols="" rows="" id="" readonly="" value="" visualEditorEnable=true 
    buttons="" language="" placeholder="" tooltip="" title="" fieldTitleBlank=false collapse=false>
  <#if tooltip?has_content> 
     <#local className = (className+ " has-tip tip-right")/>  
  </#if>
  <textarea name="${name}"<#t/>
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
    <@fieldClassStr className alert />
    <#if cols?has_content> cols="${cols}"</#if><#rt/>
    <#if rows?has_content> rows="${rows}"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if readonly?has_content> readonly="readonly"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#t/>
    <#if className?has_content> class="${className}"</#if><#rt/>
    ><#t/>
    <#if value?has_content>${value}</#if>
  </textarea><#lt/>
  
  <#--
  ToDo: Remove
  <#if visualEditorEnable?has_content>
    <script language="javascript" src="/images/jquery/plugins/elrte-1.3/js/elrte.min.js" type="text/javascript"></script><#rt/>
    <#if language?has_content && language != "en">
      <script language="javascript" src="/images/jquery/plugins/elrte-1.3/js/i18n/elrte.${language!"en"}.js" type="text/javascript"></script><#rt/>
    </#if>
    <link href="/images/jquery/plugins/elrte-1.3/css/elrte.min.css" rel="stylesheet" type="text/css">
    <script language="javascript" type="text/javascript">
      var opts = {
         cssClass : 'el-rte',
         lang     : '${language!"en"}',
         toolbar  : '${buttons?default("maxi")}',
         doctype  : '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">', //'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN">',
         cssfiles : ['/images/jquery/plugins/elrte-1.3/css/elrte-inner.css']
      }
      jQuery('#${id?default("")}').elrte(opts);
    </script>
  </#if>
  -->
</#macro>

<#-- Merges a yyyy-MM-dd into a full timestamp
     TODO: move this out to js file -->
<#assign mergeStdDateTimeJs>
                var mergeStdDateTime = function(oldDate, newDate) {
                    var result;
                    if (oldDate.match(/^\d\d\d\d-\d\d-\d\d\s/)) {
                       if (newDate.length >= oldDate.length) {
                           result = newDate;
                       }
                       else {
                           <#-- preserve everything after days -->
                           result = newDate + oldDate.substr(newDate.length);
                       }
                    }
                    else {
                       var zeroPat = "0000-00-00 00:00:00.000";
                       if (newDate.length >= zeroPat.length) {
                           result = newDate;
                       }
                       else {
                           <#-- append zeroes -->
                           result = newDate + zeroPat.substr(newDate.length);
                       }
                    }
                    return result;
                };
</#assign>

<#-- migrated from @renderDateTimeField form widget macro -->
<#macro field_datetime_widget_impl name="" className="" title="" value="" size="" maxlength="" id="" dateType="" shortDateInput=false 
    timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" 
    hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName="" 
    alert=false mask="" event="" action="" step="" timeValues="" tooltip=""collapse=false fieldTitleBlank=false>
  
  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- Note: ofbiz never handled dateType=="date" here because it pass shortDateInput=true in renderer instead-->
  <#-- These should be ~uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if (shortDateInput!false) == true>yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (((shortDateInput!false) == false) && dateType!="time")>

  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}11 ${styles.grid_cell!}">
          <#if dateType == "time">
            <input type="text" name="${name}" <@fieldClassStr className alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}"</#if> class="${styles.grid_small!}3 ${styles.grid_cell!}"/><#rt/>
          <#else>
            <input type="text" name="${name}_i18n" <@fieldClassStr className alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}_i18n"</#if> class="${styles.grid_small!}3 ${styles.grid_cell!}"/><#rt/>

            <input type="hidden" name="${name}"<#if id?has_content> id="${id}"</#if><#if value?has_content> value="${value}"</#if> />
          </#if>
        </div>
        <div class="${styles.grid_small!}1 ${styles.grid_cell!}">
        <span class="postfix"><i class="${styles.icon!} ${styles.icon_calendar!}"></i></span>
        </div>
      <#if dateType != "time">
        <script type="text/javascript">
            $(function() {

                var dateI18nToNorm = function(date) {
                    <#-- TODO (note: depends on dateType) -->
                    return date;
                };
                
                var dateNormToI18n = function(date) {
                    <#-- TODO (note: depends on dateType) -->
                    return date;
                };
            
                jQuery("#${id}_i18n").change(function() {
                    jQuery("#${id}").val(dateI18nToNorm(this.value));
                });
                
              <#if useTsFormat>
                ${mergeStdDateTimeJs}
              </#if>
                
                var oldDate = "";
                var onFDatePopup = function(ev) {
                    oldDate = dateI18nToNorm(jQuery("#${id}_i18n").val());
                };
                var onFDateChange = function(ev) {
                  <#if useTsFormat>
                    jQuery("#${id}_i18n").val(dateNormToI18n(mergeStdDateTime(oldDate, dateI18nToNorm(jQuery("#${id}_i18n").val()))));
                  </#if>
                };
                
                <#if name??>
                    <#local dateElemJs>$("input[name='${name?html}_i18n']")</#local>
                <#else>
                    <#local dateElemJs>$("input")</#local>
                </#if>
                ${dateElemJs}.fdatepicker(${fdatepickerOptions}).on('changeDate', onFDateChange).on('show', onFDatePopup);
            });
        </script>
      </#if>
  </div>
</#macro>

<#-- migrated from @renderDateFindField form widget macro -->
<#macro field_datefind_widget_impl className="" alert="" name="" localizedInputTitle="" value="" value2="" size="" maxlength="" dateType="" 
    formName="" defaultDateTimeString="" imgSrc="" localizedIconTitle="" titleStyle="" defaultOptionFrom="" defaultOptionThru="" 
    opEquals="" opSameDay="" opGreaterThanFromDayStart="" opGreaterThan="" opGreaterThan="" opLessThan="" opUpToDay="" opUpThruDay="" opIsEmpty="">

  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- note: values of localizedInputTitle are: uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if dateType == "date">yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (dateType != "date" && dateType != "time")>
  
  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}5 ${styles.grid_cell!}">
        <input class="${styles.grid_small!}3 ${styles.grid_cell!}" id="${name?html}_fld0_value" type="text" <@fieldClassStr className alert /><#if name?has_content> name="${name?html}_fld0_value"</#if><#if localizedInputTitle?has_content> title="${localizedInputTitle}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if>/><#rt/>
        </div>
        <div class="${styles.grid_small!}1 ${styles.grid_cell!}">
        <span class="postfix"><i class="${styles.icon} ${styles.icon_calendar!}"></i></span>
        </div>
        <div class="${styles.grid_small!}5 ${styles.grid_cell!} ${styles.grid_small!}offset-1">
        <select<#if name?has_content> name="${name}_fld0_op"</#if> class="selectBox"><#rt/>
          <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
          <option value="sameDay"<#if defaultOptionFrom=="sameDay"> selected="selected"</#if>>${opSameDay}</option><#rt/>
          <option value="greaterThanFromDayStart"<#if defaultOptionFrom=="greaterThanFromDayStart"> selected="selected"</#if>>${opGreaterThanFromDayStart}</option><#rt/>
          <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${opGreaterThan}</option><#rt/>
        </select><#rt/>
        </div>
      <#if dateType != "time">
        <script type="text/javascript">
            $(function() {
            
              <#if useTsFormat>
                ${mergeStdDateTimeJs}
              </#if>
                
                var oldDate = "";
                var onFDatePopup = function(ev) {
                    oldDate = jQuery("#${name?html}_fld0_value").val();
                };
                var onFDateChange = function(ev) {
                  <#if useTsFormat>
                    jQuery("#${name?html}_fld0_value").val(mergeStdDateTime(oldDate, jQuery("#${name?html}_fld0_value").val()));
                  </#if>
                };
            
                <#if name??>
                    <#local dateElemJs>$('#${name?html}_fld0_value')</#local>
                <#else>
                    <#local dateElemJs>$('input')</#local>
                </#if>
                ${dateElemJs}.fdatepicker(${fdatepickerOptions}).on('changeDate', onFDateChange).on('show', onFDatePopup);
            });
        </script>
      </#if>
  </div>
</#macro>

<#-- migrated from @renderDropDownField form widget macro -->
<#macro field_select_widget_impl name="" className="" alert="" id="" multiple="" formName="" otherFieldName="" size="" firstInList="" 
    currentValue="" explicitDescription="" allowEmpty="" options="" fieldName="" otherFieldName="" otherValue="" otherFieldSize="" 
    dDFCurrent="" noCurrentSelectedKey="" ajaxOptions="" frequency="" minChars="" choices="" autoSelect="" partialSearch="" partialChars="" 
    ignoreCase="" fullSearch="" event="" action="" ajaxEnabled=false tooltip="" manualItems=false manualItemsOnly=false 
    collapse=false fieldTitleBlank=false>

    <select name="${name!""}<#rt/>" <@fieldClassStr className alert /><#if id?has_content> id="${id}"</#if><#if multiple?has_content> multiple="multiple"</#if><#if otherFieldSize gt 0> onchange="process_choice(this,document.${formName}.${otherFieldName})"</#if><#if event?has_content> ${event}="${action}"</#if><#--<#if size?has_content> size="${size}"</#if>-->
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>>
    <#if !manualItemsOnly>  
      <#if firstInList?has_content && currentValue?has_content && !multiple?has_content>
        <option selected="selected" value="${currentValue}">${explicitDescription}</option><#rt/>
        <option value="${currentValue}">---</option><#rt/>
      </#if>
      <#if allowEmpty?has_content || (!manualItems && !options?has_content)>
        <option value="">&nbsp;</option>
      </#if>
      <#list options as item>
        <#if multiple?has_content>
          <option<#if currentValue?has_content && item.selected?has_content> selected="${item.selected}" <#elseif !currentValue?has_content && noCurrentSelectedKey?has_content && noCurrentSelectedKey == item.key> selected="selected" </#if> value="${item.key}">${item.description}</option><#rt/>
        <#else>
          <option<#if currentValue?has_content && currentValue == item.key && dDFCurrent?has_content && "selected" == dDFCurrent> selected="selected"<#elseif !currentValue?has_content && noCurrentSelectedKey?has_content && noCurrentSelectedKey == item.key> selected="selected"</#if> value="${item.key}">${item.description}</option><#rt/>
        </#if>
      </#list>
    </#if>
      <#nested>
    </select>
  <#if otherFieldName?has_content>
    <noscript><input type='text' name='${otherFieldName}' /></noscript>
    <script type='text/javascript' language='JavaScript'><!--
      disa = ' disabled';
      if(other_choice(document.${formName}.${fieldName}))
        disa = '';
      document.write("<input type='text' name='${otherFieldName}' value='${otherValue?js_string}' size='${otherFieldSize}'"+disa+" onfocus='check_choice(document.${formName}.${fieldName})' />");
      if(disa && document.styleSheets)
      document.${formName}.${otherFieldName}.styles.visibility  = 'hidden';
    //--></script>
  </#if>

  <#if ajaxEnabled>
    <script language="JavaScript" type="text/javascript">
      ajaxAutoCompleteDropDown();
      jQuery(function() {
        jQuery("#${id}").combobox();
      });
    </script>
  </#if>
</#macro>

<#-- migrated from @renderLookupField form widget macro -->
<#macro field_lookup_widget_impl name="" formName="" fieldFormName="" className="" alert="false" value="" size="" 
    maxlength="" id="" event="" action="" readonly=false autocomplete="" descriptionFieldName="" 
    targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=javaScriptEnabled presentation="layer" width="" 
    height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" 
    lastViewName="main" title="" fieldTitleBlank=false>

  <#if Static["org.ofbiz.widget.model.ModelWidget"].widgetBoundaryCommentsEnabled(context)>
  </#if>
  <#if (!ajaxUrl?has_content) && ajaxEnabled?has_content && ajaxEnabled>
    <#local ajaxUrl = requestAttributes._REQUEST_HANDLER_.makeLink(request, response, fieldFormName)/>
    <#local ajaxUrl = id + "," + ajaxUrl + ",ajaxLookup=Y" />
  </#if>
  <#if (!showDescription?has_content)>
    <#local showDescriptionProp = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.lookup.showDescription", "N")>
    <#if "Y" == showDescriptionProp>
      <#local showDescription = "true" />
    <#else>
      <#local showDescription = "false" />
    </#if>
  </#if>
  <#if ajaxEnabled?has_content && ajaxEnabled>
    <script type="text/javascript">
      jQuery(document).ready(function(){
        if (!jQuery('form[name="${formName}"]').length) {
          alert("Developer: for lookups to work you must provide a form name!")
        }
      });
    </script>
  </#if>
  <span class="field-lookup">
    <#if size?has_content && size=="0">
      <input type="hidden" <#if name?has_content> name="${name}"/></#if>
    <#else>
      <input type="text" <@fieldClassStr className alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if>
        <#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#rt/>
        <#if readonly?has_content && readonly> readonly="readonly"</#if><#rt/><#if event?has_content && action?has_content> ${event}="${action}"</#if><#rt/>
        <#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/></#if>
    <#if presentation?has_content && descriptionFieldName?has_content && presentation == "window">
      <a href="javascript:call_fieldlookup3(document.${formName?html}.${name?html},document.${formName?html}.${descriptionFieldName},'${fieldFormName}', '${presentation}'<#rt/>
      <#if targetParameterIter?has_content>
        <#list targetParameterIter as item>
          ,document.${formName}.${item}.value<#rt>
        </#list>
      </#if>
      );"></a><#rt>
    <#elseif presentation?has_content && presentation == "window">
      <a href="javascript:call_fieldlookup2(document.${formName?html}.${name?html},'${fieldFormName}', '${presentation}'<#rt/>
      <#if targetParameterIter?has_content>
        <#list targetParameterIter as item>
          ,document.${formName}.${item}.value<#rt>
        </#list>
      </#if>
      );"></a><#rt>
    <#else>
      <#if ajaxEnabled?has_content && ajaxEnabled>
        <#local defaultMinLength = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")>
        <#local defaultDelay = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")>
        <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
        <#if !ajaxUrl?contains("searchValueFieldName=")>
          <#if descriptionFieldName?has_content && showDescription == "true">
            <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + descriptionFieldName />
          <#else>
            <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + name />
          </#if>
        </#if>
      </#if>
      <script type="text/javascript">
        jQuery(document).ready(function(){
          var options = {
            requestUrl : "${fieldFormName}",
            inputFieldId : "${id}",
            dialogTarget : document.${formName?html}.${name?html},
            dialogOptionalTarget : <#if descriptionFieldName?has_content>document.${formName?html}.${descriptionFieldName}<#else>null</#if>,
            formName : "${formName?html}",
            width : "${width}",
            height : "${height}",
            position : "${position}",
            modal : "${fadeBackground}",
            ajaxUrl : <#if ajaxEnabled?has_content && ajaxEnabled>"${ajaxUrl}"<#else>""</#if>,
            showDescription : <#if ajaxEnabled?has_content && ajaxEnabled>"${showDescription}"<#else>false</#if>,
            presentation : "${presentation!}",
            defaultMinLength : "${defaultMinLength!2}",
            defaultDelay : "${defaultDelay!300}",
            args :
              <#rt/>
                <#if targetParameterIter?has_content>
                  <#local isFirst = true>
                  <#lt/>[<#rt/>
                  <#list targetParameterIter as item>
                    <#if isFirst>
                      <#lt/>document.${formName}.${item}<#rt/>
                      <#local isFirst = false>
                    <#else>
                      <#lt/> ,document.${formName}.${item}<#rt/>
                    </#if>
                  </#list>
                  <#lt/>]<#rt/>
                <#else>[]
                </#if>
                <#lt/>
          };
          new Lookup(options).init();
        });
      </script>
    </#if>
    <#if readonly?has_content && readonly>
      <a id="${id}_clear" 
        style="background:none;margin-left:5px;margin-right:15px;" 
        class="clearField" 
        href="javascript:void(0);" 
        onclick="javascript:document.${formName}.${name}.value='';
          jQuery('#' + jQuery('#${id}_clear').next().attr('id').replace('_button','') + '_${id}_lookupDescription').html('');
          <#if descriptionFieldName?has_content>document.${formName}.${descriptionFieldName}.value='';</#if>">
          <#if clearText?has_content>${clearText}<#else>${uiLabelMap.CommonClear}</#if>
      </a>
    </#if>
  </span>
  <#if ajaxEnabled?has_content && ajaxEnabled && (presentation?has_content && presentation == "window")>
    <#if ajaxUrl?index_of("_LAST_VIEW_NAME_") < 0>
      <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
    </#if>
    <script language="JavaScript" type="text/javascript">ajaxAutoCompleter('${ajaxUrl}', ${showDescription}, ${defaultMinLength!2}, ${defaultDelay!300});</script><#t/>
  </#if>
</#macro>

<#-- migrated from @renderCheckBox form widget macro -->
<#macro field_checkbox_widget_impl id="" checked=false currentValue="N" name="" action="" tooltip="" fieldTitleBlank=false>
    <div class="switch small">
    <input type="checkbox" id="<#if id?has_content>${id}<#else>${name!}</#if>"<#rt/>
      <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
      <#if (checked?is_boolean && checked) || (checked?is_string && checked == "Y")> checked="checked"
      <#elseif currentValue?has_content && currentValue=="Y"> checked="checked"</#if> 
      name="${name!""?html}" value="${currentValue!}"<#if action?has_content> onClick="${action}"</#if>/><#rt/>
      <label for="<#if id?has_content>${id}<#else>${name!}</#if>"></label>
    </div>
</#macro>

<#-- migrated from @renderRadioField form widget macro -->
<#macro field_radio_widget_impl items="" className="" alert="" currentValue="" noCurrentSelectedKey="" name="" event="" action="" tooltip="">
  <#list items as item>
    <span <@fieldClassStr className alert />><#rt/>
      <input type="radio"<#if currentValue?has_content><#if currentValue==item.key> checked="checked"</#if>
        <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
        <#elseif noCurrentSelectedKey?has_content && noCurrentSelectedKey == item.key> checked="checked"</#if> 
        name="${name!""?html}" value="${item.key!""?html}"<#if item.event?has_content> ${item.event}="${item.action!}"<#elseif event?has_content> ${event}="${action!}"</#if>/><#rt/>
      ${item.description}
    </span>
  </#list>
</#macro>

<#-- migrated from @renderFileField form widget macro -->
<#macro field_file_widget_impl className="" alert="" name="" value="" size="" maxlength="" autocomplete="" id="" title="" fieldTitleBlank=false>
  <input type="file" <@fieldClassStr className alert /><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
</#macro>

<#-- migrated from @renderPasswordField form widget macro -->
<#macro field_password_widget_impl className="" alert="" name="" value="" size="" maxlength="" id="" autocomplete="" title="" placeholder="" fieldTitleBlank=false tooltip="">
  <#if tooltip?has_content> 
     <#local className = (className+ " has-tip tip-right")/>  
  </#if> 
  <input type="password" <@fieldClassStr className alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if>
  <#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#if autocomplete?has_content> autocomplete="off"</#if> 
  <#if placeholder?has_content> placeholder="${placeholder}"</#if>
  <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
  <#if className?has_content> class="${className}"</#if><#rt/>/>
</#macro>

<#-- migrated from @renderSubmitField form widget macro -->
<#macro field_submit_widget_impl buttonType="" className="" alert="" formName="" name="" event="" action="" imgSrc="" confirmation="" 
    containerId="" ajaxUrl="" title="" fieldTitleBlank=false showProgress="" href="" onClick="" inputType="" disabled=false progressOptions={}>
  <#-- Cato: FIXME?: factor out default submit class somewhere so configurable -->
  <#if buttonType!="image">
    <#if !className?has_content || className=="smallSubmit">
      <#local className = "${styles.button_default!}">
    </#if>
  </#if>

  <#-- Cato: to omit button (show progress only), we use empty title hack " " similar to what ofbiz does with hyperlinks with no label -->
  <#if (buttonType=="text-link" || buttonType!="image") && !(title?trim?has_content)>
  <#local buttonMarkup = "">
  <#else>
  <#local buttonMarkup>
  <#if buttonType=="text-link">
    <a <@fieldClassStr className alert /> href="<#if href?has_content>${href}<#elseif formName?has_content>javascript:document.${formName}.submit()<#else>javascript:void(0)</#if>"<#if disabled> disabled="disabled"<#else><#if onClick?has_content> onclick="${onClick}"<#elseif confirmation?has_content> onclick="return confirm('${confirmation?js_string}');"</#if></#if>><#if title?has_content>${title}</#if></a>
  <#elseif buttonType=="image">
    <input type="<#if inputType?has_content>${inputType}<#else>image</#if>" src="${imgSrc}" <@fieldClassStr className alert /> <#if name?has_content> name="${name}"</#if>
    <#if title?has_content> alt="${title}"</#if><#if event?has_content> ${event}="${action}"</#if>
    <#if disabled> disabled="disabled"<#else>
      <#if onClick?has_content> onclick="${onClick}"<#elseif confirmation?has_content>onclick="return confirm('${confirmation?js_string}');"</#if>
    </#if>/>
  <#else>
    <input type="<#if inputType?has_content>${inputType}<#elseif containerId?has_content>button<#else>submit</#if>" <@fieldClassStr className alert />
    <#if name?has_content> name="${name}"</#if><#if title?has_content> value="${title}"</#if><#if event?has_content> ${event}="${action}"</#if>
    <#if disabled> disabled="disabled"<#else>
      <#if onClick?has_content> onclick="${onClick}"<#else>
        <#if containerId?has_content> onclick="<#if confirmation?has_content>if (confirm('${confirmation?js_string}')) </#if>ajaxSubmitFormUpdateAreas('${containerId}', '${ajaxUrl}')"<#else>
        <#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}');"</#if>
        </#if>
      </#if>
    </#if>/>
  </#if>
  </#local>
  </#if>
  <#if progressOptions?has_content>
      <@fieldSubmitAreaProgress progressOptions=progressOptions nestedContent=buttonMarkup />
  <#else>
      ${buttonMarkup}
  </#if>
</#macro>

<#-- migrated from @renderDisplayField form widget macro -->
<#macro field_display_widget_impl type="" imageLocation="" idName="" description="" title="" class="" alert="" inPlaceEditorUrl="" 
    inPlaceEditorParams="" imageAlt=""collapse=false fieldTitleBlank=false>
  <#if type?has_content && type=="image">
    <img src="${imageLocation}" alt="${imageAlt}"><#lt/>
  <#else>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true" || title?has_content>
      <span<#if idName?has_content> id="cc_${idName}"</#if><#if title?has_content> title="${title}"</#if> <@fieldClassStr class alert />><#t/>
    </#if>
    -->
    <#if description?has_content>
      ${description?replace("\n", "<br />")}<#t/>
    <#else>
      &nbsp;<#t/>
    </#if>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true">
      </span><#lt/>
    </#if>
    <#if inPlaceEditorUrl?has_content && idName?has_content>
      <script language="JavaScript" type="text/javascript"><#lt/>
        ajaxInPlaceEditDisplayField('cc_${idName}', '${inPlaceEditorUrl}', ${inPlaceEditorParams});<#lt/>
      </script><#lt/>
    </#if>-->
    </#if>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#macro field_textfind_widget_impl name="" value="" defaultOption="" opEquals="" opBeginsWith="" opContains="" 
    opIsEmpty="" opNotEqual="" className="" alert="" size="" maxlength="" autocomplete="" titleStyle="" 
    hideIgnoreCase="" ignCase="" ignoreCase="" title="" fieldTitleBlank=false>

  <@row collapse=collapse!false>
  <#if opEquals?has_content>
            <#local class1="${styles.grid_small!}3 ${styles.grid_large!}3"/>
            <#local class2="${styles.grid_small!}6 ${styles.grid_large!}6"/>
            <#local class3="${styles.grid_small!}3 ${styles.grid_large!}3"/>
            
        <#else>
            <#local class1=""/>
            <#local class2="${styles.grid_small!}9 ${styles.grid_large!}9"/>
            <#local class3="${styles.grid_small!}3 ${styles.grid_large!}3"/>
      </#if>      
      <#if opEquals?has_content>
        <#local newName = "${name}"/>
        <@cell class="${class1!}">
    <select <#if name?has_content>name="${name}_op"</#if>    class="selectBox"><#rt/>
      <option value="equals"<#if defaultOption=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
      <option value="like"<#if defaultOption=="like"> selected="selected"</#if>>${opBeginsWith}</option><#rt/>
      <option value="contains"<#if defaultOption=="contains"> selected="selected"</#if>>${opContains}</option><#rt/>
      <option value="empty"<#rt/><#if defaultOption=="empty"> selected="selected"</#if>>${opIsEmpty}</option><#rt/>
      <option value="notEqual"<#if defaultOption=="notEqual"> selected="selected"</#if>>${opNotEqual}</option><#rt/>
    </select>
        </@cell>
  <#else>
    <input type="hidden" name=<#if name?has_content> "${name}_op"</#if>    value="${defaultOption}"/><#rt/>
  </#if>
      <@cell class="${class2!}">
    <input type="text" <@fieldClassStr className alert /> name="${name}"<#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
       
      </@cell>
      <@cell class="${class3!}"> 
    <#if hideIgnoreCase>
      <input type="hidden" name="${name}_ic" value=<#if ignCase>"Y"<#else> ""</#if>/><#rt/>
    <#else>
            <div class="">
                <label for="${name}_ic"><input type="checkbox" id="${name}_ic" name="${name}_ic" value="Y" <#if ignCase> checked="checked"</#if> />
                ${ignoreCase!}</label>
                <#rt/>
            </div>
    </#if>
      </@cell>
  </@row>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#macro field_rangefind_widget_impl className="" alert="" name="" value="" size="" maxlength="" autocomplete="" titleStyle="" defaultOptionFrom="" opEquals="" opGreaterThan="" opGreaterThanEquals="" opLessThan="" opLessThanEquals="" value2="" defaultOptionThru="">
  <#local class1="${styles.grid_small!}9 ${styles.grid_large!}9"/>
  <#local class2="${styles.grid_small!}3 ${styles.grid_large!}3"/>
  <@row collapse=collapse!false>
    <@cell class=class1>
      <input type="text" <@fieldClassStr className alert /><#if name?has_content> name="${name}_fld0_value"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
    </@cell>
    <@cell class=class2>
      <#if titleStyle?has_content>
        <span class="${titleStyle}"><#rt/>
      </#if>
      <select<#if name?has_content> name="${name}_fld0_op"</#if> class="selectBox"><#rt/>
        <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
        <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${opGreaterThan}</option><#rt/>
        <option value="greaterThanEqualTo"<#if defaultOptionFrom=="greaterThanEqualTo"> selected="selected"</#if>>${opGreaterThanEquals}</option><#rt/>
      </select><#rt/>
      <#if titleStyle?has_content>
        </span><#rt/>
      </#if>
    </@cell>
  </@row><#rt/>
  <@row>
    <@cell class=class1>
      <input type="text" <@fieldClassStr className alert /><#if name?has_content> name="${name}_fld1_value"</#if><#if value2?has_content> value="${value2}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
    </@cell>
    <@cell class=class2>
      <#if titleStyle?has_content>
        <span class="${titleStyle}"><#rt/>
      </#if>
      <select name=<#if name?has_content>"${name}_fld1_op"</#if> class="selectBox"><#rt/>
        <option value="lessThan"<#if defaultOptionThru=="lessThan"> selected="selected"</#if>>${opLessThan?html}</option><#rt/>
        <option value="lessThanEqualTo"<#if defaultOptionThru=="lessThanEqualTo"> selected="selected"</#if>>${opLessThanEquals?html}</option><#rt/>
      </select><#rt/>
      <#if titleStyle?has_content>
        </span>
      </#if>
    </@cell>
  </@row>
</#macro>

<#-- migrated from @renderField form widget macro -->
<#macro field_generic_widget_impl text="">
  <#if text??>
    ${text}<#lt/>
  </#if>
</#macro>

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

