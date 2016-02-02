<#--
* 
* Informational/notification element HTML template include, standard Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#-- 
*************
* Modal
************
  * Usage Example *  
    <@modal id="dsadsa" attr="" >
        Modal Content 
    </@modal>        
            
  * Parameters *
    id              = set id (required)
    label           = set anchor text (required)
    icon            = generates icon inside the link (Note: has to be the full set of classes, e.g. "fa fa-fw fa-info")
-->
<#assign modal_defaultArgs = {
  "id":"", "label":"", "href":"", "icon":"", "passArgs":{}
}>
<#macro modal args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.modal_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@modal_markup id=id label=label href=href icon=icon origArgs=origArgs passArgs=passArgs><#nested></@modal_markup>
</#macro>

<#-- @modal main markup - theme override -->
<#macro modal_markup id="" label="" href="" icon="" origArgs={} passArgs={} catchArgs...>
  <a href="#" data-reveal-id="${id}_modal"<#if href?has_content> data-reveal-ajax="${href!}"</#if>><#if icon?has_content><i class="${icon!}"></i> </#if>${label}</a>
  <div id="${id}_modal" class="${styles.modal_wrap!}" data-reveal>
    <#nested>
    <a class="close-reveal-modal">&#215;</a>
  </div>
</#macro>

<#-- 
*************
* Alert box
************
Alert box for messages that must grab user attention.
NOTE: Should avoid using this for regular, common inlined message results such as "no records found" unless
    it's an unexpected result, error or one that requires user action.
    For most cases, it is preferrable to use @commonMsg macro because it is higher-level.

  * Usage Example *  
    <@alert type="info">
        This is an alert!
    </@alert>            
                    
  * Parameters *
    type           = (info|success|warning|secondary|alert|error), default info
    class          = classes or additional classes for nested container
                     supports prefixes:
                       "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                       "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
-->
<#assign alert_defaultArgs = {
  "type":"info", "class":"", "id":"", "passArgs":{}
}>
<#macro alert args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.alert_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local typeClass = "alert_type_${type!}"/>
  <#if type="error">
    <#local type = "alert">
  </#if>
  <@alert_markup type=type class=class typeClass=typeClass id=id origArgs=origArgs passArgs=passArgs><#nested></@alert_markup>
</#macro>

<#-- @alert main markup - theme override -->
<#macro alert_markup type="info" class="" typeClass="" id="" origArgs={} passArgs={} catchArgs...>
  <#local class = addClassArg(class, styles.grid_cell!"")>
  <#local class = addClassArgDefault(class, "${styles.grid_large!}12")>
  <div class="${styles.grid_row!}"<#if id?has_content> id="${id}"</#if>>
    <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
      <div data-alert class="${styles.alert_wrap!} ${styles[typeClass]!}">
        <div class="${styles.grid_row!}">
          <div<@compiledClassAttribStr class=class />>
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
  * Usage Example *  
    <@panel type="">
        This is a panel.
    </@panel>            
                    
  * Parameters *
    type           = (callout|) default:empty
    title          = Title
-->
<#assign panel_defaultArgs = {
  "type":"", "title":"", "passArgs":{}
}>
<#macro panel args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.panel_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@panel_markup type=type title=title origArgs=origArgs passArgs=passArgs><#nested></@panel_markup>
</#macro>

<#-- @panel main markup - theme override -->
<#macro panel_markup type="" title="" origArgs={} passArgs={} catchArgs...>
  <div class="${styles.panel_wrap!} ${type}">
    <div class="${styles.panel_head!}"><#if title?has_content><h5 class="${styles.panel_title!}">${title!}</h5></#if></div>
    <div class="${styles.panel_body!}"><p><#nested></p></div>
  </div>
</#macro>

<#-- 
*************
* Common messages (abstraction)
************
Abstracts and factors out the display format of common messages of specific meanings, such as
errors (e.g. permission errors) and query results (e.g. "no records found" messages).

This is higher-level than @alert macro; @alert has a specific markup/display and its types are levels of importance,
whereas @commonMsg abstracts markup/display and its messages can be a combination of levels and specific meanings;
@alert's goal is to grab user attention, whereas @commonMsg's behavior depends on the type of message.

@commonMsg may use @alert to implement itself.
A template should not assume too much about the message markup, but the markup should be kept simple.

  * Usage Example *  
    <@commonMsg type="result">${uiLabelMap.CommonNoRecordFound}.</@commonMsg>            
                    
  * Parameters *
    type        = [default|generic|result|fail|error|error-perm|error-security], default default - the type of message contained.
                  Basic types:
                    default: default. in standard Cato markup, same as generic.
                    generic: no type specified (avoid using - prefer more specific)
                  Types recognizes by Cato standard markup (theme-defined types are possible):
                    result: an informational result from a query of some kind. e.g., "no records found".
                        is a normal event that shouldn't distract user attention.
                    fail: a non-fatal error
                    error, error-*: an error message - typically an unexpected event or fatal error that should not happen in normal use.
    id          = id
    class       = classes or additional classes for message container (innermost containing element)
                  (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#assign commonMsg_defaultArgs = {
  "type":"", "class":"", "id":"", "passArgs":{}
}>
<#macro commonMsg args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.commonMsg_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !type?has_content>
    <#local type = "generic">
  </#if>
  <#-- TODO: lookup default class based on type -->
  <#local defaultClass = styles["commonmsg_" + type?replace("-", "_")]!styles["commonmsg_default"]>
  <#local class = addClassArgDefault(class, defaultClass)>
  <@commonMsg_markup type=type class=class id=id origArgs=origArgs passArgs=passArgs><#nested></@commonMsg_markup>
</#macro>

<#-- @commonMsg main markup - theme override -->
<#macro commonMsg_markup type="" class="" id="" origArgs={} passArgs={} catchArgs...>
  <#if type == "result">
    <p<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if>><#nested></p>
  <#elseif type == "error" || type?starts_with("error-")>
    <@alert type="error" class=class id=id><#nested></@alert>
  <#elseif type == "fail">
    <@alert type="error" class=class id=id><#nested></@alert>
  <#else>
    <p<@compiledClassAttribStr class=class /><#if id?has_content> id="${id}"</#if>><#nested></p>
  </#if>
</#macro>



