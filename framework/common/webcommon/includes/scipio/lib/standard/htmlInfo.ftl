<#--
* 
* Information & Notifications
*
* The information and notification elements can be used to define content that should catch the users attention,
* such as modals, panels and alerts.
*
* Included by htmlTemplate.ftl.
*
* NOTES: 
* * May have implicit dependencies on other parts of Scipio API.
*
-->

<#-- 
*************
* Modal
************
Creates a modal UI element.

FIXME: Needs parameter to control injection and location of hidden modal content so can inject
    within forms

  * Usage Examples *  
    <@modal id="dsadsa" attr="" >
        Modal Content 
    </@modal>        
            
  * Parameters *
    id                      = (required) Model ID 
    label                   = (required) Anchor text
    icon                    = Generates icon inside the link 
                              NOTE: Has to be the full set of classes, e.g. "fa fa-fw fa-info"
    class                   = ((css-class)) CSS classes or additional classes for anchor
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)  
    href                    = Link
                              NOTE: This parameter is automatically (re-)escaped for HTML and javascript (using #escapeFullUrl or equivalent) 
                                  to help prevent injection, as it is high-risk. It accepts pre-escaped query string delimiters for compatibility,
                                  but other characters should not be manually escaped (apart from URL parameter encoding).
-->
<#assign modal_defaultArgs = {
  "id":"", "label":"", "href":"", "icon":"", "class":"", "passArgs":{}
}>
<#macro modal args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.modal_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local idNum = getRequestVar("scipioModalIdNum")!0>
  <#local idNum = idNum + 1 />
  <#local dummy = setRequestVar("scipioModalIdNum", idNum)>  
  <#if !id?has_content>
    <#local id = "modal_" + (renderSeqNumber!"")?string + "_" + idNum?string>
  </#if>
  <@modal_markup id=id label=label href=href class=class icon=icon origArgs=origArgs passArgs=passArgs><#nested></@modal_markup>
</#macro>

<#-- @modal main markup - theme override -->
<#macro modal_markup id="" label="" href="" class="" icon="" origArgs={} passArgs={} catchArgs...>
  <a href="#" data-reveal-id="modal_${escapePart(id, 'html')}"<#if href?has_content> data-reveal-ajax="${escapeFullUrl(href, 'html')}"</#if><@compiledClassAttribStr class=class />><#if icon?has_content><i class="${escapePart(icon, 'html')}"></i> </#if>${escapePart(label, 'html')}</a>
  <div id="modal_${escapePart(id, 'html')}" class="${styles.modal_wrap!}" data-reveal>
    <#nested>
    <a class="close-reveal-modal">&#215;</a>
  </div>
</#macro>

<#-- 
*************
* Alert box
************
Alert box for messages that must grab user attention.

NOTE: Should avoid using this for regular, common inlined message results such as "No records found." unless
    it's an unexpected result, error or one that requires user action.
    For most cases, it is preferrable to use @commonMsg macro because it is higher-level.

  * Usage Examples *  
    <@alert type="info">
        This is an alert!
    </@alert>            
                    
  * Parameters *
    type                    = (info|success|warning|secondary|alert|error), default info
    class                   = ((css-class)) CSS classes or additional classes for nested container
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    id                      = ID on innermost container
    style                   = Legacy HTML {{{style}}} attribute on innermost container
    closable                = ((boolean), default: -from global styles-, fallback default: true) Whether the alert should be hidable/closable
-->
<#assign alert_defaultArgs = {
  "type":"info", "class":"", "id":"", "style":"", "closable":"", 
  "containerClass":"", "containerId":"", "containerStyle":"", "passArgs":{}
}>
<#macro alert args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.alert_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local typeClass = "alert_type_" + rawString(type)/>
  <#if type == "error">
    <#local type = "alert">
  </#if>
  <#if !closable?is_boolean>
    <#local closable = styles["alert_" + type + "_closable"]!styles["alert_default_closable"]!true>
  </#if>
  <#if id?has_content && !containerId?has_content>
    <#local containerId = id + "_container">
  </#if>
  <@alert_markup type=type class=class typeClass=typeClass id=id style=style closable=closable 
    containerClass=containerClass containerId=containerId containerStyle=containerStyle
    origArgs=origArgs passArgs=passArgs><#nested></@alert_markup>
</#macro>

<#-- @alert main markup - theme override -->
<#macro alert_markup type="info" class="" typeClass="" id="" style="" closable=true containerId="" 
    containerClass="" containerStyle="" origArgs={} passArgs={} catchArgs...>
  <#local class = addClassArg(class, styles.grid_cell!"")>
  <#local class = addClassArgDefault(class, "${styles.grid_large!}12")>
  <#local containerClass = addClassArg(containerClass, styles.grid_row!)>
  <div<@compiledClassAttribStr class=containerClass /><#if containerId?has_content> id="${escapePart(containerId, 'html')}"</#if><#if containerStyle?has_content> style="${escapePart(containerStyle, 'html')}"</#if>>
    <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
      <div data-alert class="${styles.alert_wrap!} ${styles[rawString(typeClass)]!}">
        <div class="${styles.grid_row!}">
          <div<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapePart(id, 'html')}"</#if><#if style?has_content> style="${escapePart(style, 'html')}"</#if>>
            <#if closable><a href="#" class="${styles.closable!}" data-dismiss="alert">&times;</a></#if>
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
Creates a panel box.

  * Usage Examples *  
    <@panel type="">
        This is a panel.
    </@panel>            
                    
  * Parameters *
    type                    = (callout|, default: -empty-)
    title                   = Title
    id                      = ID for outermost container
    class                   = ((css-class)) CSS classes or additional classes for outermost container
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)
    style                   = Legacy HTML style attribute for outermost container (for compatibility)
    topContent              = ((directive)|(string)) Additional content directly inside panel container at top (before title)
                              NOTE: Not escaped by macro.
    topContentArgs          = ((map)) Additional args for topContent
    bottomContent           = ((directive)|(string)) Additional content directly inside panel container at bottom (after body)
                              NOTE: Not escaped by macro.
    bottomContentArgs       = ((map)) Additional args for bottomContent
-->
<#assign panel_defaultArgs = {
  "type":"", "title":"", "id":"", "class":"", "style":"", 
  "topContent":false, "topContentArgs":{}, "bottomContent":false, "bottomContentArgs":{},
  "passArgs":{}
}>
<#macro panel args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.panel_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@panel_markup type=type title=title id=id class=class style=style bottomContent=bottomContent
    origArgs=origArgs passArgs=passArgs><#nested></@panel_markup>
</#macro>

<#-- @panel main markup - theme override -->
<#macro panel_markup type="" title="" id="" class="" style="" 
    topContent=false topContentArgs={}  bottomContent=false bottomContentArgs={} origArgs={} passArgs={} catchArgs...>
  <#local class = addClassArg(class, styles.panel_wrap!"")>
  <#local class = addClassArg(class, type)>
  <div<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapePart(id, 'html')}"</#if><#if style?has_content> style="${escapePart(style, 'html')}"</#if>>
    <#if !topContent?is_boolean><@contentArgRender content=topContent args=topContentArgs /></#if>
    <div<@compiledClassAttribStr class=(styles.panel_head!)/>><#if title?has_content><h5 class="${styles.panel_title!}">${escapePart(title, 'html')}</h5></#if></div>
    <div<@compiledClassAttribStr class=(styles.panel_body!)/>><#nested></div>
    <#if !bottomContent?is_boolean><@contentArgRender content=bottomContent args=bottomContentArgs /></#if>
  </div>
</#macro>

<#-- 
*************
* Common messages (abstraction)
************
Abstracts and factors out the display format of common messages of specific meanings, such as
errors (e.g. permission errors) and query results (e.g. "No records found." messages).

This labels messages according to what they are and lets the theme decide on markup and styling based on content.

This is higher-level than @alert macro; @alert has a specific markup/display and its types are usually levels of severity,
whereas @commonMsg abstracts markup/display and its messages can be a combination of levels and specific meanings;
@alert's goal is to grab user attention, whereas @commonMsg's behavior depends on the type of message.

@commonMsg may use @alert to implement its markup.
A template should not assume too much about the message markup, but the markup should be kept simple.

  * Usage from screen widgets *

<label.../> elements in screen widgets can be mapped to this macro using the special "common-msg-xxx" style name, where
xxx is the message type. e.g.: 
  <label style="common-msg-error-perm" text="Permission Error" />
translates to:
  <@commonMsg type="error-perm">Permission Error</@commonMsg>
Extra classes are also possible using colon syntax (combined with the usual "+" macro additive class instruction):
  <label style="common-msg-error-perm:+myclass" text="Permission Error" />
translates to:
  <@commonMsg type="error-perm" class="+myclass">Permission Error</@commonMsg>

  * Usage Examples *  
    <@commonMsg type="result-norecord"/>            

  * Parameters *
    type                    = (default|generic|..., default: default) The type of message contained
                              Basic types:
                              * {{{default}}}: default. in standard Scipio markup, same as generic.
                              * {{{generic}}}: no type specified (avoid using - prefer more specific)
                              Types recognizes by Scipio standard markup (theme-defined types are possible):
                              * {{{result}}}: an informational result from any kind of query. e.g., "No records found.".
                                    is a normal event that shouldn't distract user attention.
                              * {{{result-norecord}}}: specific "No records found." message.
                              * {{{info}}}: general information (NOTE: this is not really useful, but supported for completeness)
                              * {{{info-important}}}: general important information
                              * {{{warning}}}: general warning
                              * {{{fail}}}: general non-fatal error
                              * {{{error}}}: general error message - typically an unexpected event or fatal error that should not happen in intended use.
                              * {{{error-perm}}}: permission error
                              * {{{error-security}}}: security error
    id                      = ID on innermost container
    class                   = ((css-class)) CSS classes or additional classes for message container (innermost containing element)
                              Supports prefixes (see #compileClassArg for more info):
                              * {{{+}}}: causes the classes to append only, never replace defaults (same logic as empty string "")
                              * {{{=}}}: causes the classes to replace non-essential defaults (same as specifying a class name directly)  
    text                    = ((string)) Text
                              If a string is not specified, uses nested content instead.
    closable                = ((boolean), default: -from global styles-) Whether the message should be closable
                              NOTE: This is not implemented for all types and defaults vary, but if unsure and need to prevent closing, best to specify explicit false.
-->
<#assign commonMsg_defaultArgs = {
  "type":"", "class":"", "id":"", "text":true, "closable":"", "passArgs":{}
}>
<#macro commonMsg args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.commonMsg_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !type?has_content>
    <#local type = "default">
  </#if>
  <#local styleNamePrefix = "commonmsg_" + type?replace("-", "_")>
  <#local defaultClass = styles[styleNamePrefix]!styles["commonmsg_default"]>
  <#local class = addClassArgDefault(class, defaultClass)>
  <@commonMsg_markup type=type styleNamePrefix=styleNamePrefix class=class id=id closable=closable origArgs=origArgs passArgs=passArgs><#if !text?is_boolean>${escapePart(text, 'html')}<#elseif !(text?is_boolean && text == false)><#nested></#if></@commonMsg_markup>
</#macro>

<#-- @commonMsg main markup - theme override -->
<#macro commonMsg_markup type="" styleNamePrefix="" class="" id="" closable="" origArgs={} passArgs={} catchArgs...>
  <#if type == "result" || type?starts_with("result-") || type == "info">
    <#local nestedContent><#nested></#local>
    <#local nestedContent = nestedContent?trim><#-- helpful in widgets -->
    <#if !nestedContent?has_content>
      <#local nestedContent = uiLabelMap[styles.commonmsg_result_norecord_prop!"CommonNoRecordFoundSentence"]>
    </#if>
    <p<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapePart(id, 'html')}"</#if>>${nestedContent}</p>
  <#elseif type == "error" || type?starts_with("error-")>
    <#-- NOTE: here we must resolve class arg before passing to @alert and make additive only -->
    <@alert type="error" class=("+"+compileClassArg(class)) id=id closable=closable><#nested></@alert>
  <#elseif type == "fail">
    <@alert type="error" class=("+"+compileClassArg(class)) id=id closable=closable><#nested></@alert>
  <#elseif type == "warning">
    <@alert type="warning" class=("+"+compileClassArg(class)) id=id closable=closable><#nested></@alert>
  <#elseif type == "info-important">
    <@alert type="info" class=("+"+compileClassArg(class)) id=id closable=closable><#nested></@alert>
  <#else>
    <p<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapePart(id, 'html')}"</#if>><#nested></p>
  </#if>
</#macro>



