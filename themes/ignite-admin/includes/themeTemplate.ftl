<#--
* 
* A set of HTML templating macros, part of standard Scipio Freemarker API.
* Automatically included at all times.
* Intended to be swappable.
*
* Overrides the default SCIPIO styles located in 
* htmlTemplate.ftl - ofbiz_foundation/framework/common/webcommon/scipio/lib/standard/
* 
-->

<#-- Master include: Includes the default macros and allows overrides -->
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl"> 
<#-- The following is now done by the include above instead, because the standard macros need it as well.
    It creates a map of references to the original macros so we can delegate to them, essentially a namespace.
    NOTE: this overriding file creates a namespace for itself as well - scipioBsTmplLib (see end of this file).
<#assign scipioStdTmplLib = copyObject(.namespace)> -->

<#--
Other possible patterns:

<#import "component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl" as scipioStdTmplLib> 

DEV NOTE: it turns out, using #import statement here as-is is too problematic. within the import calls, local macro definitions will always
shadow the global macro definitions, which means they won't automatically use the overridden macros defined in this parent file, 
so reuse of high-level macros (that call others) becomes confusing.
whether automatic overriding is wanted or not is case-specific, but in general, here yes.
the only way around this would be to modify the namespace using:
<#import ... as scipioStdTmplLib>
<#macro row>
</#macro>
<#assign row = row in scipioStdTmplLib>
for every major macro (which could be approximated with looped assignments again, but ugly).
note this pattern is acceptable in some other cases because it can be very fine-grained, but here will probably cause only headaches
because in general we wish to override selectively, not include selectively.
-->

<#-- 
*************
* MACRO OVERRIDES
************
See component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl for documentation about
the macro interfaces used in the standard macros and these overrides.

NOTES:
  * It's important that any overrides for template-facing macros (like @field or other) be enabled with and
    use the advanced args pattern (args={} inlineArgs...). Without this, standard scipio macro interface
    changes - almost always adding an attribute - will break compatibility for any themes not controlled by Ilscipio.
    Using this pattern, any extra attributes added in standard interfaces will either be gracefully ignored by the 
    overriding macros (if total new substitute implementation) or automatically pass along the new attributes 
    to the delegated macro (if is a delegating override).
  * The markup macros are not subject to the previous point because they are not template-facing; however
    they all MUST accept an "catchArgs..." (varargs) so they are not subject to compatibility breaking.
 -->
 
 <#-- General Overrides -->
<#macro field_datefind_widget args={} inlineArgs...>
    <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.field_datefind_widget_defaultArgs)>
    <#local dummy = localsPutAll(args)>
    <#local origArgs = args>
    <@scipioStdTmplLib.field_datefind_widget inlinePostfix=true args=args />
</#macro>

<#macro field_datetime_widget args={} inlineArgs...>
    <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.field_datetime_widget_defaultArgs)>
    <#local dummy = localsPutAll(args)>
    <#local origArgs = args>
    <@scipioStdTmplLib.field_datetime_widget inlinePostfix=true args=args />
</#macro>

<#-- @field container markup - theme override 
    #nested is the actual field widget (<input>, <select>, etc.). -->
<#macro field_markup_container type="" fieldsType="" defaultGridArgs={} gridArgs={} 
    postfix=false postfixContent=true labelArea=true labelType="" labelPosition="" labelAreaContent="" collapse="" 
    collapseLabel="" collapsePostfix="" norows=false nocells=false container=true containerId="" containerClass="" containerStyle=""
    preWidgetContent=false postWidgetContent=false preLabelContent=false postLabelContent=false prePostfixContent=false postPostfixContent=false
    labelAreaContentArgs={} postfixContentArgs={} prePostContentArgs={}
    widgetAreaClass="" labelAreaClass="" postfixAreaClass="" widgetPostfixAreaClass="" inverted=false labelSmallDiffColumns=""
    origArgs={} passArgs={} required=false noLabelCell=false noInputCell=false catchArgs...>
  <#local containerClass = addClassArg(containerClass, "form-group ")>
  <#if labelArea && labelPosition == "top">
    <#local nocells = true>
  </#if>
  <#--
  <#local labelAreaClass = addClassArg(labelAreaClass, "control-label")>
  <#local widgetAreaClass = addClassArg(widgetAreaClass, "form-control")>-->

        
  <@scipioStdTmplLib.field_markup_container type=type fieldsType=fieldsType defaultGridArgs=defaultGridArgs gridArgs=gridArgs postfix=postfix  
    postfixContent=postfixContent labelArea=labelArea labelType=labelType labelPosition=labelPosition labelAreaContent=labelAreaContent 
    collapse=collapse collapseLabel=collapseLabel collapsePostfix=collapsePostfix norows=norows nocells=nocells container=container containerId=containerId containerClass=containerClass containerStyle=containerStyle
    preWidgetContent=preWidgetContent postWidgetContent=postWidgetContent preLabelContent=preLabelContent postLabelContent=postLabelContent prePostfixContent=prePostfixContent postPostfixContent=postPostfixContent
    labelAreaContentArgs=labelAreaContentArgs postfixContentArgs=postfixContentArgs prePostContentArgs=prePostContentArgs
    widgetAreaClass=widgetAreaClass labelAreaClass=labelAreaClass postfixAreaClass=postfixAreaClass widgetPostfixAreaClass=widgetPostfixAreaClass
    inverted=inverted labelSmallDiffColumns=labelSmallDiffColumns origArgs=origArgs passArgs=passArgs required=required noLabelCell=noLabelCell noInputCell=noInputCell><#nested></@scipioStdTmplLib.field_markup_container>

</#macro>

<#-- @field_lookup_widget - theme override-->
<#macro field_lookup_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.field_lookup_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !ajaxEnabled?is_boolean>
    <#if ajaxEnabled?has_content>
      <#local ajaxEnabled = ajaxEnabled?boolean>
    <#else>
      <#local ajaxEnabled = javaScriptEnabled!true>
    </#if>
  </#if>
  <@scipioStdTmplLib.field_lookup_markup_widget name=name formName=formName fieldFormName=fieldFormName class=class style=style alert=alert value=value size=size 
    maxlength=maxlength id=id events=events readonly=readonly autocomplete=autocomplete descriptionFieldName=descriptionFieldName 
    targetParameterIter=targetParameterIter imgSrc=imgSrc ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled presentation=presentation width=width 
    height=height position=position fadeBackground=fadeBackground clearText=clearText showDescription=showDescription initiallyCollapsed=initiallyCollapsed 
    lastViewName=lastViewName title=title fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel inlinePostfix=true tooltip=tooltip 
    required=required attribs=toSimpleMap(attribs) origArgs=origArgs passArgs=passArgs><#nested></@scipioStdTmplLib.field_lookup_markup_widget>
</#macro>

<#-- @field label area markup - theme override -->
<#macro field_markup_labelarea labelType="" labelPosition="" label="" labelContent=false labelDetail=false fieldType="" fieldsType="" fieldId="" collapse="" 
    required=false labelContentArgs={} norows=false nocells=false container=true origArgs={} passArgs={} catchArgs...>
  <#-- the label must be escaped by default. caller can prevent using #wrapAsRaw
  <#local label = label?trim>-->
  <#local label = escapeVal(label, 'htmlmarkup')?trim>
  <#if !labelContent?is_boolean>
    <@contentArgRender content=labelContent args=labelContentArgs doTrim=true />
    <#-- don't show this here, let macro handle it
    <#if required>*</#if>-->
  <#elseif label?has_content>
      <label class="control-label  col-form-label"<#if fieldId?has_content> for="${escapeVal(fieldId, 'html')}"</#if>>${label}<#if required> *</#if></label>
  <#-- only show this if there's a label, otherwise affects inline fields too in ugly way, and there are other indications anyhow
  <#else>
    <#if required>*</#if>-->
  </#if> 
  <#if !labelDetail?is_boolean><@contentArgRender content=labelDetail args=labelContentArgs doTrim=true /></#if>
  <#-- This was nbsp to prevent collapsing empty cells in foundation, now replaced by a CSS hack (see _base.scss)
  <#if container && !nocells>
    <#if !label?has_content && labelDetail?is_boolean && labelContent?is_boolean>
      &nbsp;
    </#if>
  </#if>-->
</#macro>

<#assign field_datetime_disptypefmts = {
    "timestamp": "YYYY-MM-DD HH:mm:ss.SSS",
    "date": "YYYY-MM-DD",
    "month": "YYYY-MM",
    "time": "HH:mm:ss.SSS"
}>

<#-- Override of @field_datetime_markup_script since fdatetime is foundation specific -->
<#macro field_datetime_markup_script inputId="" inputName="" displayInputId="" displayInputName="" dateType="" dateDisplayType="" htmlwrap=true origArgs={} passArgs={} catchArgs...>
  <#-- Display behavior flags -->
  <#-- WARN: 2018-03-08: bootstrap-datetimepicker events are not reliable enough for displayCorrect==true at this time -->
  <#local displayCorrect = false><#-- if true, display input is re-assigned value on every change; if false, lets datepicker choose -->
  <#local useFillDate = false && displayCorrect><#-- if true, the digits that picker can't set are preserved from last value - only works for simple dateDisplayConvFmt (YYYY-MM-DD) -->
  
  <#local dateConvFmt = field_datetime_typefmts[dateType]!><#-- see htmlFormFieldWidgets.ftl -->
  <#local dateDisplayConvFmt = field_datetime_disptypefmts[dateDisplayType]!>
  <#-- ignite-admin: show full internal format by default, like metro/base -->
  <#-- Effective display format for when displayCorrect==true (bypass for picker display format)
      (field_datetime_disptypefmts: friendly; field_datetime_typefmts: internal; custom hash possible) -->
  <#local dateEffDispConvFmt = field_datetime_typefmts[dateDisplayType]!>
  
  <#local displayInputIdJs = escapeVal(displayInputId, 'js')>
  <#local inputIdJs = escapeVal(inputId, 'js')>
  
  <#local keepInvalid = false>
  <#if displayCorrect && (dateConvFmt != dateEffDispConvFmt)>
    <#local extraFormats>["${dateConvFmt}", "${dateEffDispConvFmt}"]</#local>
  <#else>
    <#local extraFormats>["${dateConvFmt}"]</#local>
  </#if>
  <#if dateDisplayType == "timestamp">
    <#local datepickerOptions>{format:"${dateDisplayConvFmt}", extraFormats:${extraFormats}, keepInvalid:${keepInvalid?string},
                sideBySide:true,
                toolbarPlacement: 'bottom',
                showTodayButton:true,
                icons: {
                    time: "fa fa-clock-o",
                    date: "fa fa-calendar",
                    up: "fa fa-arrow-up",
                    down: "fa fa-arrow-down",
                    previous: "fa fa-arrow-left",
                    next: "fa fa-arrow-right",
                    today: "fa fa-hourglass-half",
                    clear: "fa fa-trash",
                    close: "fa fa-times"
                }}</#local>
  <#elseif dateDisplayType == "date">
    <#local datepickerOptions>{format:"${dateDisplayConvFmt}", extraFormats:${extraFormats}, keepInvalid:${keepInvalid?string},
                showTodayButton:true,
                icons: {
                    time: "fa fa-clock-o",
                    date: "fa fa-calendar",
                    up: "fa fa-arrow-up",
                    down: "fa fa-arrow-down",
                    previous: "fa fa-arrow-left",
                    next: "fa fa-arrow-right",
                    today: "fa fa-hourglass-half",
                    clear: "fa fa-trash",
                    close: "fa fa-times"
                }}</#local>
  <#elseif dateDisplayType == "month">
    <#local datepickerOptions>{format:"${dateDisplayConvFmt}", extraFormats:${extraFormats}, keepInvalid:${keepInvalid?string},
                showTodayButton:true,
                icons: {
                    time: "fa fa-clock-o",
                    date: "fa fa-calendar",
                    up: "fa fa-arrow-up",
                    down: "fa fa-arrow-down",
                    previous: "fa fa-arrow-left",
                    next: "fa fa-arrow-right",
                    today: "fa fa-hourglass-half",
                    clear: "fa fa-trash",
                    close: "fa fa-times"
                }}</#local>
  <#elseif dateDisplayType == "time">
    <#local datepickerOptions>{format:"${dateDisplayConvFmt}", extraFormats:${extraFormats}, keepInvalid:${keepInvalid?string},
                showTodayButton:true,
                icons: {
                    time: "fa fa-clock-o",
                    date: "fa fa-calendar",
                    up: "fa fa-arrow-up",
                    down: "fa fa-arrow-down",
                    previous: "fa fa-arrow-left",
                    next: "fa fa-arrow-right",
                    today: "fa fa-hourglass-half",
                    clear: "fa fa-trash",
                    close: "fa fa-times"
                }}</#local>
  </#if>
  <@script htmlwrap=htmlwrap>
    $(function() {
        var sfdh = new ScpFieldDateHelper({ <#-- see selectall.js -->
            displayInputId: "${displayInputIdJs}",
            inputId: "${inputIdJs}",
            displayCorrect: ${displayCorrect?string},
            useFillDate: ${useFillDate?string},
            dateFmt: "${dateConvFmt}",
            dateDisplayFmt: "${dateDisplayConvFmt}",
            dateEffDispFmt: "${dateEffDispConvFmt}"
        });
    
        var dtpElem;
        var dtpInst = null;
        var getDtp = function() {
            if (dtpInst != null) return dtpInst;
            dtpInst = dtpElem.data('DateTimePicker');
            return dtpInst;
        };
        <#-- Workaround: bootstrap-datetimepicker does not allow access to picker during
            events invoked during constructor (.data('DateTimePicker') returns null), 
            so we delay-exec the refreshDateInputs calls made during init -->
        var rdiNeeded = null;
        
        var refreshDateInputs = function(skipNormUp) {
            if (sfdh.opts.displayCorrect || skipNormUp !== true) { <#-- optimization for some cases -->
                var dtp = getDtp();
                if (dtp == null) {
                    <#-- flag the update; if previous skipNormUp true, we may get priority -->
                    if (rdiNeeded == null || rdiNeeded.skipNormUp === true) {
                        rdiNeeded = {skipNormUp:skipNormUp};
                    }
                } else {
                    var date = dtp.date() || ''; <#-- NOTE: must be empty string; null treated as error -->
                    sfdh.updateAllDateInputs({date: date, oldDate: '', skipNormUp:skipNormUp});
                }
            }
        };
        
        var onDatePopup = function(ev) {
            <#-- NOTE: old date makes no difference as long as useFillDate==false (bdp doesn't need) -->
            sfdh.saveOldDateFromI18n(); 
            refreshDateInputs(true);<#-- skip hidden input update, was not changed here -->
        };
        
        var onDateChange = function(ev) { <#-- ONLY for use with dp.change -->
            <#-- NOTE: oldDate makes no difference as long as useFillDate==false -->
            sfdh.updateAllDateInputs({date: ev.date, oldDate: ev.oldDate || ''});
        };
        
        var onDateError = function(ev) {
            <#-- here we ignore ev.date/ev.oldDate, because we just want to match what the picker
                decided to keep as its value -->
            refreshDateInputs(false);
        };
        
        <#-- Important: hook events before .datetimepicker because they are triggered during the call -->
        var dtpElem = $("#${displayInputIdJs}");
        dtpElem.on('dp.change', onDateChange).on('dp.error', onDateError).on('dp.show', onDatePopup).datetimepicker(${datepickerOptions});

        <#-- run original change events -->
        dtpElem.on('dp.change',function (e) {
          $(e.target).trigger('change');
          });


        <#-- Run any triggered refresh requests now -->
        if (rdiNeeded != null) {
            refreshDateInputs(rdiNeeded.skipNormUp);
        }
    });
  </@script>
</#macro>

<#-- @menu template-facing macro - theme override
    NOTE: the more "proper" way to modify these is now to override the @menu_markup and @menuitem_markup macros, but
        these are acceptable as well (because of args/inlineArgs pattern) and provides more examples of ways to override. 
    2016-02-22: Comment this completely because the htmlwraps are now set in styles hash, per-menu-type
<#assign menu_defaultArgs_min = {"htmlwrap":"div", "passArgs":{}}> <#- change the default value, but still possible for client to override ->
<#assign menu_defaultArgs = getScipioMacroDefaultArgs("menu", scipioStdTmplLib) + menu_defaultArgs_min>
<#macro menu args={} inlineArgs...>
  <@scipioStdTmplLib.menu args=mergeArgMaps(args, inlineArgs, scipioBsTmplLib.menu_defaultArgs_min)><#nested /></@scipioStdTmplLib.menu>
</#macro>-->

<#-- @menuitem template-facing macro - theme override
<#assign menuitem_defaultArgs_min = {"htmlwrap":false, "passArgs":{}}> <#- no html wrapper by default ->
<#assign menuitem_defaultArgs = getScipioMacroDefaultArgs("menuitem", scipioStdTmplLib) + menuitem_defaultArgs_min>
<#macro menuitem args={} inlineArgs...>
  <@scipioStdTmplLib.menuitem args=mergeArgMaps(args, inlineArgs, scipioBsTmplLib.menuitem_defaultArgs_min)><#nested /></@scipioStdTmplLib.menuitem>
</#macro>-->
<#assign modal_defaultArgs = {
  "id":"", "label":"", "href":"", "icon":"", "linkClass":"","class":"", "title":"", "footer":"", "passArgs":{}
}>
<#macro modal args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.modal_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#local idNum = getRequestVar("scipioModalIdNum")!0>
  <#local idNum = idNum + 1 />
  <#local dummy = setRequestVar("scipioModalIdNum", idNum)>
  <#if !id?has_content>
    <#local id = (renderSeqNumber!"")?string>
    <#if id?has_content>
      <#local id = "_" + idNum?string>
    <#else>
      <#local id = idNum?string>
    </#if>
  </#if>
  <#local linkHasContent = icon?has_content || label?has_content>
  <#local modalId = "modal_" + id>
  <#local linkId = "modal_link_" + id>
  <@modal_markup id=id modalId=modalId linkId=linkId label=label href=href class=class icon=icon linkClass=linkClass linkHasContent=linkHasContent title=title footer=footer origArgs=origArgs passArgs=passArgs><#nested></@modal_markup>
</#macro>



<#-- @modal main markup - theme override -->
<#macro modal_markup id="" modalId="" linkId="" label="" href="" class="" icon="" title="" footer="" linkClass="" origArgs={} passArgs={} catchArgs...>
  <#local class = addClassArg(class, "modal-dialog")>
  <a href="${escapeFullUrl(href, 'html')}"<@compiledClassAttribStr class=linkClass/><#if linkId?has_content> id="${linkId}"</#if> data-toggle="modal" data-target="#${escapeVal(modalId, 'html')}"><#if icon?has_content><i class="${escapeVal(icon, 'html')}"></i> </#if>${escapeVal(label, 'htmlmarkup')}</a>
  <div id="${escapeVal(modalId, 'html')}" role="dialog" aria-hidden="true" class="${styles.modal_wrap!""}">
        <div<@compiledClassAttribStr class=class/>>
        <#-- Modal content-->
            <div class="modal-content">
                  <div class="modal-header">
                    <h5 class="modal-title">${title!""}</h5>
                    <button type="button" class="close" data-dismiss="modal" aria-label="Close">&times;</button>
                  </div>
                  <div class="modal-body">
                    <#nested>
                  </div>
                  <div class="modal-footer">
                    ${footer!""}
                  </div>
            </div>
        </div>
  </div>
</#macro>

<#function modalControl element type="open">
    <#local call="">
    <#switch type>
        <#case "close">
        <#local call>${element!""}.modal('hide');</#local>
        <#break>
        <#case "open">
        <#default>
        <#local call>${element!""}.modal('show');</#local>
        <#break>
    </#switch>
    <#return call/>
</#function>

<#-- @slider main markup - theme override -->
<#macro slider_markup title="" id="" sliderIdNum=0 class="" library="" controls=true indicator=true
jsOptions="" origArgs={} passArgs={} catchArgs...>
    <#if langDir?has_content && langDir=="rtl"><#local jsOptions="rtl:true,"+jsOptions/></#if>
    <#if title?has_content><@heading>${escapeVal(title, 'htmlmarkup')}</@heading></#if>
    <#switch library>
        <#case "owl">
        <#local class = addClassArg(class, "owl-carousel")>
        <div<@compiledClassAttribStr class=class /> id="${escapeVal(id, 'html')}">
                <#nested/>
        </div>
        <script type="text/javascript">
            $(document).ready(function(){
        $("#${escapeVal(id, 'js')}").owlCarousel({${jsOptions}});
                });
            </script>
        <#break>
        <#case "slick">
        <div class="slider-parent">
                <div<@compiledClassAttribStr class=class /> id="${escapeVal(id, 'html')}">
                    <#nested/>
        </div>
        </div>
        <script type="text/javascript">
            $(document).ready(function(){
        $("#${escapeVal(id, 'js')}").slick({${jsOptions}});
                });
            </script>
        <#break>
        <#default>
        <#local class = addClassArg(class, styles.slider_container!)>
        <div class="${styles.slider_container!}" data-ride="carousel" data-slide-to="0" id="${escapeVal(id, 'html')}">
              <#local sliderLength = getRequestVar("scipioSliderLength")!0>
        <#if indicator>
            <ol class="carousel-indicators">
                        <#list 1..sliderLength as x>
            <li data-target="#${escapeVal(id, 'html')}" data-slide-to="${x}"<#if x==1> class="active"</#if>></li>
        </#list>
            </ol>
        </#if>
        <div class="${styles.slider_wrap!}" role="listbox">
                <#nested/>
        </div>
        <#if controls>
            <a class="carousel-control-prev" href="#${escapeVal(id, 'html')}" role="button" data-slide="prev">
                        <span class="carousel-control-prev-icon" aria-hidden="true"></span>
            <span class="sr-only">Previous</span>
            </a>
            <a class="carousel-control-next" href="#${escapeVal(id, 'html')}" role="button" data-slide="next">
                        <span class="carousel-control-next-icon" aria-hidden="true"></span>
            <span class="sr-only">Next</span>
            </a>
        </#if>
        </div>
    </#switch>
</#macro>

<#-- @slide main markup - theme override -->
<#macro slide_markup id="" sliderId="" class="" library="" image="" link="" linkTarget="" title="" slideIdNum=0 sliderLength=1 renderSeqNumber="" origArgs={} passArgs={} catchArgs...>
    <#if library=="owl" || library=="slick">
        <div id="${escapeVal(id, 'html')}" class="item">
            <#if link?has_content><a href="${escapeFullUrl(link, 'html')}"<#if linkTarget?has_content> target="${escapeVal(linkTarget, 'html')}"</#if>></#if>
        <div>
        <#if title?has_content><h2>${escapeVal(title, 'htmlmarkup')}</h2></#if>
        <#if image?has_content>
            <img src="${escapeFullUrl(image, 'html')}"  class="${styles.slide_image!}"/>
            </#if>
        <#local nestedContent><#nested></#local>
        <#if nestedContent?has_content><div class="">${nestedContent}</div></#if>
        </div>
        <#if link?has_content></a></#if>
        </div>
    <#else>
        <div data-orbit-slide="${escapeVal(id, 'html')}" class="${styles.slide_container!} <#if slideIdNum==1>active</#if>">
            <#if link?has_content><a href="${escapeFullUrl(link, 'html')}"<#if linkTarget?has_content> target="${escapeVal(linkTarget, 'html')}"</#if>></#if>
        <#if title?has_content><h2>${escapeVal(title, 'htmlmarkup')}</h2></#if>
        <#if image?has_content>
            <img src="${escapeFullUrl(image, 'html')}" class="${styles.slide_image!}"/>
              </#if>
        <#local nestedContent><#nested></#local>
        <#if nestedContent?has_content><div class="${styles.slide_content!}">${nestedContent}</div></#if>
        <#if link?has_content></a></#if>
        </div>
    </#if>
</#macro>

<#-- @menu container main markup - theme override 
    DEV NOTE: This is called directly from both @menu and widgets @renderMenuFull -->
<#macro menu_markup type="" specialType="" class="" id="" style="" attribs={} excludeAttribs=[] 
    inlineItems=false titleClass="" title="" htmlwrap="ul" isNestedMenu=false parentMenuType="" parentMenuSpecialType=""
    active=false activeTarget="" menuLevel=1 sepMenu={}
    origArgs={} passArgs={} catchArgs...>
  <#if !inlineItems && htmlwrap?has_content && specialType == "main">
      <#-- WARN: isNestedMenu check here would not be logical -->
      <li class="${styles.menu_main_wrap!}"><a href="#" class="${styles.menu_main_item_link!}"
        <#if (styles.framework!"") == "bootstrap"> data-toggle="dropdown"</#if>>${escapeVal(title, 'htmlmarkup')} <#if (styles.framework!"") == "bootstrap"> <i class="fa fa-fw fa-caret-down"></i></#if></a>
  <#elseif !inlineItems && htmlwrap?has_content && specialType == "button-dropdown">
      <div class="${styles.button_group!}">
          <div class="dropdown">
          <button href="#" data-dropdown="${escapeVal(id, 'html')}" aria-controls="${escapeVal(id, 'html')}" data-toggle="dropdown"aria-expanded="false"<@compiledClassAttribStr class=titleClass />>${escapeVal(title, 'htmlmarkup')} <span class="caret"></span></button><br>
          <#local attribs = attribs + {"data-dropdown-content":"true", "aria-hidden":"true"}>
          <#if htmlwrap?has_content><${htmlwrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
          <#nested>
          <#if htmlwrap?has_content></${htmlwrap}></#if>
          </div>
      </div>
  <#else>
      <@scipioStdTmplLib.menu_markup type=type specialType=specialType class=class id=id style=style attribs=attribs excludeAttribs=excludeAttribs
        inlineItems=inlineItems titleClass=titleClass title=title htmlwrap=htmlwrap isNestedMenu=isNestedMenu parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType
        active=active activeTarget=activeTarget menuLevel=menuLevel sepMenu=sepMenu
        origArgs=origArgs passArgs=passArgs><#nested></@scipioStdTmplLib.menu_markup>
  </#if>
</#macro>


<#-- @menuitem container markup - theme override 
  DEV NOTE: This is called directly from both @menuitem and widgets @renderMenuItemFull 
<#macro menuitem_markup type="" menuType="" menuSpecialType="" name="" menuName="" class="" id="" style="" attribs={} 
    excludeAttribs=[] inlineItem=false htmlwrap=false disabled=false selected=false active=false activeTarget=""
    isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0 origArgs={} passArgs={} catchArgs...>
    <#if menuSpecialType!="sidebar">
        <@scipioStdTmplLib.menuitem_markup type=type menuType=menuType menuSpecialType=menuSpecialType name=name menuName=menuName class=class id=id style=style attribs=attribs
        excludeAttribs=excludeAttribs inlineItem=inlineItem htmlwrap="" disabled=disabled selected=selected active=active activeTarget=activeTarget
        isNestedMenu=isNestedMenu menuLevel=menuLevel parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType itemIndex=itemIndex
            origArgs=origArgs passArgs=passArgs><#nested></@scipioStdTmplLib.menuitem_markup>
    <#else>
        <@scipioStdTmplLib.menuitem_markup type=type menuType=menuType menuSpecialType=menuSpecialType name=name menuName=menuName class=class id=id style=style attribs=attribs
        excludeAttribs=excludeAttribs inlineItem=inlineItem htmlwrap=htmlwrap disabled=disabled selected=selected active=active activeTarget=activeTarget
        isNestedMenu=isNestedMenu menuLevel=menuLevel parentMenuType=parentMenuType parentMenuSpecialType=parentMenuSpecialType itemIndex=itemIndex
            origArgs=origArgs passArgs=passArgs><#nested></@scipioStdTmplLib.menuitem_markup>

    </#if>
</#macro>-->


<#-- @nav main markup - theme override -->
<#macro nav_markup type="" id="" class="" style="" activeElem="" origArgs={} passArgs={} catchArgs...>
  <#switch type>
    <#case "magellan">
      <nav class="navbar navbar-default navbar-static" data-spy="scroll">
        <div class="container-fluid">
          <ul class="nav nav-tabs" role="tablist">
            <#nested>
          </ul>
        </div>
      </nav>
    <#break>
    <#case "breadcrumbs">
      <ul class="${styles.nav_breadcrumbs!}">
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
      <ul class="${styles.list_inline!} ${styles.nav_subnav!}">
        <#nested>
      </ul>
    <#break>
  </#switch>
</#macro>


<#-- @menuitem type="link" markup - theme override
<#macro menuitem_link_markup itemType="" menuType="" menuSpecialType="" itemName="" menuName="" class="" id="" style="" href="" name="" onClick="" target="" title=""
attribs={} excludeAttribs=[] disabled=false selected=false active=false activeTarget="" isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0
origArgs={} passArgs={} catchArgs...>
    <#t><#if active><#local class = addClassArg(class, "menu_sidebar_item_contentactive")></#if>
    <#t><a href="${escapeFullUrl(href, 'html')}"<#if onClick?has_content> onclick="${escapeVal(onClick, 'html')}"</#if><@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if name?has_content> name="${escapeVal(name, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if target?has_content> target="${escapeVal(target, 'html')}"</#if><#if title?has_content> title="${escapeVal(title, 'html')}"</#if>><#nested></a>
    <#if styles.app_icon[href]?has_content><i class="${styles.icon!} ${styles.app_icon[display.name]}"></i> </#if><#t>
</#macro>-->

<#-- @mli main markup - theme override -->
<#macro mli_markup arrival="" origArgs={} passArgs={} catchArgs...>
  <li><#nested></li>
</#macro>

<#-- since bootstrap doesn't use <li>, this check must be adjusted to something else... -->
<#function isMenuMarkupItemsInline menuContent>
  <#return menuContent?matches(r'(\s*<!--((?!<!--).)*?-->\s*)*\s*<(li|a|span|button|input)(\s|>).*', 'rs')>
</#function>


<#-- @pul main markup - theme override -->
<#macro pul_markup title="" origArgs={} passArgs={} catchArgs...>
  <div class="${styles.pricing_wrap!}">
    <#if title?has_content><@pli type="title">${escapeVal(title, 'htmlmarkup')}</@pli></#if>
    <div class="card-block">
        <#nested>
    </div>
  </div>
</#macro>

<#-- @pli main markup - theme override -->
<#macro pli_markup type="" class="" origArgs={} passArgs={} catchArgs...>
  <#switch type>
    <#case "price">
      <#local class = addClassArg(class, styles.pricing_price!)>
      <div <@compiledClassAttribStr class=class />><#nested></div>
    <#break>
    <#case "description">
      <#local class = addClassArg(class, styles.pricing_description!)>
      <div <@compiledClassAttribStr class=class />><#nested></div>
    <#break>
    <#case "title">
      <#local class = addClassArg(class, styles.pricing_title!)>
      <div <@compiledClassAttribStr class=class />><#nested></div>
    <#break>
    <#case "button">
      <#local class = addClassArg(class, styles.pricing_cta!)>
      <div <@compiledClassAttribStr class=class />><#nested></div>
    <#break>        
    <#default>
      <#local class = addClassArg(class, styles.pricing_bullet!)>
      <div <@compiledClassAttribStr class=class />><#nested></div>
    <#break>
  </#switch>
</#macro>

<#-- @chart main markup - theme override -->
<#macro chart_markup type="" chartLibrary="" title="" id="" xlabel="" ylabel="" label1="" label2="" labelUom1="" labelUom2="" chartIdNum=0 renderSeqNumber=0 origArgs={} passArgs={} catchArgs...>
  <#-- WARN/FIXME?: ids and type are not escaped, currently assumed to come from internal only... -->
  <#local nestedContent><#nested /></#local>
  <#if chartLibrary=="foundation">
    <#if nestedContent?has_content>
    <@row>
      <@cell columns=3>
        <ul data-${escapeVal(type, 'html')}-id="${escapeVal(id, 'html')}" class="${styles.chart_legend!}">
            <#nested/>
        </ul>
      </@cell>
      <@cell columns=9><div id="${escapeVal(id, 'html')}" style="height:300px;"></div></@cell>
    </@row>
    <#else>
        <#-- Default to chart.js chart for now, as this is capable of rendering an empty chart -->
        <@chart type=type library="chart" xlabel=xlabel ylabel=ylabel label1=label1 label2=label2>
            <#nested>
        </@chart>
    </#if>
  <#else>
    <#-- Get the number of datasets by inspecting the nested content (chartjs addData function values) -->
    <#if nestedContent?has_content>
        <#assign chartDatasets=chart_get_number_of_datasets(nestedContent, chartLibrary) />
        <#else>
        <#assign chartDatasets=0/>
    </#if>
    <#if (chartDatasets < 1)><#local chartDatasets = 1 /></#if>
    <#-- THEME Specific changes -->
    <div class="chart-wrapper">
        <span class="chart-data">&nbsp;</span>
        <canvas id="${escapeVal(id, 'html')}" height="300" width="500"></canvas>
    </div>
    <#-- End of THEME Specific changes -->
    <@script>
        $(function(){
            var chartDataEl = $('.chart-data').first();
            var chartData = chartDataEl.sassToJs({pseudoEl:"::before", cssProperty: "content"});
            var options =  {
                    responsive: true, 
                    responsiveAnimationDuration: 0, 
                    animation: {
                        duration: 1000
                    },
                    maintainAspectRatio: true,
                    tooltips: {
                        mode: <#if type=="line" || type=="bar">'label'<#else>'single'</#if><#if (labelUom1?has_content ||labelUom2?has_content) >,
                        callbacks: {
                            label: function(tooltipItem, data) {                                
                                <#if labelUom1?has_content> 
                                    if(tooltipItem.datasetIndex == 0) {
                                        var datasetLabel = '';                                  
                                        <#if type=="line" || type=="bar">
                                             datasetLabel = data.datasets[tooltipItem.datasetIndex].label;
                                             return datasetLabel + ': ' + tooltipItem.yLabel + ' ${escapeVal(labelUom1, 'js')}';
                                        <#elseif type="pie">
                                             datasetLabel = data.labels[tooltipItem.index] + ': ' + data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
                                             return datasetLabel + ' ${escapeVal(labelUom1, 'js')}';
                                        <#else>
                                            return datasetLabel;
                                        </#if>
                                    }
                                </#if>
                                <#if labelUom2?has_content> 
                                    if(tooltipItem.datasetIndex == 1) {
                                        var datasetLabel = data.datasets[tooltipItem.datasetIndex].label || '';
                                        return datasetLabel + ': ' + tooltipItem.yLabel + ' ${escapeVal(labelUom2, 'js')}';
                                    }
                                </#if>
                            }
                        }
                        </#if>
                    },
                    hover: {
                        mode: <#if type=="line" || type=="bar">'label'<#else>'single'</#if>
                    },
                    legend: {
                        position: 'bottom',
                        labels: {
                            boxWidth: 30
                        }
                    },
                    title: {
                        <#if title?has_content>
                            display: true,
                            text: '${escapeVal(title, 'js')}',
                        <#else>
                            display: false,
                        </#if>
                        fontColor: chartData.scaleLabelFontColor,
                        fontFamily: chartData.scaleLabelFontFamily,
                        fontSize: chartData.scaleLabelFontSize
                    }
                    <#if type=="line" || type=="bar">,
                        <#if type=="line"> animation: {
                        duration: 0
                        },</#if>
                        scales: {
                            type: chartData.scaleType,
                            display: true,                        
                            xAxes: [{
                                gridLines: {
                                    color: chartData.scaleGridLineColor
                                },
                                scaleLabel : {
                                    display: chartData.scaleLabelDisplay,
                                    <#if xlabel?has_content>labelString: '${escapeVal(xlabel, 'js')}',</#if>
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize                                
                                },
                                ticks: {
                                    display: true,
                                    autoSkip: true,
                                    padding:10,
                                    maxRotation:30,
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize
                                }                            
                              }],
                            yAxes: [{
                                scaleLabel : {
                                    display: chartData.scaleLabelDisplay,
                                    <#if ylabel?has_content>scaleLabel: '${escapeVal(xlabel, 'js')}',</#if>
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize
                                },
                                ticks: {
                                    display: true,
                                    autoSkip: true,                            
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize
                                }
                            }]
                        }
                    <#elseif type=="pie">,                        
                        scale: {
                           type: chartData.scaleType,
                           display: false
                        }            
                    </#if>
                };
            var canvasElem = $('#${escapeVal(id, 'js')}');
            var ctx = canvasElem.get(0).getContext("2d");
            var data = {
                labels :[],
                datasets: [
                    {
                      <#if type=="line" || type=="bar">
                      label: '${escapeVal(label1, 'js')}',                      
                      fill: true,
                      backgroundColor: chartData.primaryFillColor,
                      borderColor: chartData.primaryStrokeColor,
                      pointBackgroundColor: chartData.pointColor,
                      pointBorderColor: chartData.primaryPointStrokeColor,
                      pointHoverBackgroundColor: chartData.pointHighlightFill,
                      pointHoverBorderColor: chartData.pointHighlightStroke,
                      <#else>
                      backgroundColor: [
                            chartData.pieFillColor1,
                            chartData.pieFillColor2,
                            chartData.pieFillColor3,
                            chartData.pieFillColor4,
                            chartData.pieFillColor5,
                            chartData.pieFillColor6
                        ],
                        hoverBackgroundColor: [
                            chartData.pieHighlightColor1,
                            chartData.pieHighlightColor2,
                            chartData.pieHighlightColor3,
                            chartData.pieHighlightColor4,
                            chartData.pieHighlightColor5,
                            chartData.pieHighlightColor6
                        ],
                      </#if>
                      data: []
                    }
                    <#if (chartDatasets > 1)>
                    ,{
                      <#if (type=="line" || type=="bar")>
                      label: '${escapeVal(label2, 'js')}',
                      fill: true,
                      backgroundColor: chartData.secondaryFillColor,
                      borderColor: chartData.secondaryStrokeColor,
                      pointBackgroundColor: chartData.pointColor,
                      pointBorderColor: chartData.secondaryPointStrokeColor,
                      pointHoverBackgroundColor: chartData.pointHighlightFill,
                      pointHoverBorderColor: chartData.pointHighlightStroke,
                      <#else>
                       backgroundColor: [
                            chartData.pieFillColor1,
                            chartData.pieFillColor2,
                            chartData.pieFillColor3,
                            chartData.pieFillColor4,
                            chartData.pieFillColor5,
                            chartData.pieFillColor6
                        ],
                      </#if>
                      data: []
                    }           
                    </#if>        
                    ]
                };
            var config = {
                <#switch type>
                    <#case "bar">type: 'bar'<#break>
                    <#case "pie">type: 'pie'<#break>
                    <#default>type: '${type!"line"}'
                </#switch>,
                data: data,
                options: options
            };

            var chart = new Chart(ctx, config);
            canvasElem.data("chart", chart);
            var newChart = chart; <#-- backward-compat -->
            var ${escapeVal(id, 'js')} = chart; <#-- backward-compat: TODO: REMOVE: escapeVal is not valid for use outside quotes (see freemarker docs for ?js) -->
            <#-- escapeVal unsafe outside quotes and other issues
            ${escapeVal(id, 'js')} = chart; -->
            ${nestedContent}
            chart.update();
        });
    </@script>
  </#if>
</#macro>


<#-- save copy of this namespace so that our macros are able to access its own definitions without overrides (sometimes needed) -->
<#assign scipioBsTmplLib = copyObject(.namespace)>
