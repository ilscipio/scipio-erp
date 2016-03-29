<#--
* 
* Individual field widgets HTML template include, standard Cato markup
*
* Included by htmlForm.ftl.
*
* NOTES: 
* * May have implicit dependencies on other parts of Cato API.
*
-->

<#-- 
TODO:
  * The parameters on these should be refined to be less ofbiz-like and more cato-like; but must
    be flexible enough to 100% allow calls from ofbiz widget lib macros.
  * _markup_widget macros should be cleaned up of logic and the logic moved to _widget macros 
  * The tooltip attributes are not safe for arbitrary attributes coming from styles hash
  * title arg should override tooltip, but currently doing the other way around
  * CONVERT EVERYTHING TO USE attribs WITH @elemAttribStr MAP INSTEAD OF INLINES, this 
    would fix the previous point too and prevent a lot of other bugs
    but may be a little bit harder than it sounds due to multiple sources of attrib maps
-->

<#-- migrated from @renderClass form widget macro -->
<#macro fieldClassAttribStr class alert=false>
  <#if alert?string == "true">
    <#local class = addClassArg(class, "alert")>
  </#if>
  <@compiledClassAttribStr class=class /><#t/>
</#macro>

<#-- 
*************
* fieldElemAttribStr
************
Specific version of @elemAttribStr, similar to @commonElemAttribStr but specific for these fields.
-->
<#macro fieldElemAttribStr attribs includeEmpty=false emptyValToken="_EMPTY_VALUE_" noValToken="_NO_VALUE_" exclude=[] noExclude=[]
  attribNamePrefix="" alwaysAddPrefix=true attribNamePrefixStrip="" attribNameSubstitutes={} camelCaseToDashLowerNames=true>
  <#t/><@elemAttribStr attribs=attribs includeEmpty=includeEmpty emptyValToken=emptyValToken noValToken=noValToken exclude=exclude
    attribNamePrefix=attribNamePrefix alwaysAddPrefix=alwaysAddPrefix attribNamePrefixStrip=attribNamePrefixStrip 
    attribNameSubstitutes=attribNameSubstitutes camelCaseToDashLowerNames=camelCaseToDashLowerNames /><#t/>
</#macro>

<#-- migrated from @renderTextField form widget macro -->
<#assign field_input_widget_defaultArgs = {
  "name":"", "class":"", "alert":"", "value":"", "textSize":"", "maxlength":"", "id":"", "events":{}, "disabled":false, "ajaxUrl":"", 
  "ajaxEnabled":false, "mask":false, "clientAutocomplete":"", "placeholder":"", "tooltip":"", "title":"", "collapse":false, "readonly":false, 
  "fieldTitleBlank":false, "inlineLabel":false, "passArgs":{}
}>
<#macro field_input_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_input_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !disabled?is_boolean>
    <#if disabled?has_content>
      <#local disabled = true>
    <#else>
      <#local disabled = false>
    </#if>
  </#if>
  <#if !readonly?is_boolean>
    <#if readonly?has_content>
      <#local readonly = true>
    <#else>
      <#local readonly = false>
    </#if>
  </#if>
  <#if !clientAutocomplete?is_boolean>
    <#if clientAutocomplete?has_content>
      <#local clientAutocomplete = clientAutocomplete?boolean>
    <#else>
      <#local clientAutocomplete = true>
    </#if>
  </#if>
  <@field_input_markup_widget name=name class=class alert=alert value=value textSize=textSize maxlength=maxlength id=id events=events disabled=disabled ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled 
    mask=mask clientAutocomplete=clientAutocomplete placeholder=placeholder tooltip=tooltip title=title collapse=collapse readonly=readonly fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_input_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_input_markup_widget name="" class="" alert="" value="" textSize="" maxlength="" id="" events={} disabled=false ajaxUrl="" ajaxEnabled=false 
    mask=false clientAutocomplete=true placeholder="" tooltip="" title="" collapse=false readonly=false fieldTitleBlank=false inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content> 
    <#local class = addClassArg(class, styles.field_input_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = attribs + styles.field_input_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
  </#if>
  <#if mask?has_content && mask>
    <@script>
      jQuery(function($){jQuery("#${id}").mask("${mask!}");});
    </@script>
  </#if>
  <input type="text" name="${name?html}"<#t/>
    <@fieldElemAttribStr attribs=attribs /><#t/>
    <#if title?has_content> title="${title}"</#if><#t/>
    <@fieldClassAttribStr class=class alert=alert /><#t/>
    <#if value?has_content> value="${value}"</#if><#t/>
    <#if textSize?has_content> size="${textSize}"</#if><#t/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#t/>
    <#if disabled> disabled="disabled"</#if><#t/>
    <#if readonly> readonly="readonly"</#if><#t/>
    <#if id?has_content> id="${id}"</#if><#t/>
    <#if events?has_content><@commonElemEventAttribStr events=events /></#if><#t/>
    <#if !clientAutocomplete> autocomplete="off"</#if><#t/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#t/>
  /><#t/>
  <#if ajaxUrl?has_content>
    <#local defaultMinLength = getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")!2>
    <#local defaultDelay = getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")!300>
    <@script>ajaxAutoCompleter('${ajaxUrl}', false, ${defaultMinLength}, ${defaultDelay});</@script><#lt/>
  </#if>
</#macro>

<#-- migrated from @renderTextareaField form widget macro -->
<#assign field_textarea_widget_defaultArgs = {
  "name":"", "class":"", "alert":"", "cols":"", "rows":"", "id":"", "readonly":"", "value":"", "visualEditorEnable":true, 
  "buttons":"", "language":"", "placeholder":"", "tooltip":"", "title":"", "fieldTitleBlank":false, "collapse":false, 
  "inlineLabel":false, "wrap":"", "passArgs":{}
}>
<#macro field_textarea_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_textarea_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !readonly?is_boolean>
    <#if readonly?has_content>
      <#local readonly = true>
    <#else>
      <#local readonly = false>
    </#if>
  </#if>
  <@field_textarea_markup_widget name=name class=class alert=alert cols=cols rows=rows id=id readonly=readonly value=value visualEditorEnable=visualEditorEnable 
    buttons=buttons language=language placeholder=placeholder tooltip=tooltip title=title fieldTitleBlank=fieldTitleBlank collapse=collapse inlineLabel=inlineLabel wrap=wrap origArgs=origArgs passArgs=passArgs><#nested></@field_textarea_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_textarea_markup_widget name="" class="" alert="" cols="" rows="" id="" readonly=false value="" visualEditorEnable=true 
    buttons="" language="" placeholder="" tooltip="" title="" fieldTitleBlank=false collapse=false inlineLabel=false wrap="" origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content> 
    <#local class = addClassArg(class, styles.field_textarea_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = attribs + styles.field_textarea_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
  </#if>
  <textarea name="${name}"<#t/>
    <@fieldElemAttribStr attribs=attribs /><#t/>
    <@fieldClassAttribStr class=class alert=alert /><#t/>
    <#if title?has_content> title="${title}"</#if><#t/>
    <#if cols?has_content> cols="${cols}"</#if><#t/>
    <#if rows?has_content> rows="${rows}"</#if><#t/>
    <#if id?has_content> id="${id}"</#if><#t/>
    <#if readonly> readonly="readonly"</#if><#t/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#t/>
    <#if wrap?has_content> wrap="${wrap}"</#if><#t/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#t/>
  ><#t/>
    <#if value?has_content>${value}<#else><#nested></#if><#t/>
  </textarea><#lt/>
  <#--
  TODO: Remove
  <#if visualEditorEnable?has_content>
    <@script src="/images/jquery/plugins/elrte-1.3/js/elrte.min.js" /><#rt/>
    <#if language?has_content && language != "en">
      <@script src="/images/jquery/plugins/elrte-1.3/js/i18n/elrte.${language!'en'}.js" /><#rt/>
    </#if>
    <link href="<@ofbizContentUrl>/images/jquery/plugins/elrte-1.3/css/elrte.min.css</@ofbizContentUrl>" rel="stylesheet" type="text/css">
    <@script>
      var opts = {
         cssClass : 'el-rte',
         lang     : '${language!"en"}',
         toolbar  : '${buttons!"maxi"}',
         doctype  : '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">', //'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN">',
         cssfiles : ['/images/jquery/plugins/elrte-1.3/css/elrte-inner.css']
      }
      jQuery('#${id!""}').elrte(opts);
    </@script>
  </#if>
  -->
</#macro>

<#-- migrated from @renderDateTimeField form widget macro -->
<#assign field_datetime_widget_defaultArgs = {
  "name":"", "class":"", "title":"", "value":"", "size":"", "maxlength":"", "id":"", "dateType":"", "dateDisplayType":"", 
  "timeDropdownParamName":"", "defaultDateTimeString":"", "localizedIconTitle":"", "timeDropdown":"", "timeHourName":"", 
  "classString":"", "hour1":"", "hour2":"", "timeMinutesName":"", "minutes":"", "isTwelveHour":"", "ampmName":"", "amSelected":"", 
  "pmSelected":"", "compositeType":"", "formName":"", "alert":"", "mask":"", "events":{}, "step":"", "timeValues":"", "tooltip":"", 
  "collapse":false, "fieldTitleBlank":false, "origLabel":"", "inlineLabel":false, "passArgs":{}
}>
<#macro field_datetime_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_datetime_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !["date", "time", "timestamp"]?seq_contains(dateType)>
    <#local dateType = "timestamp">
  </#if>
  <#if !dateDisplayType?has_content || dateDisplayType == "default">
    <#-- make this easier for markup -->
    <#local dateDisplayType = dateType>
  </#if>
  <#-- REQUIRE a unique ID for this type of field -->
  <#if !id?has_content>
    <#local id = getNextFieldId()>
  </#if>
  <@field_datetime_markup_widget name=name class=class title=title value=value size=size maxlength=maxlength id=id dateType=dateType dateDisplayType=dateDisplayType 
    timeDropdownParamName=timeDropdownParamName defaultDateTimeString=defaultDateTimeString localizedIconTitle=localizedIconTitle timeDropdown=timeDropdown timeHourName=timeHourName classString=classString 
    hour1=hour1 hour2=hour2 timeMinutesName=timeMinutesName minutes=minutes isTwelveHour=isTwelveHour ampmName=ampmName amSelected=amSelected pmSelected=pmSelected compositeType=compositeType formName=formName 
    alert=alert mask=mask events=events step=step timeValues=timeValues tooltip=tooltip collapse=false fieldTitleBlank=fieldTitleBlank origLabel=origLabel inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_datetime_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_datetime_markup_widget name="" class="" title="" value="" size="" maxlength="" id="" dateType="" dateDisplayType="" 
    timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" 
    hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName="" 
    alert=false mask="" events={} step="" timeValues="" tooltip="" collapse=false fieldTitleBlank=false origLabel="" inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#-- NOTE: dateType and dateDisplayType (previously shortDateInput) are distinct and both are necessary. 
      dateType controls the type of data sent to the server; dateDisplayType only controls what's displayed to user. 
      (dateType=="date") is not the same as (dateDisplayType=="date" && dateType=="timestamp"). -->  
  <#local dateDisplayFormat><#if dateDisplayType == "date">yyyy-MM-dd<#elseif dateDisplayType == "time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local dateDisplayFormatProp><#if dateDisplayType == "date">CommonFormatDate<#elseif dateDisplayType == "time">CommonFormatTime<#else>CommonFormatDateTime</#if></#local>
  <#local displayInputId = "">
  <#local inputId = "">
  <#if id?has_content>
    <#local displayInputId = "${id}_i18n">
    <#local inputId = "${id}">
  </#if>
  <#local displayInputName = "">
  <#local inputName = "">
  <#if name?has_content>
    <#local displayInputName = "${name?html}_i18n">
    <#local inputName = "${name?html}">
  </#if>
  <#local attribs = {}>
  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateDisplayFormat}">
    <div class="${styles.grid_small!}11 ${styles.grid_cell!}">
      <#if tooltip?has_content> 
        <#local class = addClassArg(class, styles.field_datetime_tooltip!styles.field_default_tooltip!"")>
        <#-- tooltip supplants title -->
        <#local title = tooltip>
        <#local attribs = attribs + styles.field_datetime_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
      </#if>
      <#if title?is_boolean && title == false>
        <#local title = "">
      <#else>
        <#if title?is_boolean && title == true>
          <#local title = "">
        </#if>
        <#if !title?has_content>
          <#local title = styles.field_datetime_default_title!>
        </#if>
        <#if title?has_content>
          <#-- NOTE: two property lookups kind of inefficient, but at least customizable, no sense going back -->
          <#local dateFormatString = getPropertyMsg("CommonUiLabels", dateDisplayFormatProp)!"">
          <#if title == "FORMAT">
            <#local title = dateFormatString>
          <#elseif title == "LABEL">
            <#local title = origLabel>
          <#elseif title == "LABEL+FORMAT">
            <#if origLabel?has_content>
              <#local title = origLabel + " (" + dateFormatString + ")">
            <#else>
              <#local title = dateFormatString>
            </#if>
          <#else>
            <#local title = getTextLabelFromExpr(title, {"dateLabel":origLabel, "dateFormatString":dateFormatString})!"">
          </#if>
        </#if>
      </#if>
      <#local class = addClassArg(class, "${styles.grid_small!}3 ${styles.grid_cell!}")>
      <input type="text" name="${displayInputName}"<@fieldClassAttribStr class=class alert=alert /><#rt/>
        <@fieldElemAttribStr attribs=attribs /><#t/>
        <#if title?has_content> title="${title}"</#if><#t/>
        <#if value?has_content> value="${value}"</#if><#t/>
        <#if size?has_content> size="${size}"</#if><#t/>
        <#if maxlength?has_content> maxlength="${maxlength}"</#if>
        <#if displayInputId?has_content> id="${displayInputId}"</#if> /><#t/>
      <input type="hidden"<#if inputName?has_content> name="${inputName}"</#if><#if inputId?has_content> id="${inputId}"</#if><#if value?has_content> value="${value}"</#if> />
    </div>
    <div class="${styles.grid_small!}1 ${styles.grid_cell!}">
      <span class="postfix"><i class="${styles.icon!} ${styles.icon_calendar!}"></i></span>
    </div>
  </div>
  <@field_datetime_markup_script inputId=inputId inputName=inputName displayInputId=displayInputId displayInputName=displayInputName dateType=dateType dateDisplayType=dateDisplayType origArgs=origArgs passArgs=passArgs />
</#macro>

<#macro field_datetime_markup_script inputId="" inputName="" displayInputId="" displayInputName="" dateType="" dateDisplayType="" htmlwrap=true origArgs={} passArgs={} catchArgs...>
  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <@script htmlwrap=htmlwrap>
    $(function() {

        var dateI18nToNorm = function(date) {
            <#-- TODO: WARN: this needs to be implemented if the displayed date is ever different from the 
                    internal format (timestamp-like) 
                NOTE: this will vary based on date type and format -->
            return date;
        };
        
        var dateNormToI18n = function(date) {
            <#-- TODO: WARN: this needs to be implemented if the displayed date is ever different from the 
                    internal format (timestamp-like) 
                NOTE: this will vary based on date type and format -->
            return date;
        };
    
        jQuery("#${displayInputId}").change(function() {
          <#if dateType == "timestamp">
            jQuery("#${inputId}").val(convertToDateTimeNorm(dateI18nToNorm(this.value)));
          <#elseif dateType == "date">
            jQuery("#${inputId}").val(convertToDateNorm(dateI18nToNorm(this.value)));
          <#elseif dateType == "time">
            jQuery("#${inputId}").val(convertToTimeNorm(dateI18nToNorm(this.value)));
          </#if>
        });
        
      <#if dateType == "time">
      
        <#-- do nothing for now; user inputs into box manually and change() should adjust -->

      <#else>
      
        var oldDate = "";
        var onFDatePopup = function(ev) {
            oldDate = dateI18nToNorm(jQuery("#${displayInputId}").val());
        };
        var onFDateChange = function(ev) {
          <#if dateDisplayType == "timestamp">
            jQuery("#${displayInputId}").val(dateNormToI18n(convertToDateTimeNorm(dateI18nToNorm(jQuery("#${displayInputId}").val()), oldDate)));
          <#elseif dateDisplayType == "date">
            jQuery("#${displayInputId}").val(dateNormToI18n(convertToDateNorm(dateI18nToNorm(jQuery("#${displayInputId}").val()), oldDate)));
          </#if>
        };
        
        <#-- Cato: How this works: the fdatepicker will put a yyyy-MM-dd value into the id_i18n field. 
            This triggers onFDateChange which may transform the date and put it back in id_i18n.
            This triggers then another change() which copies it into the hidden id field (with another conversion if necessary). -->
        $("#${displayInputId}").fdatepicker(${fdatepickerOptions}).on('changeDate', onFDateChange).on('show', onFDatePopup);
        <#-- Cannot use name, must use ID, this is invalid (will break multiple forms per page): $("input[name='${displayInputName}']")-->

      </#if>
    });
  </@script>
</#macro>

<#-- migrated from @renderDateFindField form widget macro -->
<#assign field_datefind_widget_defaultArgs = {
  "class":"", "id":"", "alert":"", "name":"", "localizedInputTitle":"", "value":"", "value2":"", "size":"", "maxlength":"", "dateType":"", "dateDisplayType":"",
  "formName":"", "defaultDateTimeString":"", "imgSrc":"", "localizedIconTitle":"", "titleClass":"", "defaultOptionFrom":"", 
  "defaultOptionThru":"", "opEquals":"", "opSameDay":"", "opGreaterThanFromDayStart":"", "opGreaterThan":"",
  "opLessThan":"", "opUpToDay":"", "opUpThruDay":"", "opIsEmpty":"", 
  "title":"", "tooltip":"", "inlineLabel":false, "origLabel":"", "passArgs":{}
}>
<#macro field_datefind_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_datefind_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !["date", "time", "timestamp"]?seq_contains(dateType)>
    <#local dateType = "timestamp">
  </#if>
  <#if !dateDisplayType?has_content || dateDisplayType == "default">
    <#-- make this easier for markup -->
    <#local dateDisplayType = dateType>
  </#if>
  <#-- REQUIRE a unique ID for this type of field -->
  <#if !id?has_content>
    <#local id = getNextFieldId()>
  </#if>
  <#if !(opEquals?is_boolean && opEquals == false) && !opEquals?has_content>
    <#local opEquals = getPropertyMsg("conditional", "equals")!"">
  </#if>
  <#if !(opGreaterThan?is_boolean && opGreaterThan == false) && !opGreaterThan?has_content>
    <#local opGreaterThan = getPropertyMsg("conditional", "greater_than")!"">
  </#if>
  <#if !(opSameDay?is_boolean && opSameDay == false) && !opSameDay?has_content>
    <#local opSameDay = getPropertyMsg("conditional", "same_day")!"">
  </#if>
  <#if !(opGreaterThanFromDayStart?is_boolean && opGreaterThanFromDayStart == false) && !opGreaterThanFromDayStart?has_content>
    <#local opGreaterThanFromDayStart = getPropertyMsg("conditional", "greater_than_from_day_start")!"">
  </#if>
  <#if !(opLessThan?is_boolean && opLessThan == false) && !opLessThan?has_content>
    <#local opLessThan = getPropertyMsg("conditional", "less_than")!"">
  </#if>
  <#if !(opUpToDay?is_boolean && opUpToDay == false) && !opUpToDay?has_content>
    <#local opUpToDay = getPropertyMsg("conditional", "up_to_day")!"">
  </#if>
  <#if !(opUpThruDay?is_boolean && opUpThruDay == false) && !opUpThruDay?has_content>
    <#local opUpThruDay = getPropertyMsg("conditional", "up_thru_day")!"">
  </#if>
  <#if !(opIsEmpty?is_boolean && opIsEmpty == false) && !opIsEmpty?has_content>
    <#local opIsEmpty = getPropertyMsg("conditional", "is_empty")!"">
  </#if>
  <@field_datefind_markup_widget id=id class=class alert=alert name=name localizedInputTitle=localizedInputTitle value=value value2=value2 size=size maxlength=maxlength dateType=dateType dateDisplayType=dateDisplayType
    formName=formName defaultDateTimeString=defaultDateTimeString imgSrc=imgSrc localizedIconTitle=localizedIconTitle titleClass=titleClass defaultOptionFrom=defaultOptionFrom defaultOptionThru=defaultOptionThru 
    opEquals=opEquals opSameDay=opSameDay opGreaterThanFromDayStart=opGreaterThanFromDayStart opGreaterThan=opGreaterThan opGreaterThan=opGreaterThan opLessThan=opLessThan opUpToDay=opUpToDay 
    opUpThruDay=opUpThruDay opIsEmpty=opIsEmpty 
    title=title tooltip=tooltip inlineLabel=inlineLabel origLabel=origLabel origArgs=origArgs passArgs=passArgs><#nested></@field_datefind_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_datefind_markup_widget id="" class="" alert="" name="" localizedInputTitle="" value="" value2="" size="" maxlength="" dateType="" dateDisplayType=""
    formName="" defaultDateTimeString="" imgSrc="" localizedIconTitle="" titleClass="" defaultOptionFrom="" defaultOptionThru="" 
    opEquals="" opSameDay="" opGreaterThanFromDayStart="" opGreaterThan="" opLessThan="" opUpToDay="" opUpThruDay="" opIsEmpty="" 
    title="" tooltip="" inlineLabel=false origLabel=origLabel origArgs={} passArgs={} catchArgs...>
  <#-- NOTE: values of localizedInputTitle are: uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateDisplayFormat><#if dateDisplayType == "date">yyyy-MM-dd<#elseif dateDisplayType == "time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local displayInputId = "">
  <#local inputId = "">
  <#if id?has_content>
    <#local displayInputId = "${id}_i18n">
    <#local inputId = "${id}">
  </#if>
  <#local displayInputName = "">
  <#local inputName = "">
  <#if name?has_content>
    <#local displayInputName = "${name?html}_fld0_i18n">
    <#local inputName = "${name?html}_fld0_value">
  </#if>
  <#local opSelectName = "">
  <#if name?has_content>
    <#local opSelectName = "${name?html}_fld0_op">
  </#if>
  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateDisplayFormat}">
    <div class="${styles.grid_small!}5 ${styles.grid_cell!}">
      <#local class = addClassArg(class, "${styles.grid_small!}3 ${styles.grid_cell!}")>
      <input type="text"<#if displayInputId?has_content> id="${displayInputId}"</#if><#if displayInputName?has_content> name="${displayInputName}"</#if><@fieldClassAttribStr class=class alert=alert /><#rt/>
        <#if localizedInputTitle?has_content> title="${localizedInputTitle}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if>/><#lt/>
      <input type="hidden"<#if inputId?has_content> id="${inputId}"</#if><#if inputName?has_content> name="${inputName}"</#if><#if value?has_content> value="${value}"</#if>/>
    </div>
    <div class="${styles.grid_small!}1 ${styles.grid_cell!}">
      <span class="postfix"><i class="${styles.icon} ${styles.icon_calendar!}"></i></span>
    </div>
    <div class="${styles.grid_small!}5 ${styles.grid_cell!} ${styles.grid_small!}offset-1">
      <select<#if opSelectName?has_content> name="${opSelectName}"</#if> class="selectBox">
        <option value="equals"<#if defaultOptionFrom == "equals"> selected="selected"</#if>>${opEquals}</option>
        <option value="sameDay"<#if defaultOptionFrom == "sameDay"> selected="selected"</#if>>${opSameDay}</option>
        <option value="greaterThanFromDayStart"<#if defaultOptionFrom == "greaterThanFromDayStart"> selected="selected"</#if>>${opGreaterThanFromDayStart}</option>
        <option value="greaterThan"<#if defaultOptionFrom == "greaterThan"> selected="selected"</#if>>${opGreaterThan}</option>
      </select>
    </div>
  </div>
  <@field_datetime_markup_script inputId=inputId inputName=inputName displayInputId=displayInputId displayInputName=displayInputName dateType=dateType dateDisplayType=dateDisplayType origArgs=origArgs passArgs=passArgs />  
</#macro>

<#-- migrated from @renderDropDownField form widget macro -->
<#assign field_select_widget_defaultArgs = {
  "name":"", "class":"", "alert":"", "id":"", "multiple":"", "formName":"", "formId":"", "otherFieldName":"", "size":"", 
  "currentFirst":"", "currentValue":"", "currentDescription":"", "allowEmpty":"", "options":"", "fieldName":"", "otherFieldName":"", 
  "otherValue":"", "otherFieldSize":"", "dDFCurrent":"", "defaultValue":"", "ajaxOptions":"", "frequency":"", "minChars":"",
  "choices":"", "autoSelect":"", "partialSearch":"", "partialChars":"", "ignoreCase":"", "fullSearch":"", "events":{}, 
  "ajaxEnabled":false, "title":"", "tooltip":"", "description":"", "manualItems":false, "manualItemsOnly":false, "collapse":false, 
  "fieldTitleBlank":false, "inlineSelected":true, "disabled":false, "asmSelectArgs":{}, "inlineLabel":false, "passArgs":{}
}>
<#macro field_select_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_select_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !multiple?is_boolean>
    <#if multiple == "true" || multiple == "multiple" || multiple == "Y">
      <#local multiple = true>
    <#elseif multiple == "false" || multiple == "N">
      <#local multiple = false>
    <#elseif multiple?has_content> <#-- legacy ofbiz behavior -->
      <#local multiple = true>
    <#else>
      <#local multiple = false>
    </#if>
  </#if>
  <#if !allowEmpty?is_boolean>
    <#if allowEmpty == "true" || allowEmpty == "Y">
      <#local allowEmpty = true>
    <#elseif allowEmpty == "false" || allowEmpty == "N">
      <#local allowEmpty = false>  
    <#elseif allowEmpty?has_content> <#-- legacy ofbiz behavior -->
      <#local allowEmpty = true>
    <#else>
      <#local allowEmpty = false>
    </#if>
  </#if>
  <#if !currentFirst?is_boolean>
    <#if currentFirst == "true" || currentFirst == "Y" || currentFirst == "first-in-list">
      <#local currentFirst = true>
    <#elseif currentFirst == "false" || currentFirst == "N">
      <#local currentFirst = false>  
    <#elseif currentFirst?has_content> <#-- legacy ofbiz behavior -->
      <#local currentFirst = true>
    <#else>
      <#local currentFirst = false>
    </#if>
  </#if>
  <#if currentFirst>
    <#-- these are mutually exclusive -->
    <#local inlineSelected = false>
  </#if>
  <#if currentFirst && !currentDescription?has_content && currentValue?has_content>
    <#-- have to find it -->
    <#if options?has_content>
      <#list options as option>
        <#if (option.key!"") == currentValue>
          <#local currentDescription = option.description!"">
          <#break>
        </#if>
      </#list>
    </#if>
  </#if>
  <@field_select_markup_widget name=name class=class alert=alert id=id multiple=multiple formName=formName formId=formId otherFieldName=otherFieldName size=size currentFirst=currentFirst 
    currentValue=currentValue currentDescription=currentDescription allowEmpty=allowEmpty options=options fieldName=fieldName otherFieldName=otherFieldName otherValue=otherValue otherFieldSize=otherFieldSize 
    dDFCurrent=dDFCurrent defaultValue=defaultValue ajaxOptions=ajaxOptions frequency=frequency minChars=minChars choices=choices autoSelect=autoSelect partialSearch=partialSearch partialChars=partialChars 
    ignoreCase=ignoreCase fullSearch=fullSearch events=events ajaxEnabled=ajaxEnabled title=title tooltip=tooltip description=description manualItems=manualItems manualItemsOnly=manualItemsOnly 
    collapse=collapse fieldTitleBlank=fieldTitleBlank inlineSelected=inlineSelected asmSelectArgs=asmSelectArgs inlineLabel=inlineLabel disabled=disabled origArgs=origArgs passArgs=passArgs><#nested></@field_select_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_select_markup_widget name="" class="" alert="" id="" multiple=false formName="" formId="" otherFieldName="" size="" currentFirst=false 
    currentValue="" currentDescription="" allowEmpty=true options="" fieldName="" otherFieldName="" otherValue="" otherFieldSize="" 
    dDFCurrent="" defaultValue="" ajaxOptions="" frequency="" minChars="" choices="" autoSelect="" partialSearch="" partialChars="" 
    ignoreCase="" fullSearch="" events={} ajaxEnabled=false title="" tooltip="" description="" manualItems=false manualItemsOnly=false 
    collapse=false fieldTitleBlank=false inlineSelected=true disabled=false asmSelectArgs={} inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content>
    <#local class = addClassArg(class, styles.field_select_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = attribs + styles.field_select_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
  </#if>
  <select name="${name}"<@fieldClassAttribStr class=class alert=alert /><#rt/>
    <@fieldElemAttribStr attribs=attribs /><#t/>
    <#if id?has_content> id="${id}"</#if><#t/>
    <#if multiple> multiple="multiple"</#if><#t/>
    <#-- FIXME: this onchange may conflict with the other events -->
    <#if (otherFieldSize > 0)> onchange="process_choice(this,document.${formName}.${otherFieldName})"</#if><#t/>
    <#if events?has_content><@commonElemEventAttribStr events=events /></#if><#t/>
    <#--<#if size?has_content> size="${size}"</#if>-->
    <#if title?has_content> title="${title}"</#if><#t/>
    <#if disabled> disabled="disabled"</#if><#t/>
  ><#lt/>
  <#if !manualItemsOnly>  
    <#if currentFirst && currentValue?has_content && !multiple>
      <option selected="selected" value="${currentValue}">${currentDescription}</option>
      <option value="${currentValue}">---</option>
    </#if>
    <#if allowEmpty || (!manualItems && !options?has_content)>
      <option value="">&nbsp;</option>
    </#if>
    <#list options as item>
      <#-- Cato: NOTE: this macro must support both item.value and legacy item.key. Here they are the same thing. -->
      <#local itemValue = item.value!item.key!>
      <#local itemMarkedSelected = item.selected?? && ((item.selected?is_boolean && item.selected == true) || (!item.selected?is_boolean && item.selected?has_content))>
      <#if multiple>
        <option<#if currentValue?has_content && itemMarkedSelected> selected="selected"<#rt/>
          <#elseif !currentValue?has_content && defaultValue?has_content && defaultValue == itemValue> selected="selected"</#if> value="${itemValue}">${item.description!}</option><#lt/>
      <#else>
        <option<#if currentValue?has_content && currentValue == itemValue && inlineSelected> selected="selected"<#rt/>
          <#elseif !currentValue?has_content && defaultValue?has_content && defaultValue == itemValue> selected="selected"</#if> value="${itemValue}">${item.description!}</option><#lt/>
      </#if>
    </#list>
  </#if>
    <#nested>
  </select>
  <#if otherFieldName?has_content>
    <noscript><input type="text" name="${otherFieldName}" /></noscript>
    <@script>
      disa = ' disabled';
      if(other_choice(document.${formName}.${fieldName}))
        disa = '';
      document.write('<input type="text" name="${otherFieldName}" value="${otherValue?js_string}" size="${otherFieldSize}"'+disa+' onfocus="check_choice(document.${formName}.${fieldName})" />');
      if(disa && document.styleSheets)
      document.${formName}.${otherFieldName}.styles.visibility  = 'hidden';
    </@script>
  </#if>
  <#if ajaxEnabled>
    <@script>
      ajaxAutoCompleteDropDown();
      jQuery(function() {
        jQuery("#${id}").combobox();
      });
    </@script>
  </#if>
  <#if asmSelectArgs?has_content>
    <#local asmtitle = asmSelectArgs.title!"">
    <#if !asmtitle?has_content>
      <#if title?has_content>
        <#local asmtitle = title>
      <#elseif description?has_content>
        <#local asmtitle = description>
      <#elseif tooltip?has_content>
        <#local asmtitle = tooltip>
      <#else>
        <#local asmtitle = ""> <#-- otherwise will show 'undefined' -->
      </#if>
    </#if>
    <@asmSelectScript id=id formName=formName formId=formId title=asmtitle args=asmSelectArgs passArgs=passArgs/>
  </#if>
</#macro>

<#assign field_option_widget_defaultArgs = {
  "text":"", "value":"", "selected":false, "passArgs":{}
}>
<#macro field_option_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_option_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !text?has_content>
    <#local text><#nested></#local>
  </#if>
  <@field_option_markup_widget text=text value=value selected=selected origArgs=origArgs passArgs=passArgs/>
</#macro>

<#-- field markup - theme override -->
<#macro field_option_markup_widget text="" value="" selected=false origArgs={} passArgs={} catchArgs...>
   <option value="${value}"<#if selected> selected="selected"</#if>>${text}</option><#t/>
</#macro>    

<#-- migrated from @renderLookupField form widget macro -->
<#assign field_lookup_widget_defaultArgs = {
  "name":"", "formName":"", "fieldFormName":"", "class":"", "alert":"", "value":"", "size":"", "maxlength":"", "id":"", 
  "events":{}, "readonly":false, "autocomplete":"", "descriptionFieldName":"", "targetParameterIter":"", "imgSrc":"", "ajaxUrl":"", 
  "ajaxEnabled":"", "presentation":"layer", "width":"", "height":"", "position":"", "fadeBackground":"true", 
  "clearText":"", "showDescription":"", "initiallyCollapsed":"", "lastViewName":"main", "title":"", "fieldTitleBlank":false, 
  "inlineLabel":false, "passArgs":{}
}>
<#macro field_lookup_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_lookup_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !ajaxEnabled?is_boolean>
    <#if ajaxEnabled?has_content>
      <#local ajaxEnabled = ajaxEnabled?boolean>
    <#else>
      <#local ajaxEnabled = javaScriptEnabled!false>
    </#if>
  </#if>
  <@field_lookup_markup_widget name=name formName=formName fieldFormName=fieldFormName class=class alert=alert value=value size=size 
    maxlength=maxlength id=id events=events readonly=readonly autocomplete=autocomplete descriptionFieldName=descriptionFieldName 
    targetParameterIter=targetParameterIter imgSrc=imgSrc ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled presentation=presentation width=width 
    height=height position=position fadeBackground=fadeBackground clearText=clearText showDescription=showDescription initiallyCollapsed=initiallyCollapsed 
    lastViewName=lastViewName title=title fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_lookup_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_lookup_markup_widget name="" formName="" fieldFormName="" class="" alert="false" value="" size="" 
    maxlength="" id="" events={} readonly=false autocomplete="" descriptionFieldName="" 
    targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=false presentation="layer" width="" 
    height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" 
    lastViewName="main" title="" fieldTitleBlank=false inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#if Static["org.ofbiz.widget.model.ModelWidget"].widgetBoundaryCommentsEnabled(context)>
  </#if>
  <#if (!ajaxUrl?has_content) && ajaxEnabled>
    <#local ajaxUrl = requestAttributes._REQUEST_HANDLER_.makeLink(request, response, fieldFormName)/>
    <#local ajaxUrl = id + "," + ajaxUrl + ",ajaxLookup=Y" />
  </#if>
  <#if (!showDescription?has_content)>
    <#local showDescriptionProp = getPropertyValue("widget.properties", "widget.lookup.showDescription")!"N">
    <#if "Y" == showDescriptionProp>
      <#local showDescription = "true" />
    <#else>
      <#local showDescription = "false" />
    </#if>
  </#if>
  <#if ajaxEnabled>
    <@script>
      jQuery(document).ready(function(){
        if (!jQuery('form[name="${formName}"]').length) {
          alert("Developer: for lookups to work you must provide a form name!")
        }
      });
    </@script>
  </#if>
  <span class="field-lookup">
    <#if size?has_content && size=="0">
      <input type="hidden" <#if name?has_content> name="${name}"/></#if>
    <#else>
      <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#rt/>
        <#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#t/>
        <#if readonly?has_content && readonly> readonly="readonly"</#if><#if events?has_content><@commonElemEventAttribStr events=events /></#if><#t/>
        <#if autocomplete?has_content> autocomplete="off"</#if>/></#if><#t/>
    <#if presentation?has_content && descriptionFieldName?has_content && presentation == "window">
      <a href="javascript:call_fieldlookup3(document.${formName?html}.${name?html},document.${formName?html}.${descriptionFieldName},'${fieldFormName}', '${presentation}'<#rt/>
      <#if targetParameterIter?has_content>
        <#list targetParameterIter as item>
          ,document.${formName}.${item}.value<#t/>
        </#list>
      </#if>
      );"></a><#rt/>
    <#elseif presentation?has_content && presentation == "window">
      <a href="javascript:call_fieldlookup2(document.${formName?html}.${name?html},'${fieldFormName}', '${presentation}'<#rt/>
      <#if targetParameterIter?has_content>
        <#list targetParameterIter as item>
          ,document.${formName}.${item}.value<#t/>
        </#list>
      </#if>
      );"></a><#rt/>
    <#else>
      <#if ajaxEnabled>
        <#local defaultMinLength = getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")!2>
        <#local defaultDelay = getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")!300>
        <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
        <#if !ajaxUrl?contains("searchValueFieldName=")>
          <#if descriptionFieldName?has_content && showDescription == "true">
            <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + descriptionFieldName />
          <#else>
            <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + name />
          </#if>
        </#if>
      </#if>
      <@script>
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
            ajaxUrl : <#if ajaxEnabled>"${ajaxUrl}"<#else>""</#if>,
            showDescription : <#if ajaxEnabled>"${showDescription}"<#else>false</#if>,
            presentation : "${presentation!}",
            defaultMinLength : "${defaultMinLength}",
            defaultDelay : "${defaultDelay}",
            args :
                <#if targetParameterIter?has_content>
                  <#local isFirst = true>
                  [<#t/>
                  <#list targetParameterIter as item>
                    <#if isFirst>
                      document.${formName}.${item}<#t/>
                      <#local isFirst = false>
                    <#else>
                      ,document.${formName}.${item}<#t/>
                    </#if>
                  </#list>
                  ]<#t/>
                <#else>[]
                </#if>
          };
          new Lookup(options).init();
        });
      </@script>
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
  <#if ajaxEnabled && (presentation?has_content && presentation == "window")>
    <#if ajaxUrl?index_of("_LAST_VIEW_NAME_") < 0>
      <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
    </#if>
    <@script>ajaxAutoCompleter('${ajaxUrl}', ${showDescription}, ${defaultMinLength}, ${defaultDelay});</@script><#t/>
  </#if>
</#macro>

<#-- migrated from @renderCheckField (a.k.a. @renderCheckBox) form widget macro -->
<#assign field_checkbox_widget_defaultArgs = {
  "items":[], "id":"", "class":"", "alert":"", "allChecked":"", "currentValue":"", "defaultValue":"", "name":"", "events":{}, 
  "tooltip":"", "title":"", "fieldTitleBlank":false, "multiMode":true, "inlineItems":"", "inlineLabel":false, "type":"", "passArgs":{}
}>
<#macro field_checkbox_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_checkbox_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !items?has_content>
    <#local items = []> <#-- ensure list -->
  </#if>
  <#if !inlineItems?is_boolean>
    <#if inlineItems?has_content>
      <#-- do conversion to boolean so markup doesn't have to; but don't impose a default -->
      <#local inlineItems = inlineItems?boolean>
    </#if>
  </#if>
  <#if !allChecked?is_boolean>
    <#if allChecked?has_content>
      <#if allChecked == "Y" || allChecked == "true" || allChecked == "checked">
        <#local allChecked = true>
      <#else>
        <#local allChecked = false>
      </#if>
    <#else>
      <#local allChecked = "">
    </#if>
  </#if>
  <#if currentValue?has_content>
    <#if !currentValue?is_sequence>
      <#local currentValue = [currentValue]> <#-- supports string arg for legacy -->
    </#if>
  <#else>
    <#local currentValue = []>
  </#if>
  <#if defaultValue?has_content>
    <#if !defaultValue?is_sequence>
      <#local defaultValue = [defaultValue]> <#-- supports string arg for legacy -->
    </#if>
  <#else>
    <#local defaultValue = []>
  </#if>
  <#if type == "default">
    <#local type = "">
  </#if>
  <#local stylesPrefix = "field_checkbox_" + type?replace("-", "_")>
  <#local defaultClass = styles[stylesPrefix]!styles["field_checkbox_default"]!"">
  <#local class = addClassArgDefault(class, defaultClass)>
  <#local labelType = styles[stylesPrefix + "_labeltype"]!styles["field_checkbox_default_labeltype"]!"standard">
  <#local labelPosition = styles[stylesPrefix + "_labelposition"]!styles["field_checkbox_default_labelposition"]!"after">
  <@field_checkbox_markup_widget items=items id=id class=class alert=alert allChecked=allChecked 
    currentValue=currentValue defaultValue=defaultValue name=name events=events tooltip=tooltip title=title multiMode=multiMode 
    fieldTitleBlank=fieldTitleBlank inlineItems=inlineItems inlineLabel=inlineLabel type=type stylesPrefix=stylesPrefix
    labelType=labelType labelPosition=labelPosition origArgs=origArgs passArgs=passArgs><#nested></@field_checkbox_markup_widget>
</#macro>

<#-- field markup - theme override 
     FIXME: the styling for these is strange, can't get it to work no matter what -->
<#macro field_checkbox_markup_widget items=[] id="" class="" alert="" allChecked="" currentValue=[] defaultValue=[] name="" 
    events={} tooltip="" title="" fieldTitleBlank=false multiMode=true inlineItems="" inlineLabel=false type="default" stylesPrefix=""
    labelType="standard" labelPosition="after" origArgs={} passArgs={} catchArgs...>
  <#if !inlineItems?is_boolean>
    <#local inlineItems = true>
  </#if>

  <#if multiMode>
    <div<@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${id}_multi"</#if>>
    <#local class = ""> <#-- in multi mode, classes only on parent for now (?) -->
  </#if>

  <#local attribs = {}>
  <#if inlineItems>
    <#local inlineClass = "checkbox-item-inline">
  <#else>
    <#local inlineClass = "checkbox-item-noninline">
  </#if>
  <#local class = addClassArg(class, "checkbox-item")>
  <#local class = addClassArg(class, inlineClass)>

  <#if tooltip?has_content>
    <#-- redundant...
    <#local class = addClassArg(class, styles.field_checkbox_tooltip!styles.field_default_tooltip!"")>
    <#local attribs = attribs + styles.field_checkbox_tooltip_attribs!styles.field_default_tooltip_attribs!{}>-->
    <#local title = tooltip>
  </#if>

  <#-- Cato: must have id on each elem or else the foundation switches break 
       The first item receives the exact id passed to macro because this is what original ofbiz macros expect
       (and is logical for single items at least). -->
  <#local currentId = id>
  <#list items as item>
    <#local inputAttribs = {}>
    <#local itemValue = item.value!"">
    <#local itemClass = class>
    <#local itemAlert = alert>
    
    <#local inputTitle = title>
    <#local inputClass = "">
    <#local inputAlert = false>
    <#if item.tooltip?has_content || tooltip?has_content>
      <#local inputClass = addClassArg(inputClass, styles.field_checkbox_tooltip!styles.field_default_tooltip!"")>
      <#if item.tooltip?has_content>
        <#local inputTitle = item.tooltip>
      </#if>
      <#local inputAttribs = inputAttribs + styles.field_checkbox_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
    </#if>
    <span<@fieldClassAttribStr class=itemClass alert=itemAlert /><#if currentId?has_content> id="${currentId}_item"</#if>>
      <#local labelMarkup>
        <#if labelType == "extralabel">
          <label<#if currentId?has_content> for="${currentId}"</#if>></label>
          <#-- FIXME?: description destroys field if put inside <label> above... also <label> has to be separate from input (not parent)... ? -->
          <#if item.description?has_content><span class="checkbox-label-local">${item.description}</span></#if>
        <#elseif labelType == "spanonly">
          <#if item.description?has_content><span class="checkbox-label-local">${item.description}</span></#if>
        <#else> <#-- labelType == "standard" -->
          <#if item.description?has_content><label class="checkbox-label-local"<#if currentId?has_content> for="${currentId}"</#if>>${item.description}</label></#if>
        </#if>
      </#local>
      <#if labelPosition == "before">${labelMarkup}</#if>
      <input type="checkbox"<@fieldClassAttribStr class=inputClass alert=inputAlert /><#rt/>
        <@fieldElemAttribStr attribs=attribs+inputAttribs /><#t/>
        <#if inputTitle?has_content> title="${inputTitle}"</#if><#t/>
        <#if currentId?has_content> id="${currentId}"</#if><#t/>
        <#if item.checked?has_content><#if item.checked> checked="checked"</#if><#elseif allChecked?has_content><#if allChecked> checked="checked"</#if><#t/>
        <#elseif currentValue?has_content && currentValue?seq_contains(itemValue)> checked="checked"<#t/>
        <#elseif defaultValue?has_content && defaultValue?seq_contains(itemValue)> checked="checked"</#if><#t/>
        <#if name?has_content> name="${name?html}"</#if> value="${itemValue?html}"<@commonElemEventAttribStr events=((events!{}) + (item.events!{})) />/><#lt/>
      <#if labelPosition != "before">${labelMarkup}</#if>
    </span>
    <#local sepClass = "">
    <#local sepClass = addClassArg(sepClass, "checkbox-item-separator")>
    <#local sepClass = addClassArg(sepClass, inlineClass)>
    <br<@fieldClassAttribStr class=sepClass /> /> <#-- controlled via css with display:none; TODO? maybe there's a better way -->
    <#if id?has_content>
      <#local currentId = id + "_" + (item_index + 2)?string>
    </#if>
  </#list>
  <#if multiMode>
    </div>
  </#if>
</#macro>

<#-- migrated from @renderRadioField form widget macro -->
<#assign field_radio_widget_defaultArgs = {
  "items":"", "id":"", "class":"", "alert":"", "currentValue":"", "defaultValue":"", "name":"", "events":{}, "tooltip":"", "title":"",
  "multiMode":true, "inlineItems":"", "fieldTitleBlank":false, "inlineLabel":false, "type":"", "passArgs":{}
}>
<#macro field_radio_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_radio_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !inlineItems?is_boolean>
    <#if inlineItems?has_content>
      <#-- do conversion to boolean so markup doesn't have to; but don't impose a default -->
      <#local inlineItems = inlineItems?boolean>
    </#if>
  </#if>
  <#if !type?has_content>
    <#local type = "default">
  </#if>
  <#local stylesPrefix = "field_radio_" + type?replace("-", "_")>
  <#local defaultClass = styles[stylesPrefix]!styles["field_radio_default"]!"">
  <#local class = addClassArgDefault(class, defaultClass)>
  <#local labelType = styles[stylesPrefix + "_labeltype"]!styles["field_radio_default_labeltype"]!"standard">
  <#local labelPosition = styles[stylesPrefix + "_labelposition"]!styles["field_radio_default_labelposition"]!"after">
  <@field_radio_markup_widget items=items id=id class=class alert=alert currentValue=currentValue defaultValue=defaultValue name=name 
    events=events tooltip=tooltip title=title multiMode=multiMode inlineItems=inlineItems fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel 
    type=type stylesPrefix=stylesPrefix labelType=labelType labelPosition=labelPosition origArgs=origArgs passArgs=passArgs><#nested></@field_radio_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_radio_markup_widget items="" id="" class="" alert="" currentValue="" defaultValue="" name="" events={} tooltip="" title="" multiMode=true inlineItems="" 
    type="default" stylesPrefix="" fieldTitleBlank=false inlineLabel=false 
    labelType="standard" labelPosition="after" origArgs={} passArgs={} catchArgs...>
  <#if !inlineItems?is_boolean>
    <#local inlineItems = true>
  </#if>

  <#if multiMode>
    <div<@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${id}_multi"</#if>>
    <#local class = ""> <#-- in multi mode, classes only on parent for now (?) -->
  </#if>
  <#if inlineItems>
    <#local inlineClass = "radio-item-inline">
  <#else>
    <#local inlineClass = "radio-item-noninline">
  </#if>
  <#local attribs = {}>
  <#local class = addClassArg(class, "radio-item")>
  <#local class = addClassArg(class, inlineClass)>
  <#if tooltip?has_content>
    <#-- redundant...
    <#local class = addClassArg(class, styles.field_radio_tooltip!styles.field_default_tooltip!"")>
    <#local attribs = attribs + styles.field_radio_tooltip_attribs!styles.field_default_tooltip_attribs!{}>-->
    <#local title = tooltip>
  </#if>
  <#local currentId = id>
  <#list items as item>
    <#-- Cato: NOTE: this macro must support both item.value and legacy item.key. Here they are the same thing. -->
    <#local itemValue = item.value!item.key!>
    <#local itemClass = class>
    <#local itemAlert = alert>
    
    <#local inputClass = "">
    <#local inputAlert = false>
    <#local inputAttribs = {}>
    <#local inputTitle = title>
    <#if item.tooltip?has_content || tooltip?has_content>
      <#local inputClass = addClassArg(inputClass, styles.field_radio_tooltip!styles.field_default_tooltip!"")>
      <#local inputAttribs = inputAttribs + styles.field_radio_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
      <#if item.tooltip?has_content>
        <#local inputTitle = item.tooltip>
      </#if>
    </#if>
    <span<@fieldClassAttribStr class=itemClass alert=itemAlert /><#if currentId?has_content> id="${currentId}_item"</#if>><#rt/>
      <#local labelMarkup>
        <#if item.description?has_content>
          <label class="radio-label-local"<#if currentId?has_content> for="${currentId}"</#if>>${item.description}</label>
        </#if>
      </#local>
      <#if labelPosition == "before">${labelMarkup}</#if>
      <input type="radio"<@fieldClassAttribStr class=inputClass alert=inputAlert /><#rt/>
        <@fieldElemAttribStr attribs=attribs+inputAttribs /><#t/>
        <#if inputTitle?has_content> title="${inputTitle}"</#if><#t/>
        <#if currentId?has_content> id="${currentId}"</#if><#t/>
        <#if item.checked?has_content><#if item.checked> checked="checked"</#if><#elseif currentValue?has_content><#if currentValue==itemValue> checked="checked"</#if>
        <#elseif defaultValue?has_content && defaultValue == itemValue> checked="checked"</#if><#t/>
        name="${name?html}" value="${(itemValue!"")?html}"<@commonElemEventAttribStr events=((events!{}) + (item.events!{})) />/><#rt/>
      <#if labelPosition != "before">${labelMarkup}</#if>
    </span>
    <#local sepClass = "">
    <#local sepClass = addClassArg(sepClass, "radio-item-separator")>
    <#local sepClass = addClassArg(sepClass, inlineClass)>
    <br<@fieldClassAttribStr class=sepClass /> /> <#-- controlled via css with display:none; TODO? maybe there's a better way -->
    <#if id?has_content>
      <#local currentId = id + "_" + (item_index + 2)?string>
    </#if>    
  </#list>
  <#if multiMode>
    </div>
  </#if>  
</#macro>

<#-- migrated from @renderFileField form widget macro -->
<#assign field_file_widget_defaultArgs = {
  "class":"", "alert":"", "name":"", "value":"", "size":"", "maxlength":"", "autocomplete":"", "id":"", "title":"", 
  "fieldTitleBlank":false, "inlineLabel":false, "passArgs":{}
}>
<#macro field_file_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_file_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@field_file_markup_widget class=class alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete id=id title=title fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_file_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_file_markup_widget class="" alert="" name="" value="" size="" maxlength="" autocomplete="" id="" title="" fieldTitleBlank=false inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <input type="file"<@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
</#macro>

<#-- migrated from @renderPasswordField form widget macro -->
<#assign field_password_widget_defaultArgs = {
  "class":"", "alert":"", "name":"", "value":"", "size":"", "maxlength":"", "id":"", "autocomplete":"", "title":"", "placeholder":"", 
  "fieldTitleBlank":false, "tooltip":"", "inlineLabel":false, "passArgs":{}
}>
<#macro field_password_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_password_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@field_password_markup_widget class=class alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete title=title placeholder=placeholder fieldTitleBlank=fieldTitleBlank tooltip=tooltip inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_password_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_password_markup_widget class="" alert="" name="" value="" size="" maxlength="" id="" autocomplete="" title="" placeholder="" fieldTitleBlank=false tooltip="" inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content>
    <#local class = addClassArg(class, styles.field_password_tooltip!styles.field_default_tooltip!"")>
    <#local attribs = attribs + styles.field_password_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
    <#local title = tooltip>
  </#if>
  <input type="password"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#if autocomplete?has_content> autocomplete="off"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#t/>
    <@fieldElemAttribStr attribs=attribs /><#t/>
    <#if title?has_content> title="${title}"</#if><#t/>
  /><#t/>
</#macro>

<#-- migrated from @renderResetField form widget macro -->
<#assign field_reset_widget_defaultArgs = {
  "class":"", "alert":"", "name":"", "text":"", "fieldTitleBlank":false, "inlineLabel":false, "passArgs":{}
}>
<#macro field_reset_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_reset_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@field_reset_markup_widget class=class alert=alert name=name text=text fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs/>
</#macro>

<#-- field markup - theme override -->
<#macro field_reset_markup_widget class="" alert="" name="" text="" fieldTitleBlank=false inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <input type="reset"<@fieldClassAttribStr class=class alert=alert /> name="${name}"<#if text?has_content> value="${text}"</#if>/>
</#macro>

<#-- migrated from @renderSubmitField form widget macro 
  * Parameters*
    buttonType    = (text-link|image|button, default: button) Logical button type (based on ofbiz form widget types)
    inputType     = the low-level <input> type attrib (within/depends on buttonType) -->
<#assign field_submit_widget_defaultArgs = {
  "buttonType":"", "class":"", "alert":"", "formName":"", "name":"", "events":{}, "imgSrc":"", "confirmation":"", 
  "containerId":"", "ajaxUrl":"", "text":"", "description":"", "fieldTitleBlank":false, "showProgress":"", "href":"", "inputType":"", 
  "disabled":false, "progressArgs":{}, "progressOptions":{}, "id":"", "inlineLabel":false, "passArgs":{}
}>
<#macro field_submit_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_submit_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>   
  <#if disabled>
    <#local class = addClassArg(class, styles.disabled!)>
  </#if>
  <#if progressArgs?has_content> <#-- note progressArgs or progressOptions could be empty strings -->
    <#if !progressArgs.progressOptions?? && progressOptions?has_content>
      <#local progressArgs = progressArgs + {"progressOptions":progressOptions}>
    </#if>
  <#elseif progressOptions?has_content>
    <#local progressArgs = {"progressOptions":progressOptions}>
  <#else>
    <#local progressArgs = {}>
  </#if>
  <@field_submit_markup_widget buttonType=buttonType class=class alert=alert formName=formName name=name events=events imgSrc=imgSrc confirmation=confirmation 
    containerId=containerId ajaxUrl=ajaxUrl text=text description=description fieldTitleBlank=fieldTitleBlank showProgress=showProgress href=href inputType=inputType 
    disabled=disabled progressArgs=progressArgs id=id inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_submit_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_submit_markup_widget buttonType="" class="" alert="" formName="" name="" events={} imgSrc="" confirmation="" 
    containerId="" ajaxUrl="" text="" fieldTitleBlank=false showProgress="" href="" inputType="" disabled=false 
    progressArgs={} id="" inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#-- Cato: to omit button (show progress only), we use empty title hack " " similar to what ofbiz does with hyperlinks with no label -->
  <#if (buttonType == "text-link" || buttonType != "image") && !(text?trim?has_content)>
    <#local buttonMarkup = "">
  <#else>
    <#local buttonMarkup>
      <#if buttonType == "text-link">
        <#-- FIXME?: slow, very specific check to test if link already has an action class.
            Currently, unsure how should have this default semantic, so play it safe. -->
        <#if styles.action_prefix?has_content && !containsStyleNamePrefix(class, styles.action_prefix)>
          <#local class = addClassArgDefault(class, styles.link_run_sys!)>
        </#if>
        <#-- FIXME: this static method of disabling links means the link loses information and not easily toggleable -->
        <#if disabled>
          <#local href = "javascript:void(0)">
        </#if>
        <a<@fieldClassAttribStr class=class alert=alert /> href="<#if (href?string == "false")>javascript:void(0)<#elseif href?has_content>${href}<#elseif formName?has_content>javascript:document.${formName}.submit()<#else>javascript:void(0)</#if>"<#if disabled> disabled="disabled"<#else><#if events?has_content><@commonElemEventAttribStr events=events /><#elseif confirmation?has_content> onclick="return confirm('${confirmation?js_string}');"</#if></#if><#if id?has_content> id="${id}"</#if>><#if text?has_content>${text}</#if></a>
      <#elseif buttonType == "image">
        <input type="<#if inputType?has_content>${inputType}<#else>image</#if>" src="${imgSrc}"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}"</#if><#if id?has_content> id="${id}"</#if>
        <#if description?has_content> alt="${description}"</#if>
        <#if disabled> disabled="disabled"<#else>
          <#if events?has_content><@commonElemEventAttribStr events=events /><#elseif confirmation?has_content>onclick="return confirm('${confirmation?js_string}');"</#if>
        </#if>/>
      <#else>
        <#-- FIXME?: slow, very specific check to test if link already has an action class.
            Currently, unsure how should have this default semantic, so play it safe. -->
        <#if styles.action_prefix?has_content && !containsStyleNamePrefix(class, styles.action_prefix)>
          <#local class = addClassArgDefault(class, styles.link_run_sys!)>
        </#if>
        <#-- TODO?: here there is no case to generate <button> (instead of <input type="button">) in case template needs... -->
        <input type="<#if inputType?has_content>${inputType}<#elseif containerId?has_content>button<#else>submit</#if>"<@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${id}"</#if>
        <#if name?has_content> name="${name}"</#if><#if text?has_content> value="${text}"</#if>
        <#if disabled> disabled="disabled"<#else>
          <#if events?has_content><@commonElemEventAttribStr events=events /><#else>
            <#if containerId?has_content> onclick="<#if confirmation?has_content>if (confirm('${confirmation?js_string}')) </#if>ajaxSubmitFormUpdateAreas('${containerId}', '${ajaxUrl}')"<#else>
            <#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}');"</#if>
            </#if>
          </#if>
        </#if>/>
      </#if>
    </#local>
  </#if>
  <#if progressArgs?has_content && ((progressArgs.enabled!true) != false)>
      <@field_submitarea_markup_widget_progress progressArgs=progressArgs passArgs=passArgs>${buttonMarkup}</@field_submitarea_markup_widget_progress>
  <#else>
      ${buttonMarkup}
  </#if>
</#macro>

<#assign field_submitarea_widget_defaultArgs = {
  "progressArgs":{}, "progressOptions":"", "inlineLabel":false, "passArgs":{}
}>
<#macro field_submitarea_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_submitarea_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if progressArgs?has_content> <#-- note progressArgs or progressOptions could be empty strings -->
    <#if !progressArgs.progressOptions?? && progressOptions?has_content>
      <#local progressArgs = progressArgs + {"progressOptions":progressOptions}>
    </#if>
  <#elseif progressOptions?has_content>
    <#local progressArgs = {"progressOptions":progressOptions}>
  <#else>
    <#local progressArgs = {}>
  </#if>
  <@field_submitarea_markup_widget progressArgs=progressArgs inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_submitarea_markup_widget>
</#macro>

<#-- submitarea widget markup - theme override -->
<#macro field_submitarea_markup_widget progressArgs={} inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#if progressArgs?has_content && ((progressArgs.enabled!true) != false)>
      <@field_submitarea_markup_widget_progress progressArgs=progressArgs inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_submitarea_markup_widget_progress>
  <#else>
      <#nested>
  </#if>
</#macro>

<#-- submitarea widget progress markup - theme override -->
<#macro field_submitarea_markup_widget_progress progressArgs={} inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#local progressOptions = progressArgs.progressOptions!{}>
  <#local nestedContent><#nested></#local>
  <#local rowClass>submit-progress-row<#if nestedContent?has_content> has-submit-button<#else> no-submit-button</#if></#local>
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
        <@progress id=progressOptions.progBarId type="info" containerClass="+${styles.hidden!}" progressOptions=progressOptions/>
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
       <@progressScript progressArgs=progressArgs htmlwrap=true passArgs=passArgs />
    </#if>
  </@row>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#assign field_textfind_widget_defaultArgs = {
  "name":"", "formName":"", "value":"", "defaultOption":"", "opEquals":"", "opBeginsWith":"", "opContains":"", "opIsEmpty":"", "opNotEqual":"", "opLike":"",
  "class":"", "id":"", "alert":"", "size":"", "maxlength":"", "autocomplete":"", "titleClass":"", "hideIgnoreCase":false, "ignoreCase":"", 
  "ignoreCaseMsg":"", "title":"", "tooltip":"", "fieldTitleBlank":false, "hideOptions":false, "inlineLabel":false, "origLabel":"", "collapse":false, "passArgs":{}
}>
<#macro field_textfind_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_textfind_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
    <#if !(opEquals?is_boolean && opEquals == false) && !opEquals?has_content>
    <#local opEquals = getPropertyMsg("conditional", "equals")!"">
  </#if>
  <#if !(opBeginsWith?is_boolean && opBeginsWith == false) && !opBeginsWith?has_content>
    <#local opBeginsWith = getPropertyMsg("conditional", "begins_with")!"">
  </#if>
  <#if !(opContains?is_boolean && opContains == false) && !opContains?has_content>
    <#local opContains = getPropertyMsg("conditional", "contains")!"">
  </#if>
  <#if !(opIsEmpty?is_boolean && opIsEmpty == false) && !opIsEmpty?has_content>
    <#local opIsEmpty = getPropertyMsg("conditional", "is_empty")!"">
  </#if>
  <#if !(opNotEqual?is_boolean && opNotEqual == false) && !opNotEqual?has_content>
    <#local opNotEqual = getPropertyMsg("conditional", "not_equal")!"">
  </#if>
  <#if !(opLike?is_boolean && opLike == false) && !opLike?has_content>
    <#local opLike = getPropertyMsg("conditional", "like")!"">
  </#if>
  <#if !(ignoreCaseMsg?is_boolean && ignoreCaseMsg == false) && !ignoreCaseMsg?has_content>
    <#local ignoreCaseMsg = getPropertyMsg("conditional", "ignore_case")!"">
  </#if>
  <#if !ignoreCase?is_boolean>
    <#local ignoreCase = true>
  </#if>
  <#if !autocomplete?is_boolean>
    <#if autocomplete?has_content>
      <#local autocomplete = false>
    <#else>
      <#local autocomplete = true>
    </#if>
  </#if>
  <@field_textfind_markup_widget name=name formName=formName value=value defaultOption=defaultOption opEquals=opEquals opBeginsWith=opBeginsWith opContains=opContains 
    opIsEmpty=opIsEmpty opNotEqual=opNotEqual opLike=opLike class=class id=id alert=alert size=size maxlength=maxlength autocomplete=autocomplete titleClass=titleClass 
    hideIgnoreCase=hideIgnoreCase ignoreCase=ignoreCase ignoreCaseMsg=ignoreCaseMsg title=title tooltip=tooltip fieldTitleBlank=fieldTitleBlank hideOptions=hideOptions 
    inlineLabel=inlineLabel origLabel=origLabel collapse=collapse origArgs=origArgs passArgs=passArgs><#nested></@field_textfind_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_textfind_markup_widget name="" formName="" value="" defaultOption="" opEquals="" opBeginsWith="" opContains="" 
    opIsEmpty="" opNotEqual="" opLike="" class="" id="" alert="" size="" maxlength="" autocomplete=true titleClass="" 
    hideIgnoreCase="" ignoreCase=false ignoreCaseMsg="" title="" tooltip="" fieldTitleBlank=false hideOptions=false inlineLabel=false origLabel="" collapse=false origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content> 
    <#local class = addClassArg(class, styles.field_textfind_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = attribs + styles.field_textfind_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
  </#if>
  <#-- Get heuristic-based current container sizes -->
  <#local absColSizes = getAbsContainerSizeFactors()>
  <#-- we say the parent is large if the ratio is greater than 6 -->
  <#local isLargeParent = (absColSizes.large > 6)>
  <#-- NOTE: all the code below assumes that the parent containers are using only
      large-x classes to split the page or form into columns (e.g., large-6, not medium-6) -->
  <@row collapse=collapse>
    <#if isLargeParent>
      <#-- Here, specify only large, so that when screen is smaller, the second cell will auto-wrap onto a second row (as usual)... -->
      <#local classOuter1 = "${styles.grid_large!}9"/>
      <#local classOuter2 = "${styles.grid_large!}3"/>
    <#else>
      <#-- FIXME?: I think this is not proper foundation (> 12 columns), but we want this to emulate the behavior
        when small screen takes effect (above) but no small-x classes are specified, which is more like this than it is making a separate row... right?
        NOTE: if someone changes this, you can't use #local capture, move nested to a separate macro call instead. 
            This hackish version simplifies it... -->
      <#local classOuter1 = "${styles.grid_large!}12"/>
      <#local classOuter2 = "${styles.grid_large!}12"/>
    </#if>
    <@cell class=classOuter1>
      <@row collapse=collapse>
        <#if !hideOptions>
          <#if isLargeParent>
            <#local class1 = "${styles.grid_small!}3 ${styles.grid_large!}3"/>
            <#local class2 = "${styles.grid_small!}9 ${styles.grid_large!}9"/>
          <#else>
            <#-- NOTE: this works together with the rest of the page. When small screen, usually
                the outer grid will create only one page column (small-12), and 3/9 looks fine.
                if we're in large screen, the page/form will be in two big columns (large-6), and since our parent
                is small, will look squished unless we set 4/8. -->
            <#local class1 = "${styles.grid_small!}3 ${styles.grid_large!}4"/>
            <#local class2 = "${styles.grid_small!}9 ${styles.grid_large!}8"/>
          </#if>
        <#else>
          <#local class1 = ""/>
          <#local class2 = "${styles.grid_small!}12 ${styles.grid_large!}12"/>
        </#if>      
        <#if !hideOptions>
          <#local newName = "${name}"/>
          <@cell class="${class1!}">
            <select<#if name?has_content> name="${name}_op"</#if> class="selectBox">
              <option value="equals"<#if defaultOption == "equals"> selected="selected"</#if>>${opEquals}</option>
              <option value="contains"<#if defaultOption == "contains"> selected="selected"</#if>>${opContains}</option>
              <option value="empty"<#if defaultOption == "empty"> selected="selected"</#if>>${opIsEmpty}</option>
              <option value="notEqual"<#if defaultOption == "notEqual"> selected="selected"</#if>>${opNotEqual}</option>
              <option value="like"<#if defaultOption == "like"> selected="selected"</#if>>${opBeginsWith} (${opLike})</option>
            </select>
          </@cell>
        <#else>
          <input type="hidden"<#if name?has_content> name="${name}_op"</#if> value="${defaultOption}"/><#rt/>
        </#if>
        <@cell class="${class2!}">
          <input type="text"<@fieldClassAttribStr class=class alert=alert /> name="${name}"<#if value?has_content> value="${value}"</#if><#rt/>
            <#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if !autocomplete> autocomplete="off"</#if><#t/>
            <@fieldElemAttribStr attribs=attribs /><#t/>
            <#if title?has_content> title="${title}"</#if>/><#t/>
        </@cell>
      </@row>
    </@cell>
    <@cell class=classOuter2>
        <#if hideIgnoreCase>
          <input type="hidden" name="${name}_ic" value="<#if ignoreCase>Y<#else></#if>"/><#rt/> 
        <#else>
          <div>
            <label for="${name}_ic"><input type="checkbox" id="${name}_ic" name="${name}_ic" value="Y"<#if ignoreCase> checked="checked"</#if>/>
            ${ignoreCaseMsg}</label>
            <#rt/>
          </div>
        </#if>
    </@cell>
  </@row>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#assign field_rangefind_widget_defaultArgs = {
  "class":"", "id":"", "alert":"", "name":"", "formName":"", "value":"", "size":"", "maxlength":"", "autocomplete":"", "titleClass":"", "defaultOptionFrom":"", 
  "opEquals":"", "opGreaterThan":"", "opGreaterThanEquals":"", "opLessThan":"", "opLessThanEquals":"", "opIsEmpty":"", "value2":"", 
  "defaultOptionThru":"", "title":"", "tooltip":"", "inlineLabel":false, "origLabel":"", "collapse":false, "passArgs":{}
}>
<#macro field_rangefind_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_rangefind_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !(opEquals?is_boolean && opEquals == false) && !opEquals?has_content>
    <#local opEquals = getPropertyMsg("conditional", "equals")!"">
  </#if>
  <#if !(opGreaterThan?is_boolean && opGreaterThan == false) && !opGreaterThan?has_content>
    <#local opGreaterThan = getPropertyMsg("conditional", "greater_than")!"">
  </#if>
  <#if !(opGreaterThanEquals?is_boolean && opGreaterThanEquals == false) && !opGreaterThanEquals?has_content>
    <#local opGreaterThanEquals = getPropertyMsg("conditional", "greater_than_equals")!"">
  </#if>
  <#if !(opLessThan?is_boolean && opLessThan == false) && !opLessThan?has_content>
    <#local opLessThan = getPropertyMsg("conditional", "less_than")!"">
  </#if>
  <#if !(opLessThanEquals?is_boolean && opLessThanEquals == false) && !opLessThanEquals?has_content>
    <#local opLessThanEquals = getPropertyMsg("conditional", "less_than_equals")!"">
  </#if>
  <#if !(opIsEmpty?is_boolean && opIsEmpty == false) && !opIsEmpty?has_content>
    <#local opIsEmpty = getPropertyMsg("conditional", "is_empty")!"">
  </#if>
  <#if !autocomplete?is_boolean>
    <#if autocomplete?has_content>
      <#local autocomplete = false>
    <#else>
      <#local autocomplete = true>
    </#if>
  </#if>
  <@field_rangefind_markup_widget class=class id=id alert=alert name=name formName=formName value=value size=size maxlength=maxlength 
      autocomplete=autocomplete titleClass=titleClass defaultOptionFrom=defaultOptionFrom opEquals=opEquals 
      opGreaterThan=opGreaterThan opGreaterThanEquals=opGreaterThanEquals opLessThan=opLessThan opLessThanEquals=opLessThanEquals opIsEmpty=opIsEmpty
      value2=value2 defaultOptionThru=defaultOptionThru title=title tooltip=tooltip
      inlineLabel=inlineLabel origLabel=origLabel collapse=collapse origArgs=origArgs passArgs=passArgs><#nested></@field_rangefind_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_rangefind_markup_widget class="" id="" alert="" name="" formName="" value="" size="" maxlength="" autocomplete=true titleClass="" 
    defaultOptionFrom="" opEquals="" opGreaterThan="" opGreaterThanEquals="" opLessThan="" opLessThanEquals="" opIsEmpty="" value2="" 
    defaultOptionThru="" title="" tooltip="" inlineLabel=false origLabel="" collapse=false origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content> 
    <#local class = addClassArg(class, styles.field_rangefind_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = attribs + styles.field_rangefind_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
  </#if>
  <#local class1="${styles.grid_small!}9 ${styles.grid_large!}9"/>
  <#local class2="${styles.grid_small!}3 ${styles.grid_large!}3"/>
  <@row collapse=collapse>
    <@cell class=class1>
      <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}_fld0_value"</#if><#rt/>
        <#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#t/>
        <#if !autocomplete> autocomplete="off"</#if><#t/>
        <@fieldElemAttribStr attribs=attribs /><#t/>
        <#if title?has_content> title="${title}"</#if>/><#t/>
    </@cell>
    <@cell class=class2>
      <#if titleClass?has_content>
        <span class="${titleClass}"><#rt/>
      </#if>
      <select<#if name?has_content> name="${name}_fld0_op"</#if> class="selectBox">
        <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${opEquals}</option>
        <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${opGreaterThan}</option>
        <option value="greaterThanEqualTo"<#if defaultOptionFrom=="greaterThanEqualTo"> selected="selected"</#if>>${opGreaterThanEquals}</option>
      </select><#rt/>
      <#if titleClass?has_content>
        </span><#rt/>
      </#if>
    </@cell>
  </@row><#rt/>
  <@row>
    <@cell class=class1>
      <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}_fld1_value"</#if><#rt/>
        <#if value2?has_content> value="${value2}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#t/>
        <#if !autocomplete> autocomplete="off"</#if><#t/>
        <@fieldElemAttribStr attribs=attribs /><#t/>
        <#if title?has_content> title="${title}"</#if>/><#t/>
    </@cell>
    <@cell class=class2>
      <#if titleClass?has_content>
        <span class="${titleClass}"><#rt/>
      </#if>
      <select<#if name?has_content> name="${name}_fld1_op"</#if> class="selectBox">
        <option value="lessThan"<#if defaultOptionThru == "lessThan"> selected="selected"</#if>>${opLessThan?html}</option>
        <option value="lessThanEqualTo"<#if defaultOptionThru == "lessThanEqualTo"> selected="selected"</#if>>${opLessThanEquals?html}</option>
      </select>
      <#if titleClass?has_content>
        </span>
      </#if>
    </@cell>
  </@row>
</#macro>

<#-- migrated from @renderHiddenField form widget macro -->
<#assign field_hidden_widget_defaultArgs = {
  "name":"", "value":"", "id":"", "events":{}, "inlineLabel":false, "passArgs":{}
}>
<#macro field_hidden_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_hidden_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@field_hidden_markup_widget name=name value=value id=id events=events inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs/>
</#macro>

<#-- field markup - theme override -->
<#macro field_hidden_markup_widget name="" value="" id="" events={} inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <input type="hidden" name="${name}"<#if value?has_content> value="${value}"</#if><#if id?has_content> id="${id}"</#if><#if events?has_content><@commonElemEventAttribStr events=events /></#if>/>
</#macro>

<#-- migrated from @renderDisplayField form widget macro -->
<#assign field_display_widget_defaultArgs = {
  "type":"", "imageLocation":"", "idName":"", "description":"", "title":"", "class":"", "id":"", "alert":"", "inPlaceEditorUrl":"", 
  "inPlaceEditorParams":"", "imageAlt":"", "collapse":false, "fieldTitleBlank":false, "tooltip":"", "inlineLabel":false, 
  "formatText":"", "passArgs":{}
}>
<#macro field_display_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_display_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <#if !formatText?is_boolean>
    <#local formatText = true>
  </#if>
  <@field_display_markup_widget type=type imageLocation=imageLocation id=id idName=idName description=description title=title class=class alert=alert inPlaceEditorUrl=inPlaceEditorUrl 
    inPlaceEditorParams=inPlaceEditorParams imageAlt=imageAlt collapse=false fieldTitleBlank=fieldTitleBlank tooltip=tooltip inlineLabel=inlineLabel formatText=formatText origArgs=origArgs passArgs=passArgs><#nested></@field_display_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_display_markup_widget type="" imageLocation="" idName="" id="" description="" title="" class="" alert="" inPlaceEditorUrl="" 
    inPlaceEditorParams="" imageAlt="" collapse=false fieldTitleBlank=false tooltip="" inlineLabel=false formatText=true origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content>
    <#local class = addClassArg(class, styles.field_display_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = attribs + styles.field_display_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
  </#if>
  <#local classes = compileClassArg(class)>
  <#local hasWrapper = (title?has_content || classes?has_content || id?has_content)>
  <#if hasWrapper>
    <div<#if id?has_content> id="${id}"</#if><#if classes?has_content> class="${classes}"</#if><#if title?has_content> title="${title}"</#if>><#rt/>
  </#if>
  <#if type?has_content && type == "image">
    <img src="${imageLocation}" alt="${imageAlt}"/><#lt/>
  <#else>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true" || title?has_content>
      <span<#if idName?has_content> id="cc_${idName}"</#if><#if title?has_content> title="${title}"</#if><@fieldClassAttribStr class=class alert=alert />><#t/>
    </#if>
    -->
    <#if description?has_content>
      <#if formatText>
        ${description?replace("\n", "<br />")}<#t/>
      <#else>
        ${description}<#t/>
      </#if>
    <#else>
      &nbsp;<#t/>
    </#if>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true">
      </span><#lt/>
    </#if>
    <#if inPlaceEditorUrl?has_content && idName?has_content>
      <@script>ajaxInPlaceEditDisplayField('cc_${idName}', '${inPlaceEditorUrl}', ${inPlaceEditorParams});</@script>
    </#if>-->
  </#if>
  <#if hasWrapper>
    </div><#lt/>
  </#if>
</#macro>

<#-- migrated from @renderField form widget macro -->
<#assign field_generic_widget_defaultArgs = {
  "text":"", "class":"", "id":"", "title":"", "tooltip":"", "inlineLabel":false, "passArgs":{}
}>
<#macro field_generic_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_generic_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#local origArgs = args>
  <@field_generic_markup_widget text=text class=class id=id title=title tooltip=tooltip inlineLabel=inlineLabel origArgs=origArgs passArgs=passArgs><#nested></@field_generic_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_generic_markup_widget text="" class="" id="" tooltip="" title="" inlineLabel=false origArgs={} passArgs={} catchArgs...>
  <#local attribs = {}>
  <#if tooltip?has_content>
    <#local class = addClassArg(class, styles.field_generic_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = attribs + styles.field_generic_tooltip_attribs!styles.field_default_tooltip_attribs!{}>
  </#if>
  <#local classes = compileClassArg(class)>
  <#local hasWrapper = (title?has_content || classes?has_content || id?has_content)>
  <#if hasWrapper>
    <div<#if id?has_content> id="${id}"</#if><#if classes?has_content> class="${classes}"</#if><#if title?has_content> title="${title}"</#if>><#rt/>
  </#if>
    <#if text?has_content>
      ${text}<#t/>
    <#else>
      <#nested><#t/>
    </#if>
  <#if hasWrapper>
    </div><#lt/>
  </#if>
</#macro>
