<#--
* 
* Individual field widgets HTML template include, standard Cato markup.
*
* Included by htmlForm.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#-- 
TODO: the parameters on these should be refined to be less ofbiz-like and more cato-like; but must
    be flexible enough to 100% allow calls from ofbiz widget lib macros.
TODO: _markup_widget macros should be cleaned up and logic moved to _widget macros -->

<#-- migrated from @renderClass form widget macro -->
<#macro fieldClassAttribStr class alert=false>
  <#if alert?string == "true">
    <#local class = addClassArg(class, "alert")>
  </#if>
  <@compiledClassAttribStr class=class /><#t>
</#macro>

<#-- migrated from @renderTextField form widget macro -->
<#assign field_input_widget_defaultArgs = {
  "name":"", "class":"", "alert":"", "value":"", "textSize":"", "maxlength":"", "id":"", "events":{}, "disabled":false, "ajaxUrl":"", 
  "ajaxEnabled":false, "mask":false, "clientAutocomplete":"", "placeholder":"", "tooltip":"", "collapse":false, "readonly":false, 
  "fieldTitleBlank":false, "inlineLabel":false
}>
<#macro field_input_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_input_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_input_markup_widget name=name class=class alert=alert value=value textSize=textSize maxlength=maxlength id=id events=events disabled=disabled ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled 
    mask=mask clientAutocomplete=clientAutocomplete placeholder=placeholder tooltip=tooltip collapse=collapse readonly=readonly fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=args><#nested></@field_input_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_input_markup_widget name="" class="" alert="" value="" textSize="" maxlength="" id="" events={} disabled=false ajaxUrl="" ajaxEnabled=false 
    mask=false clientAutocomplete="" placeholder="" tooltip="" collapse=false readonly=false fieldTitleBlank=false inlineLabel=false origArgs={} extraArgs...>
  <#if tooltip?has_content> 
    <#local class = addClassArg(class, "has-tip tip-right")>
  </#if>
  <#if mask?has_content && mask>
    <@script>
      jQuery(function($){jQuery("#${id}").mask("${mask!}");});
    </@script>
  </#if>
  <input type="text" name="${name?default("")?html}"<#t/>
    <#if tooltip?has_content> 
     data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"<#rt/>
     </#if><#rt/>
    <@fieldClassAttribStr class=class alert=alert />
    <#if value?has_content> value="${value}"</#if><#rt/>
    <#if textSize?has_content> size="${textSize}"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    <#if disabled?has_content && disabled> disabled="disabled"</#if><#rt/>
    <#if readonly?has_content && readonly> readonly="readonly"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if events?has_content><@commonElemEventAttribStr events=events /></#if><#rt/>
    <#if clientAutocomplete?has_content && clientAutocomplete=="false"> autocomplete="off"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#rt/>
  /><#t/>
  <#if ajaxUrl?has_content>
    <#local defaultMinLength = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")>
    <#local defaultDelay = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")>
    <@script>ajaxAutoCompleter('${ajaxUrl}', false, ${defaultMinLength!2}, ${defaultDelay!300});</@script><#lt/>
  </#if>
</#macro>

<#-- migrated from @renderTextareaField form widget macro -->
<#assign field_textarea_widget_defaultArgs = {
  "name":"", "class":"", "alert":"", "cols":"", "rows":"", "id":"", "readonly":"", "value":"", "visualEditorEnable":true, 
  "buttons":"", "language":"", "placeholder":"", "tooltip":"", "title":"", "fieldTitleBlank":false, "collapse":false, 
  "inlineLabel":false
}>
<#macro field_textarea_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_textarea_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_textarea_markup_widget name=name class=class alert=alert cols=cols rows=rows id=id readonly=readonly value=value visualEditorEnable=visualEditorEnable 
    buttons=buttons language=language placeholder=placeholder tooltip=tooltip title=title fieldTitleBlank=fieldTitleBlank collapse=collapse inlineLabel=inlineLabel origArgs=args><#nested></@field_textarea_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_textarea_markup_widget name="" class="" alert="" cols="" rows="" id="" readonly="" value="" visualEditorEnable=true 
    buttons="" language="" placeholder="" tooltip="" title="" fieldTitleBlank=false collapse=false inlineLabel=false origArgs={} extraArgs...>
  <#if tooltip?has_content> 
    <#local class = addClassArg(class, "has-tip tip-right")>
  </#if>
  <textarea name="${name}"<#t/>
    <@fieldClassAttribStr class=class alert=alert />
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
    <#if cols?has_content> cols="${cols}"</#if><#rt/>
    <#if rows?has_content> rows="${rows}"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if (readonly?is_string && readonly?has_content) || (readonly?is_boolean && readonly == true)> readonly="readonly"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#t/>
    ><#t/>
    <#if value?has_content>${value}<#else><#nested></#if><#t>
  </textarea><#lt/>
  
  <#--
  ToDo: Remove
  <#if visualEditorEnable?has_content>
    <@script src="/images/jquery/plugins/elrte-1.3/js/elrte.min.js" /><#rt/>
    <#if language?has_content && language != "en">
      <@script src="/images/jquery/plugins/elrte-1.3/js/i18n/elrte.${language!'en'}.js" /><#rt/>
    </#if>
    <link href="/images/jquery/plugins/elrte-1.3/css/elrte.min.css" rel="stylesheet" type="text/css">
    <@script>
      var opts = {
         cssClass : 'el-rte',
         lang     : '${language!"en"}',
         toolbar  : '${buttons?default("maxi")}',
         doctype  : '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">', //'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN">',
         cssfiles : ['/images/jquery/plugins/elrte-1.3/css/elrte-inner.css']
      }
      jQuery('#${id?default("")}').elrte(opts);
    </@script>
  </#if>
  -->
</#macro>

<#-- migrated from @renderDateTimeField form widget macro -->
<#assign field_datetime_widget_defaultArgs = {
  "name":"", "class":"", "title":"", "value":"", "size":"", "maxlength":"", "id":"", "dateType":"", "shortDateInput":false, 
  "timeDropdownParamName":"", "defaultDateTimeString":"", "localizedIconTitle":"", "timeDropdown":"", "timeHourName":"", 
  "classString":"", "hour1":"", "hour2":"", "timeMinutesName":"", "minutes":"", "isTwelveHour":"", "ampmName":"", "amSelected":"", 
  "pmSelected":"", "compositeType":"", "formName":"", "alert":false, "mask":"", "events":{}, "step":"", "timeValues":"", "tooltip":"", 
  "collapse":false, "fieldTitleBlank":false, "inlineLabel":false
}>
<#macro field_datetime_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_datetime_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_datetime_markup_widget name=name class=class title=title value=value size=size maxlength=maxlength id=id dateType=dateType shortDateInput=shortDateInput 
    timeDropdownParamName=timeDropdownParamName defaultDateTimeString=defaultDateTimeString localizedIconTitle=localizedIconTitle timeDropdown=timeDropdown timeHourName=timeHourName classString=classString 
    hour1=hour1 hour2=hour2 timeMinutesName=timeMinutesName minutes=minutes isTwelveHour=isTwelveHour ampmName=ampmName amSelected=amSelected pmSelected=pmSelected compositeType=compositeType formName=formName 
    alert=alert mask=mask events=events step=step timeValues=timeValues tooltip=tooltip collapse=false fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=args><#nested></@field_datetime_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_datetime_markup_widget name="" class="" title="" value="" size="" maxlength="" id="" dateType="" shortDateInput=false 
    timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" 
    hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName="" 
    alert=false mask="" events={} step="" timeValues="" tooltip="" collapse=false fieldTitleBlank=false inlineLabel=false origArgs={} extraArgs...>
  
  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- Note: ofbiz never handled dateType=="date" here because it pass shortDateInput=true in renderer instead-->
  <#-- These should be ~uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if (shortDateInput!false) == true>yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (((shortDateInput!false) == false) && dateType!="time")>

  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}11 ${styles.grid_cell!}">
          <#if tooltip?has_content> 
            <#local class = addClassArg(class, "has-tip tip-right")>
          </#if>
          <#local class = addClassArg(class, "${styles.grid_small!}3 ${styles.grid_cell!}")>
          <#if dateType == "time">
            <input type="text" name="${name}"<@fieldClassAttribStr class=class alert=alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}"</#if> /><#rt/>
          <#else>
            <input type="text" name="${name}_i18n"<@fieldClassAttribStr class=class alert=alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}_i18n"</#if> /><#rt/>

            <input type="hidden" name="${name}"<#if id?has_content> id="${id}"</#if><#if value?has_content> value="${value}"</#if> />
          </#if>
        </div>
        <div class="${styles.grid_small!}1 ${styles.grid_cell!}">
          <span class="postfix"><i class="${styles.icon!} ${styles.icon_calendar!}"></i></span>
        </div>
      <#if dateType != "time">
        <@script>
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
                
                var oldDate = "";
                var onFDatePopup = function(ev) {
                    oldDate = dateI18nToNorm(jQuery("#${id}_i18n").val());
                };
                var onFDateChange = function(ev) {
                  <#if useTsFormat>
                    jQuery("#${id}_i18n").val(dateNormToI18n(convertToDateTimeNorm(dateI18nToNorm(jQuery("#${id}_i18n").val()), oldDate)));
                  </#if>
                };
                
                <#if name??>
                    <#local dateElemJs>$("input[name='${name?html}_i18n']")</#local>
                <#else>
                    <#local dateElemJs>$("input")</#local>
                </#if>
                ${dateElemJs}.fdatepicker(${fdatepickerOptions}).on('changeDate', onFDateChange).on('show', onFDatePopup);
            });
        </@script>
      </#if>
  </div>
</#macro>

<#-- migrated from @renderDateFindField form widget macro -->
<#assign field_datefind_widget_defaultArgs = {
  "class":"", "alert":"", "name":"", "localizedInputTitle":"", "value":"", "value2":"", "size":"", "maxlength":"", "dateType":"", 
  "formName":"", "defaultDateTimeString":"", "imgSrc":"", "localizedIconTitle":"", "titleStyle":"", "defaultOptionFrom":"", 
  "defaultOptionThru":"", "opEquals":"", "opSameDay":"", "opGreaterThanFromDayStart":"", "opGreaterThan":"", "opGreaterThan":"", 
  "opLessThan":"", "opUpToDay":"", "opUpThruDay":"", "opIsEmpty":"", "inlineLabel":false
}>
<#macro field_datefind_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_datefind_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_datefind_markup_widget class=class alert=alert name=name localizedInputTitle=localizedInputTitle value=value value2=value2 size=size maxlength=maxlength dateType=dateType 
    formName=formName defaultDateTimeString=defaultDateTimeString imgSrc=imgSrc localizedIconTitle=localizedIconTitle titleStyle=titleStyle defaultOptionFrom=defaultOptionFrom defaultOptionThru=defaultOptionThru 
    opEquals=opEquals opSameDay=opSameDay opGreaterThanFromDayStart=opGreaterThanFromDayStart opGreaterThan=opGreaterThan opGreaterThan=opGreaterThan opLessThan=opLessThan opUpToDay=opUpToDay 
    opUpThruDay=opUpThruDay opIsEmpty=opIsEmpty inlineLabel=inlineLabel origArgs=args><#nested></@field_datefind_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_datefind_markup_widget class="" alert="" name="" localizedInputTitle="" value="" value2="" size="" maxlength="" dateType="" 
    formName="" defaultDateTimeString="" imgSrc="" localizedIconTitle="" titleStyle="" defaultOptionFrom="" defaultOptionThru="" 
    opEquals="" opSameDay="" opGreaterThanFromDayStart="" opGreaterThan="" opGreaterThan="" opLessThan="" opUpToDay="" opUpThruDay="" opIsEmpty="" inlineLabel=false origArgs={} extraArgs...>

  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- note: values of localizedInputTitle are: uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if dateType == "date">yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (dateType != "date" && dateType != "time")>
  
  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}5 ${styles.grid_cell!}">
          <#local class = addClassArg(class, "${styles.grid_small!}3 ${styles.grid_cell!}")>
          <input id="${name?html}_fld0_value" type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name?html}_fld0_value"</#if><#if localizedInputTitle?has_content> title="${localizedInputTitle}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if>/><#rt/>
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
        <@script>
            $(function() {
                var oldDate = "";
                var onFDatePopup = function(ev) {
                    oldDate = jQuery("#${name?html}_fld0_value").val();
                };
                var onFDateChange = function(ev) {
                  <#if useTsFormat>
                    jQuery("#${name?html}_fld0_value").val(convertToDateTimeNorm(jQuery("#${name?html}_fld0_value").val(), oldDate));
                  </#if>
                };
            
                <#if name??>
                    <#local dateElemJs>$('#${name?html}_fld0_value')</#local>
                <#else>
                    <#local dateElemJs>$('input')</#local>
                </#if>
                ${dateElemJs}.fdatepicker(${fdatepickerOptions}).on('changeDate', onFDateChange).on('show', onFDatePopup);
            });
        </@script>
      </#if>
  </div>
</#macro>

<#-- migrated from @renderDropDownField form widget macro -->
<#assign field_select_widget_defaultArgs = {
  "name":"", "class":"", "alert":"", "id":"", "multiple":"", "formName":"", "formId":"", "otherFieldName":"", "size":"", 
  "currentFirst":"", "currentValue":"", "currentDescription":"", "allowEmpty":"", "options":"", "fieldName":"", "otherFieldName":"", 
  "otherValue":"", "otherFieldSize":"", "dDFCurrent":"", "defaultValue":"", "ajaxOptions":"", "frequency":"", "minChars":"",
  "choices":"", "autoSelect":"", "partialSearch":"", "partialChars":"", "ignoreCase":"", "fullSearch":"", "events":{}, 
  "ajaxEnabled":false, "title":"", "tooltip":"", "description":"", "manualItems":false, "manualItemsOnly":false, "collapse":false, 
  "fieldTitleBlank":false, "inlineSelected":true, "asmSelectArgs":{}, "inlineLabel":false
}>
<#macro field_select_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_select_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
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
    collapse=collapse fieldTitleBlank=fieldTitleBlank inlineSelected=inlineSelected asmSelectArgs=asmSelectArgs inlineLabel=inlineLabel origArgs=args><#nested></@field_select_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_select_markup_widget name="" class="" alert="" id="" multiple=false formName="" formId="" otherFieldName="" size="" currentFirst=false 
    currentValue="" currentDescription="" allowEmpty=true options="" fieldName="" otherFieldName="" otherValue="" otherFieldSize="" 
    dDFCurrent="" defaultValue="" ajaxOptions="" frequency="" minChars="" choices="" autoSelect="" partialSearch="" partialChars="" 
    ignoreCase="" fullSearch="" events={} ajaxEnabled=false title="" tooltip="" description="" manualItems=false manualItemsOnly=false 
    collapse=false fieldTitleBlank=false inlineSelected=true asmSelectArgs={} inlineLabel=false origArgs={} extraArgs...>

    <#if tooltip?has_content>
      <#local class = addClassArg(class, "has-tip tip-right")>
    </#if>
    <select name="${name!""}<#rt/>"<@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${id}"</#if><#if multiple> multiple="multiple"</#if><#if (otherFieldSize > 0)> onchange="process_choice(this,document.${formName}.${otherFieldName})"</#if><#if events?has_content><@commonElemEventAttribStr events=events /></#if><#rt/><#--<#if size?has_content> size="${size}"</#if>-->
    <#if title?has_content> title="${title}"<#elseif tooltip?has_content> title="${tooltip}"</#if>
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true"</#if><#rt/>>
    <#if !manualItemsOnly>  
      <#if currentFirst && currentValue?has_content && !multiple>
        <option selected="selected" value="${currentValue}">${currentDescription}</option><#rt/>
        <option value="${currentValue}">---</option><#rt/>
      </#if>
      <#if allowEmpty || (!manualItems && !options?has_content)>
        <option value="">&nbsp;</option>
      </#if>
      <#list options as item>
        <#-- Cato: NOTE: this macro must support both item.value and legacy item.key. Here they are the same thing. -->
        <#local itemValue = item.value!item.key!>
        <#local itemMarkedSelected = item.selected?? && ((item.selected?is_boolean && item.selected == true) || (!item.selected?is_boolean && item.selected?has_content))>
        <#if multiple>
          <option<#if currentValue?has_content && itemMarkedSelected> selected="selected"<#elseif !currentValue?has_content && defaultValue?has_content && defaultValue == itemValue> selected="selected"</#if> value="${itemValue}">${item.description!}</option><#rt/>
        <#else>
          <option<#if currentValue?has_content && currentValue == itemValue && inlineSelected> selected="selected"<#elseif !currentValue?has_content && defaultValue?has_content && defaultValue == itemValue> selected="selected"</#if> value="${itemValue}">${item.description!}</option><#rt/>
        </#if>
      </#list>
    </#if>
      <#nested>
    </select>
  <#if otherFieldName?has_content>
    <noscript><input type='text' name='${otherFieldName}' /></noscript>
    <@script>
      disa = ' disabled';
      if(other_choice(document.${formName}.${fieldName}))
        disa = '';
      document.write("<input type='text' name='${otherFieldName}' value='${otherValue?js_string}' size='${otherFieldSize}'"+disa+" onfocus='check_choice(document.${formName}.${fieldName})' />");
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
    <@asmSelectScript id=id formName=formName formId=formId title=asmtitle args=asmSelectArgs />
  </#if>
</#macro>

<#assign field_option_widget_defaultArgs = {
  "text":"", "value":"", "selected":false
}>
<#macro field_option_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_option_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if !text?has_content>
    <#local text><#nested></#local>
  </#if>
  <@field_option_markup_widget text=text value=value selected=selected origArgs=args/>
</#macro>

<#-- field markup - theme override -->
<#macro field_option_markup_widget text="" value="" selected=false origArgs={} extraArgs...>
   <option value="${value}"<#if selected> selected="selected"</#if>>${text}</option><#t>
</#macro>    

<#-- migrated from @renderLookupField form widget macro -->
<#assign field_lookup_widget_defaultArgs = {
  "name":"", "formName":"", "fieldFormName":"", "class":"", "alert":"false", "value":"", "size":"", "maxlength":"", "id":"", 
  "events":{}, "readonly":false, "autocomplete":"", "descriptionFieldName":"", "targetParameterIter":"", "imgSrc":"", "ajaxUrl":"", 
  "ajaxEnabled":javaScriptEnabled, "presentation":"layer", "width":"", "height":"", "position":"", "fadeBackground":"true", 
  "clearText":"", "showDescription":"", "initiallyCollapsed":"", "lastViewName":"main", "title":"", "fieldTitleBlank":false, 
  "inlineLabel":false
}>
<#macro field_lookup_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_lookup_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_lookup_markup_widget name=name formName=formName fieldFormName=fieldFormName class=class alert=alert value=value size=size 
    maxlength=maxlength id=id events=events readonly=readonly autocomplete=autocomplete descriptionFieldName=descriptionFieldName 
    targetParameterIter=targetParameterIter imgSrc=imgSrc ajaxUrl=ajaxUrl ajaxEnabled=ajaxEnabled presentation=presentation width=width 
    height=height position=position fadeBackground=fadeBackground clearText=clearText showDescription=showDescription initiallyCollapsed=initiallyCollapsed 
    lastViewName=lastViewName title=title fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=args><#nested></@field_lookup_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_lookup_markup_widget name="" formName="" fieldFormName="" class="" alert="false" value="" size="" 
    maxlength="" id="" events={} readonly=false autocomplete="" descriptionFieldName="" 
    targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=javaScriptEnabled presentation="layer" width="" 
    height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" 
    lastViewName="main" title="" fieldTitleBlank=false inlineLabel=false origArgs={} extraArgs...>
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
      <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if>
        <#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#rt/>
        <#if readonly?has_content && readonly> readonly="readonly"</#if><#rt/><#if events?has_content><@commonElemEventAttribStr events=events /></#if><#rt/><#rt/>
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
  <#if ajaxEnabled?has_content && ajaxEnabled && (presentation?has_content && presentation == "window")>
    <#if ajaxUrl?index_of("_LAST_VIEW_NAME_") < 0>
      <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
    </#if>
    <@script>ajaxAutoCompleter('${ajaxUrl}', ${showDescription}, ${defaultMinLength!2}, ${defaultDelay!300});</@script><#t/>
  </#if>
</#macro>

<#-- migrated from @renderCheckField (a.k.a. @renderCheckBox) form widget macro -->
<#assign field_checkbox_widget_defaultArgs = {
  "items":[], "id":"", "class":"", "alert":"", "allChecked":"", "currentValue":"", "defaultValue":"", "name":"", "events":{}, 
  "tooltip":"", "fieldTitleBlank":false, "multiMode":true, "inlineItems":"", "inlineLabel":false
}>
<#macro field_checkbox_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_checkbox_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
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
  <@field_checkbox_markup_widget items=items id=id class=class alert=alert allChecked=allChecked 
    currentValue=currentValue defaultValue=defaultValue name=name events=events tooltip=tooltip multiMode=multiMode 
    fieldTitleBlank=fieldTitleBlank inlineItems=inlineItems inlineLabel=inlineLabel origArgs=args><#nested></@field_checkbox_markup_widget>
</#macro>

<#-- field markup - theme override 
     FIXME: the styling for these is strange, can't get it to work no matter what -->
<#macro field_checkbox_markup_widget items=[] id="" class="" alert="" allChecked="" currentValue=[] defaultValue=[] name="" 
    events={} tooltip="" fieldTitleBlank=false multiMode=true inlineItems="" inlineLabel=false origArgs={} extraArgs...>
  <#if !inlineItems?is_boolean>
    <#local inlineItems = true>
  </#if>

  <#if multiMode>
    <div<@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${id}_multi"</#if>>
    <#local class = ""> <#-- in multi mode, classes only on parent for now (?) -->
  </#if>

  <#if inlineItems>
    <#local inlineClass = "checkbox-item-inline">
  <#else>
    <#local inlineClass = "checkbox-item-noninline">
  </#if>
  <#local class = addClassArg(class, "checkbox-item")>
  <#local class = addClassArg(class, inlineClass)>
  <#local class = addClassArgDefault(class, "${styles.switch} ${styles.small}")>

  <#-- Cato: must have id on each elem or else the foundation switches break 
       The first item receives the exact id passed to macro because this is what original ofbiz macros expect
       (and is logical for single items at least). -->
  <#local currentId = id>
  <#list items as item>
    <#local itemValue = item.value!"">
    <#local itemClass = class>
    <#local itemAlert = alert>
    
    <#local inputClass = "">
    <#local inputAlert = false>
    <#if item.tooltip?has_content || tooltip?has_content>
      <#local inputClass = addClassArg(inputClass, "has-tip tip-right")>
    </#if>
    <span<@fieldClassAttribStr class=itemClass alert=itemAlert /><#if currentId?has_content> id="${currentId}_item"</#if>>
      <input type="checkbox"<@fieldClassAttribStr class=inputClass alert=inputAlert /><#if currentId?has_content> id="${currentId}"</#if><#rt/>
        <#if item.tooltip?has_content || tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="<#if item.tooltip?has_content>${item.tooltip}<#else>${tooltip!}</#if>"</#if><#rt/>
        <#if item.checked?has_content><#if item.checked> checked="checked"</#if><#elseif allChecked?has_content><#if allChecked> checked="checked"</#if>
        <#elseif currentValue?has_content && currentValue?seq_contains(itemValue)> checked="checked"
        <#elseif defaultValue?has_content && defaultValue?seq_contains(itemValue)> checked="checked"</#if> 
        name="${name?html}" value="${itemValue?html}"<@commonElemEventAttribStr events=((events!{}) + (item.events!{})) />/><#rt/>
      <label<#if currentId?has_content> for="${currentId}"</#if>></label>
      <#-- FIXME?: description destroys field if put inside <label> above... also <label> has to be separate from input (not parent)... ? -->
      <#if item.description?has_content><span class="checkbox-label-local">${item.description}</span></#if>
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
  "items":"", "id":"", "class":"", "alert":"", "currentValue":"", "defaultValue":"", "name":"", "events":{}, "tooltip":"", 
  "multiMode":true, "inlineItems":"", "fieldTitleBlank":false, "inlineLabel":false
}>
<#macro field_radio_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_radio_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if !inlineItems?is_boolean>
    <#if inlineItems?has_content>
      <#-- do conversion to boolean so markup doesn't have to; but don't impose a default -->
      <#local inlineItems = inlineItems?boolean>
    </#if>
  </#if>
  <@field_radio_markup_widget items=items id=id class=class alert=alert currentValue=currentValue defaultValue=defaultValue name=name 
    events=events tooltip=tooltip multiMode=multiMode inlineItems=inlineItems fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=args><#nested></@field_radio_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_radio_markup_widget items="" id="" class="" alert="" currentValue="" defaultValue="" name="" events={} tooltip="" multiMode=true inlineItems="" fieldTitleBlank=false inlineLabel=false origArgs={} extraArgs...>
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

  <#local class = addClassArg(class, "radio-item")>
  <#local class = addClassArg(class, inlineClass)>
  <#if tooltip?has_content>
    <#local class = addClassArg(class, "has-tip tip-right")>
  </#if>
  <#local currentId = id>
  <#list items as item>
    <#-- Cato: NOTE: this macro must support both item.value and legacy item.key. Here they are the same thing. -->
    <#local itemValue = item.value!item.key!>
    <#local itemClass = class>
    <#local itemAlert = alert>
    
    <#local inputClass = "">
    <#local inputAlert = false>
    <#if item.tooltip?has_content || tooltip?has_content>
      <#local inputClass = addClassArg(inputClass, "has-tip tip-right")>
    </#if>
    <span<@fieldClassAttribStr class=itemClass alert=itemAlert /><#if currentId?has_content> id="${currentId}_item"</#if>><#rt/>
      <input type="radio"<@fieldClassAttribStr class=inputClass alert=inputAlert /><#if currentId?has_content> id="${currentId}"</#if><#if item.tooltip?has_content || tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="<#if item.tooltip?has_content>${item.tooltip}<#else>${tooltip!}</#if>"</#if><#rt/>
        <#if item.checked?has_content><#if item.checked> checked="checked"</#if><#elseif currentValue?has_content><#if currentValue==itemValue> checked="checked"</#if>
        <#elseif defaultValue?has_content && defaultValue == itemValue> checked="checked"</#if> 
        name="${name?html}" value="${itemValue!""?html}"<@commonElemEventAttribStr events=((events!{}) + (item.events!{})) />/><#rt/>
      <#if item.description?has_content>
        <label class="radio-label-local"<#if currentId?has_content> for="${currentId}"</#if>>${item.description}</span>
      </#if>
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
  "fieldTitleBlank":false, "inlineLabel":false
}>
<#macro field_file_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_file_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_file_markup_widget class=class alert=alert name=name value=value size=size maxlength=maxlength autocomplete=autocomplete id=id title=title fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=args><#nested></@field_file_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_file_markup_widget class="" alert="" name="" value="" size="" maxlength="" autocomplete="" id="" title="" fieldTitleBlank=false inlineLabel=false origArgs={} extraArgs...>
  <input type="file"<@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${id}"</#if><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
</#macro>

<#-- migrated from @renderPasswordField form widget macro -->
<#assign field_password_widget_defaultArgs = {
  "class":"", "alert":"", "name":"", "value":"", "size":"", "maxlength":"", "id":"", "autocomplete":"", "title":"", "placeholder":"", 
  "fieldTitleBlank":false, "tooltip":"", "inlineLabel":false
}>
<#macro field_password_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_password_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_password_markup_widget class=class alert=alert name=name value=value size=size maxlength=maxlength id=id autocomplete=autocomplete title=title placeholder=placeholder fieldTitleBlank=fieldTitleBlank tooltip=tooltip inlineLabel=inlineLabel origArgs=args><#nested></@field_password_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_password_markup_widget class="" alert="" name="" value="" size="" maxlength="" id="" autocomplete="" title="" placeholder="" fieldTitleBlank=false tooltip="" inlineLabel=false origArgs={} extraArgs...>
  <#if tooltip?has_content>
    <#local class = addClassArg(class, "has-tip tip-right")>
  </#if>
  <input type="password"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if>
  <#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#if autocomplete?has_content> autocomplete="off"</#if> 
  <#if placeholder?has_content> placeholder="${placeholder}"</#if>
  <#if tooltip?has_content> data-tooltip aria-haspopup="true" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
  />
</#macro>

<#-- migrated from @renderResetField form widget macro -->
<#assign field_reset_widget_defaultArgs = {
  "class":"", "alert":"", "name":"", "text":"", "fieldTitleBlank":false, "inlineLabel":false
}>
<#macro field_reset_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_reset_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_reset_markup_widget class=class alert=alert name=name text=text fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=args/>
</#macro>

<#-- field markup - theme override -->
<#macro field_reset_markup_widget class="" alert="" name="" text="" fieldTitleBlank=false inlineLabel=false origArgs={} extraArgs...>
  <input type="reset"<@fieldClassAttribStr class=class alert=alert /> name="${name}"<#if text?has_content> value="${text}"</#if>/>
</#macro>

<#-- migrated from @renderSubmitField form widget macro 
  * Parameters*
    buttonType    = [text-link|image|button], default button - logical button type (based on ofbiz form widget types)
    inputType     = the low-level <input> type attrib (within/depends on buttonType) -->
<#assign field_submit_widget_defaultArgs = {
  "buttonType":"", "class":"", "alert":"", "formName":"", "name":"", "events":{}, "imgSrc":"", "confirmation":"", 
  "containerId":"", "ajaxUrl":"", "text":"", "description":"", "fieldTitleBlank":false, "showProgress":"", "href":"", "inputType":"", 
  "disabled":false, "progressArgs":{}, "progressOptions":{}, "id":"", "inlineLabel":false
}>
<#macro field_submit_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_submit_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>   
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
    disabled=disabled progressArgs=progressArgs id=id inlineLabel=inlineLabel origArgs=args><#nested></@field_submit_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_submit_markup_widget buttonType="" class="" alert="" formName="" name="" events={} imgSrc="" confirmation="" 
    containerId="" ajaxUrl="" text="" fieldTitleBlank=false showProgress="" href="" inputType="" disabled=false 
    progressArgs={} id="" inlineLabel=false origArgs={} extraArgs...>
  <#-- Cato: to omit button (show progress only), we use empty title hack " " similar to what ofbiz does with hyperlinks with no label -->
  <#if (buttonType == "text-link" || buttonType != "image") && !(text?trim?has_content)>
    <#local buttonMarkup = "">
  <#else>
    <#local buttonMarkup>
      <#if buttonType == "text-link">
        <#local class = addClassArgDefault(class, styles.link_action!)>
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
        <#local class = addClassArgDefault(class, styles.link_action!)>
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
      <@field_submitarea_markup_widget_progress progressArgs=progressArgs>${buttonMarkup}</@field_submitarea_markup_widget_progress>
  <#else>
      ${buttonMarkup}
  </#if>
</#macro>

<#assign field_submitarea_widget_defaultArgs = {
  "progressArgs":{}, "progressOptions":"", "inlineLabel":false
}>
<#macro field_submitarea_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_submitarea_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <#if progressArgs?has_content> <#-- note progressArgs or progressOptions could be empty strings -->
    <#if !progressArgs.progressOptions?? && progressOptions?has_content>
      <#local progressArgs = progressArgs + {"progressOptions":progressOptions}>
    </#if>
  <#elseif progressOptions?has_content>
    <#local progressArgs = {"progressOptions":progressOptions}>
  <#else>
    <#local progressArgs = {}>
  </#if>
  <@field_submitarea_markup_widget progressArgs=progressArgs inlineLabel=inlineLabel origArgs=args><#nested></@field_submitarea_markup_widget>
</#macro>

<#-- submitarea widget markup - theme override -->
<#macro field_submitarea_markup_widget progressArgs={} inlineLabel=false origArgs={} extraArgs...>
  <#if progressArgs?has_content && ((progressArgs.enabled!true) != false)>
      <@field_submitarea_markup_widget_progress progressArgs=progressArgs inlineLabel=inlineLabel origArgs=origArgs><#nested></@field_submitarea_markup_widget_progress>
  <#else>
      <#nested>
  </#if>
</#macro>

<#-- submitarea widget progress markup - theme override -->
<#macro field_submitarea_markup_widget_progress progressArgs={} inlineLabel=false origArgs={} extraArgs...>
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
       <@progressScript progressArgs=progressArgs htmlwrap=true />
    </#if>
  </@row>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#assign field_textfind_widget_defaultArgs = {
  "name":"", "value":"", "defaultOption":"", "opEquals":"", "opBeginsWith":"", "opContains":"", "opIsEmpty":"", "opNotEqual":"", 
  "class":"", "alert":"", "size":"", "maxlength":"", "autocomplete":"", "titleStyle":"", "hideIgnoreCase":"", "ignCase":"", 
  "ignoreCase":"", "title":"", "fieldTitleBlank":false, "inlineLabel":false
}>
<#macro field_textfind_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_textfind_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_textfind_markup_widget name=name value=value defaultOption=defaultOption opEquals=opEquals opBeginsWith=opBeginsWith opContains=opContains 
    opIsEmpty=opIsEmpty opNotEqual=opNotEqual class=class alert=alert size=size maxlength=maxlength autocomplete=autocomplete titleStyle=titleStyle 
    hideIgnoreCase=hideIgnoreCase ignCase=ignCase ignoreCase=ignoreCase title=title fieldTitleBlank=fieldTitleBlank inlineLabel=inlineLabel origArgs=args><#nested></@field_textfind_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_textfind_markup_widget name="" value="" defaultOption="" opEquals="" opBeginsWith="" opContains="" 
    opIsEmpty="" opNotEqual="" class="" alert="" size="" maxlength="" autocomplete="" titleStyle="" 
    hideIgnoreCase="" ignCase="" ignoreCase="" title="" fieldTitleBlank=false inlineLabel=false origArgs={} extraArgs...>

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
        <select<#if name?has_content> name="${name}_op"</#if> class="selectBox"><#rt/>
            <option value="equals"<#if defaultOption=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
            <option value="like"<#if defaultOption=="like"> selected="selected"</#if>>${opBeginsWith}</option><#rt/>
            <option value="contains"<#if defaultOption=="contains"> selected="selected"</#if>>${opContains}</option><#rt/>
            <option value="empty"<#rt/><#if defaultOption=="empty"> selected="selected"</#if>>${opIsEmpty}</option><#rt/>
            <option value="notEqual"<#if defaultOption=="notEqual"> selected="selected"</#if>>${opNotEqual}</option><#rt/>
        </select>
      </@cell>
    <#else>
      <input type="hidden"<#if name?has_content> name="${name}_op"</#if> value="${defaultOption}"/><#rt/>
    </#if>
      <@cell class="${class2!}">
        <input type="text"<@fieldClassAttribStr class=class alert=alert /> name="${name}"<#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
      </@cell>
      <@cell class="${class3!}"> 
        <#if hideIgnoreCase>
          <input type="hidden" name="${name}_ic" value=<#if ignCase>"Y"<#else> ""</#if>/><#rt/>
        <#else>
          <div>
            <label for="${name}_ic"><input type="checkbox" id="${name}_ic" name="${name}_ic" value="Y"<#if ignCase> checked="checked"</#if>/>
            ${ignoreCase!}</label>
            <#rt/>
          </div>
        </#if>
      </@cell>
  </@row>
</#macro>

<#-- migrated from @renderRangeFindField form widget macro -->
<#assign field_rangefind_widget_defaultArgs = {
  "class":"", "alert":"", "name":"", "value":"", "size":"", "maxlength":"", "autocomplete":"", "titleStyle":"", "defaultOptionFrom":"", 
  "opEquals":"", "opGreaterThan":"", "opGreaterThanEquals":"", "opLessThan":"", "opLessThanEquals":"", "value2":"", 
  "defaultOptionThru":"", "inlineLabel":false
}>
<#macro field_rangefind_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_rangefind_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_rangefind_markup_widget class=class alert=alert name=name value=value size=size maxlength=maxlength 
      autocomplete=autocomplete titleStyle=titleStyle defaultOptionFrom=defaultOptionFrom opEquals=opEquals 
      opGreaterThan=opGreaterThan opGreaterThanEquals=opGreaterThanEquals opLessThan=opLessThan opLessThanEquals=opLessThanEquals
      value2=value2 defaultOptionThru=defaultOptionThru inlineLabel=inlineLabel origArgs=args><#nested></@field_rangefind_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_rangefind_markup_widget class="" alert="" name="" value="" size="" maxlength="" autocomplete="" titleStyle="" 
    defaultOptionFrom="" opEquals="" opGreaterThan="" opGreaterThanEquals="" opLessThan="" opLessThanEquals="" value2="" 
    defaultOptionThru="" inlineLabel=false origArgs={} extraArgs...>
  <#local class1="${styles.grid_small!}9 ${styles.grid_large!}9"/>
  <#local class2="${styles.grid_small!}3 ${styles.grid_large!}3"/>
  <@row collapse=collapse!false>
    <@cell class=class1>
      <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}_fld0_value"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
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
      <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${name}_fld1_value"</#if><#if value2?has_content> value="${value2}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
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

<#-- migrated from @renderHiddenField form widget macro -->
<#assign field_hidden_widget_defaultArgs = {
  "name":"", "value":"", "id":"", "events":{}, "inlineLabel":false
}>
<#macro field_hidden_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_hidden_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_hidden_markup_widget name=name value=value id=id events=events inlineLabel=inlineLabel origArgs=args/>
</#macro>

<#-- field markup - theme override -->
<#macro field_hidden_markup_widget name="" value="" id="" events={} inlineLabel=false origArgs={} extraArgs...>
  <input type="hidden" name="${name}"<#if value?has_content> value="${value}"</#if><#if id?has_content> id="${id}"</#if><#if events?has_content><@commonElemEventAttribStr events=events /></#if>/>
</#macro>

<#-- migrated from @renderDisplayField form widget macro -->
<#assign field_display_widget_defaultArgs = {
  "type":"", "imageLocation":"", "idName":"", "description":"", "title":"", "class":"", "alert":"", "inPlaceEditorUrl":"", 
  "inPlaceEditorParams":"", "imageAlt":"", "collapse":false, "fieldTitleBlank":false, "tooltip":"", "inlineLabel":false
}>
<#macro field_display_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_display_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_display_markup_widget type=type imageLocation=imageLocation idName=idName description=description title=title class=class alert=alert inPlaceEditorUrl=inPlaceEditorUrl 
    inPlaceEditorParams=inPlaceEditorParams imageAlt=imageAlt collapse=false fieldTitleBlank=fieldTitleBlank tooltip=tooltip inlineLabel=inlineLabel origArgs=args><#nested></@field_display_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_display_markup_widget type="" imageLocation="" idName="" description="" title="" class="" alert="" inPlaceEditorUrl="" 
    inPlaceEditorParams="" imageAlt="" collapse=false fieldTitleBlank=false tooltip="" inlineLabel=false origArgs={} extraArgs...>
  <#if type?has_content && type=="image">
    <img src="${imageLocation}" alt="${imageAlt}"><#lt/>
  <#else>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true" || title?has_content>
      <span<#if idName?has_content> id="cc_${idName}"</#if><#if title?has_content> title="${title}"</#if><@fieldClassAttribStr class=class alert=alert />><#t/>
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
      <@script>ajaxInPlaceEditDisplayField('cc_${idName}', '${inPlaceEditorUrl}', ${inPlaceEditorParams});</@script>
    </#if>-->
  </#if>
  <#-- TODO: better tooltips -->
  <#if tooltip?has_content>
    <span class="tooltip">${tooltip}</span>
  </#if>
</#macro>

<#-- migrated from @renderField form widget macro -->
<#assign field_generic_widget_defaultArgs = {
  "text":"", "tooltip":"", "inlineLabel":false
}>
<#macro field_generic_widget args={} inlineArgs...>
  <#local args = mergeArgMaps(args, inlineArgs, catoStdTmplLib.field_generic_widget_defaultArgs)>
  <#local dummy = localsPutAll(args)>
  <@field_generic_markup_widget text=text tooltip=tooltip inlineLabel=inlineLabel origArgs=args><#nested></@field_generic_markup_widget>
</#macro>

<#-- field markup - theme override -->
<#macro field_generic_markup_widget text="" tooltip="" inlineLabel=false origArgs={} extraArgs...>
  <#if text?has_content>
    ${text}<#t>
  <#else>
    <#nested><#t>
  </#if>
  <#-- TODO: better tooltips -->
  <#if tooltip?has_content>
    <span class="tooltip">${tooltip}</span>
  </#if>
</#macro>
