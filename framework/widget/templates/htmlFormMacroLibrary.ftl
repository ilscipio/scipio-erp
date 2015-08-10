<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#macro renderField text>
  <#if text??>
    ${text}<#lt/>
  </#if>
</#macro>

<#macro renderDisplayField type imageLocation idName description title class alert inPlaceEditorUrl="" inPlaceEditorParams="">
  <#if type?has_content && type=="image">
    <img src="${imageLocation}" alt=""><#lt/>
  <#else>
    <#--
    <#if inPlaceEditorUrl?has_content || class?has_content || alert=="true" || title?has_content>
      <span <#if idName?has_content>id="cc_${idName}"</#if> <#if title?has_content>title="${title}"</#if> <@renderClass class alert />><#t/>
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
<#macro renderHyperlinkField></#macro>

<#macro renderTextField name className alert value textSize maxlength id event="" action="" disabled=false ajaxUrl="" ajaxEnabled=false mask=false clientAutocomplete="" placeholder="" tooltip="">
  <#if mask?has_content && mask>
    <script type="text/javascript">
      jQuery(function($){jQuery("#${id}").mask("${mask!}");});
    </script>
  </#if>
  <input type="text" name="${name?default("")?html}"<#t/>
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
    <@renderClass className alert />
    <#if value?has_content> value="${value}"</#if><#rt/>
    <#if textSize?has_content> size="${textSize}"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    <#if disabled?has_content && disabled> disabled="disabled"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if event?has_content && action?has_content> ${event}="${action}"</#if><#rt/>
    <#if clientAutocomplete?has_content && clientAutocomplete=="false"> autocomplete="off"</#if><#rt/>
    <#if placeholder?has_content> placeholder="${placeholder}"</#if><#rt/>
  /><#t/>
  <#if ajaxUrl?has_content>
    <#assign defaultMinLength = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")>
    <#assign defaultDelay = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")>
    <script language="JavaScript" type="text/javascript">ajaxAutoCompleter('${ajaxUrl}', false, ${defaultMinLength!2}, ${defaultDelay!300});</script><#lt/>
  </#if>
</#macro>

<#macro renderTextareaField name className alert cols rows id readonly value visualEditorEnable=true buttons="" language="" tooltip="">
  <textarea name="${name}"<#t/>
    <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
    <@renderClass className alert />
    <#if cols?has_content> cols="${cols}"</#if><#rt/>
    <#if rows?has_content> rows="${rows}"</#if><#rt/>
    <#if id?has_content> id="${id}"</#if><#rt/>
    <#if readonly?has_content> readonly="readonly"</#if><#rt/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>
    ><#t/>
    <#if value?has_content>${value}</#if><#t/>
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

<#macro renderDateTimeField name className title value size maxlength id dateType shortDateInput timeDropdownParamName defaultDateTimeString localizedIconTitle timeDropdown timeHourName classString hour1 hour2 timeMinutesName minutes isTwelveHour ampmName amSelected pmSelected compositeType formName alert=false mask="" event="" action="" step="" timeValues="" tooltip="">
  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- Note: ofbiz never handled dateType=="date" here because it pass shortDateInput=true in renderer instead-->
  <#-- These should be ~uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if (shortDateInput!false) == true>yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (((shortDateInput!false) == false) && dateType!="time")>

  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}11 ${styles.grid_cell!}">
          <#if dateType == "time">
            <input type="text" name="${name}" <@renderClass className alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}"</#if> class="${styles.grid_small!}3 ${styles.grid_cell!}"/><#rt/>
          <#else>
            <input type="text" name="${name}_i18n" <@renderClass className alert /><#rt/>
            <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
            <#if title?has_content> title="${title}"</#if>
            <#if value?has_content> value="${value}"</#if>
            <#if size?has_content> size="${size}"</#if><#rt/>
            <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
            <#if id?has_content> id="${id}_i18n"</#if> class="${styles.grid_small!}3 ${styles.grid_cell!}"/><#rt/>

            <input type="hidden" name="${name}" <#if id?has_content> id="${id}"</#if> <#if value?has_content> value="${value}"</#if> />            
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
  


  
  <#-- Old DateTimeField
  <span class="view-calendar">
    <#if dateType!="time" >
      <input type="text" name="${name}_i18n" <@renderClass className alert /><#rt/>
        <#if title?has_content> title="${title}"</#if>
        <#if value?has_content> value="${value}"</#if>
        <#if size?has_content> size="${size}"</#if><#rt/>
        <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
        <#if id?has_content> id="${id}_i18n"</#if>/><#rt/>
    </#if>
    <input type="text" name="${name}" style="height:1px;width:1px;border:none;background-color:transparent" <#if event?has_content && action?has_content> ${event}="${action}"</#if> <@renderClass className alert /><#rt/>
      <#if title?has_content> title="${title}"</#if>
      <#if value?has_content> value="${value}"</#if>
      <#if size?has_content> size="${size}"</#if><#rt/>
      <#if maxlength?has_content>  maxlength="${maxlength}"</#if>
      <#if id?has_content> id="${id}"</#if>/><#rt/>
    <#if dateType!="time" >
      <script type="text/javascript">
        if (Date.CultureInfo != undefined) {
          var initDate = <#if value?has_content>jQuery("#${id}").val()<#else>""</#if>;
          if (initDate != "") {
            var dateFormat = Date.CultureInfo.formatPatterns.shortDate<#if shortDateInput?? && !shortDateInput> + " " + Date.CultureInfo.formatPatterns.longTime</#if>;
            if (initDate.indexOf('.') != -1) {
              initDate = initDate.substring(0, initDate.indexOf('.'));
            }
            jQuery("#${id}").val(initDate);
            var ofbizTime = "<#if shortDateInput?? && shortDateInput>yyyy-MM-dd<#else>yyyy-MM-dd HH:mm:ss</#if>";
            var dateObj = Date.parseExact(initDate, ofbizTime);
            var formatedObj = dateObj.toString(dateFormat);
            jQuery("#${id}_i18n").val(formatedObj);
          }

          jQuery("#${id}").change(function() {
            var ofbizTime = "<#if shortDateInput?? && shortDateInput>yyyy-MM-dd<#else>yyyy-MM-dd HH:mm:ss</#if>";
            var newValue = ""
            if (this.value != "") {
              var dateObj = Date.parseExact(this.value, ofbizTime);
              var dateFormat = Date.CultureInfo.formatPatterns.shortDate<#if shortDateInput?? && !shortDateInput> + " " + Date.CultureInfo.formatPatterns.longTime</#if>;
              newValue = dateObj.toString(dateFormat);
            }
            jQuery("#${id}_i18n").val(newValue);
          });
          jQuery("#${id}_i18n").change(function() {
            var dateFormat = Date.CultureInfo.formatPatterns.shortDate<#if shortDateInput?? && !shortDateInput> + " " + Date.CultureInfo.formatPatterns.longTime</#if>,
            newValue = "",
            dateObj = Date.parseExact(this.value, dateFormat),
            ofbizTime;
            if (this.value != "" && dateObj !== null) {
              ofbizTime = "<#if shortDateInput?? && shortDateInput>yyyy-MM-dd<#else>yyyy-MM-dd HH:mm:ss</#if>";
              newValue = dateObj.toString(ofbizTime);
            }
            else { // invalid input
              jQuery("#${id}_i18n").val("");
            }
            jQuery("#${id}").val(newValue);
          });
        } else {
          jQuery("#${id}").change(function() {
          jQuery("#${id}_i18n").val(this.value);
        });
        jQuery("#${id}_i18n").change(function() {
          jQuery("#${id}").val(this.value);
        });
      }

      <#if shortDateInput?? && shortDateInput>
        jQuery("#${id}").datepicker({
      <#else>
        jQuery("#${id}").datetimepicker({
          showSecond: true,
          timeFormat: 'HH:mm:ss',
          stepHour: 1,
          stepMinute: 1,
          stepSecond: 1,
      </#if>
          showOn: 'button',
          buttonImage: '',
          ${styles.button_default!}: '',
          buttonImageOnly: false,
          dateFormat: 'yy-mm-dd'
        })
        <#if mask?has_content>.mask("${mask}")</#if>
        ;
      </script>
    </#if>
    <#if timeDropdown?has_content && timeDropdown=="time-dropdown">
      <select name="${timeHourName}" <#if classString?has_content>class="${classString}"</#if>><#rt/>
        <#if isTwelveHour>
          <#assign x=11>
          <#list 0..x as i>
            <option value="${i}"<#if hour1?has_content><#if i=hour1> selected="selected"</#if></#if>>${i}</option><#rt/>
          </#list>
        <#else>
          <#assign x=23>
          <#list 0..x as i>
            <option value="${i}"<#if hour2?has_content><#if i=hour2> selected="selected"</#if></#if>>${i}</option><#rt/>
          </#list>
        </#if>
        </select>:<select name="${timeMinutesName}" <#if classString?has_content>class="${classString}"</#if>><#rt/>
          <#assign values = Static["org.ofbiz.base.util.StringUtil"].toList(timeValues)>
          <#list values as i>
            <option value="${i}"<#if minutes?has_content><#if i?number== minutes ||((i?number==(60 -step?number)) && (minutes &gt; 60 - (step?number/2))) || ((minutes &gt; i?number )&& (minutes &lt; i?number+(step?number/2))) || ((minutes &lt; i?number )&& (minutes &gt; i?number-(step?number/2)))> selected="selected"</#if></#if>>${i}</option><#rt/>
          </#list>
        </select>
        <#rt/>
        <#if isTwelveHour>
          <select name="${ampmName}" <#if classString?has_content>class="${classString}"</#if>><#rt/>
            <option value="AM" <#if amSelected == "selected">selected="selected"</#if> >AM</option><#rt/>
            <option value="PM" <#if pmSelected == "selected">selected="selected"</#if>>PM</option><#rt/>
          </select>
        <#rt/>
      </#if>
    </#if>
    <input type="hidden" name="${compositeType}" value="Timestamp"/>
  </span>
  -->
</#macro>


<#macro renderDropDownField name className alert id multiple formName otherFieldName size firstInList currentValue explicitDescription allowEmpty options fieldName otherFieldName otherValue otherFieldSize dDFCurrent noCurrentSelectedKey ajaxOptions frequency minChars choices autoSelect partialSearch partialChars ignoreCase fullSearch event="" action="" ajaxEnabled=false tooltip="" manualItems=false manualItemsOnly=false>
    <select name="${name!""}<#rt/>" <@renderClass className alert /><#if id?has_content> id="${id}"</#if><#if multiple?has_content> multiple="multiple"</#if><#if otherFieldSize gt 0> onchange="process_choice(this,document.${formName}.${otherFieldName})"</#if><#if event?has_content> ${event}="${action}"</#if><#--<#if size?has_content> size="${size}"</#if>-->
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
      document.${formName}.${otherFieldName}.style.visibility  = 'hidden';
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

<#macro renderCheckBox id="" checked=false currentValue="N" name="" action="" tooltip="">
    <div class="switch small">
    <input type="checkbox" id="<#if id?has_content>${id}<#else>${name!}</#if>"<#rt/>
      <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
      <#if (checked?is_boolean && checked) || checked == "Y"> checked="checked"
      <#elseif currentValue?has_content && currentValue=="Y"> checked="checked"</#if> 
      name="${name!""?html}" value="${currentValue!}"<#if action?has_content> onClick="${action}"</#if>/><#rt/>
      <label for="<#if id?has_content>${id}<#else>${name!}</#if>"></label>
    </div>
</#macro>

<#macro renderCheckField items className alert id allChecked currentValue name event action tooltip="">
  <#list items as item>
    <div class="switch small">
    <span <@renderClass className alert />><#rt/>
      <input type="checkbox"<#if (item_index == 0)> id="${id}"</#if><#rt/>
        <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
        <#if allChecked?has_content && allChecked> checked="checked" <#elseif allChecked?has_content && !allChecked>
          <#elseif currentValue?has_content && currentValue==item.value> checked="checked"</#if> 
          name="${name?default("")?html}" value="${item.value?default("")?html}" <#if item.event?has_content> ${item.event}="${item.action!}"<#elseif event?has_content> ${event}="${action}"</#if>/><#rt/>
          <label for="${id!}"></label>
    </span>
    </div>
  </#list>
</#macro>

<#macro renderRadioField items className alert currentValue noCurrentSelectedKey name event action tooltip="">
  <#list items as item>
    <span <@renderClass className alert />><#rt/>
      <input type="radio"<#if currentValue?has_content><#if currentValue==item.key> checked="checked"</#if>
        <#if tooltip?has_content> data-tooltip aria-haspopup="true" class="has-tip tip-right" data-options="disable_for_touch:true" title="${tooltip!}"</#if><#rt/>
        <#elseif noCurrentSelectedKey?has_content && noCurrentSelectedKey == item.key> checked="checked"</#if> 
        name="${name!""?html}" value="${item.key!""?html}"<#if item.event?has_content> ${item.event}="${item.action!}"<#elseif event?has_content> ${event}="${action!}"</#if>/><#rt/>
      ${item.description}
    </span>
  </#list>
</#macro>

<#macro renderSubmitField buttonType className alert formName title name event action imgSrc confirmation containerId ajaxUrl>
  <#-- Cato: FIXME?: factor out default submit class somewhere so configurable -->
  <#if buttonType!="image">
    <#if !className?has_content || className=="smallSubmit" || className=="_BUTTON_">
      <#local className = "${styles.button!} ${styles.tiny!}">
    </#if>
  </#if>

  <#local buttonMarkup>
  <#if buttonType=="text-link">
    <a <@renderClass className alert /> href="javascript:document.${formName}.submit()" <#if confirmation?has_content>onclick="return confirm('${confirmation?js_string}');"</#if>><#if title?has_content>${title}</#if> </a>
  <#elseif buttonType=="image">
    <input type="image" src="${imgSrc}" <@renderClass className alert /><#if name?has_content> name="${name}"</#if>
    <#if title?has_content> alt="${title}"</#if><#if event?has_content> ${event}="${action}"</#if>
    <#if confirmation?has_content>onclick="return confirm('${confirmation?js_string}');"</#if>/>
  <#else>
    <input type="<#if containerId?has_content>button<#else>submit</#if>" <@renderClass className alert />
    <#if name??> name="${name}"</#if><#if title?has_content> value="${title}"</#if><#if event?has_content> ${event}="${action}"</#if>
    <#if containerId?has_content> onclick="<#if confirmation?has_content>if (confirm('${confirmation?js_string}')) </#if>ajaxSubmitFormUpdateAreas('${containerId}', '${ajaxUrl}')"
      <#else><#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}');"</#if>
    </#if>/>
  </#if>
  </#local>
  
  <#if (htmlFormRenderFormInfo.formType)! == "upload" && (htmlFormRenderFormInfo.showProgress)! == true>
      <#local baseId = htmlFormRenderFormInfo.name + "_catouplprogform">
      <@row>
        <@cell class="${styles.grid_small!}3 ${styles.grid_large!}2">
          ${buttonMarkup}
        </@cell>
        <@cell class="${styles.grid_small!}6 ${styles.grid_large!}6">
          
          <#local progressOptions = {
            "formSel" : "form[name=${htmlFormRenderFormInfo.name}]",
            "progTextBoxId" : "${baseId}_textbox",
            
            "expectedResultContainerSel" : "#main-content",
            "errorResultContainerSel" : "#main-${styles.alert_wrap!}",
            "errorResultAddWrapper" : false
          }>
          <#local action = htmlFormRenderFormInfo.progressSuccessAction!"">
          <#if action?starts_with("redirect;")>
            <#local progressOptions = progressOptions + { "successRedirectUrl" : action?substring("redirect;"?length) }>
          <#elseif action == "reload" || action?starts_with("reload:")>
            <#local progressOptions = progressOptions + { "successReloadWindow" : true }>
          </#if>
          
          <#if htmlFormRenderFormInfo.progressOptions?has_content>
            <#-- json is valid freemarker map -->
            <#local addOpts = ("{" + htmlFormRenderFormInfo.progressOptions + "}")?eval>
            <#if addOpts?has_content>
              <#local progressOptions = progressOptions + addOpts>  
            </#if>
          </#if>
          
          <@progress id="${baseId}_progbar" type="info" addWrapClass="${styles.hidden!}" progressOptions=progressOptions/>
        </@cell>
        <@cell class="${styles.grid_small!}3 ${styles.grid_large!}4" id="${baseId}_textbox">
        </@cell>
      </@row>
  <#else>
      ${buttonMarkup}
  </#if>

</#macro>

<#macro renderResetField className alert name title>
  <input type="reset" <@renderClass className alert /> name="${name}"<#if title?has_content> value="${title}"</#if>/>
</#macro>

<#macro renderHiddenField name value id event action>
  <input type="hidden" name="${name}"<#if value?has_content> value="${value}"</#if><#if id?has_content> id="${id}"</#if><#if event?has_content && action?has_content> ${event}="${action}"</#if>/>
</#macro>

<#macro renderIgnoredField></#macro>

<#macro renderFieldTitle style title id fieldHelpText="" for="">
  <#--<label <#if for?has_content>for="${for}"</#if> <#if fieldHelpText?has_content> title="${fieldHelpText}"</#if><#if style?has_content> class="${style}"</#if><#if id?has_content> id="${id}"</#if>><#t/>-->
    ${title}<#t/>
  <#--</label><#t/>-->
</#macro>

<#macro renderSingleFormFieldTitle></#macro>

<#macro renderFormOpen linkUrl formType targetWindow containerId containerStyle autocomplete name viewIndexField viewSizeField viewIndex viewSize useRowSubmit showProgress=false progressOptions="" progressSuccessAction="">
  <#global htmlFormRenderFormInfo = { "name" : name, "formType" : formType, "showProgress" : showProgress, "progressOptions" : StringUtil.wrapString(progressOptions), "progressSuccessAction" : StringUtil.wrapString(progressSuccessAction)}>
  <form method="post" action="${linkUrl}"<#if formType=="upload"> enctype="multipart/form-data"</#if><#if targetWindow?has_content> target="${targetWindow}"</#if><#if containerId?has_content> id="${containerId}"</#if> class=<#if containerStyle?has_content>"${containerStyle}"<#else>"basic-form"</#if> onsubmit="javascript:submitFormDisableSubmits(this)"<#if autocomplete?has_content> autocomplete="${autocomplete}"</#if> name="${name}"><#lt/>
    <#if useRowSubmit?has_content && useRowSubmit>
      <input type="hidden" name="_useRowSubmit" value="Y"/>
      <#if linkUrl?index_of("VIEW_INDEX") &lt;= 0 && linkUrl?index_of(viewIndexField) &lt;= 0>
        <input type="hidden" name="${viewIndexField}" value="${viewIndex}"/>
      </#if>
      <#if linkUrl?index_of("VIEW_SIZE") &lt;= 0 && linkUrl?index_of(viewSizeField) &lt;= 0>
        <input type="hidden" name="${viewSizeField}" value="${viewSize}"/>
      </#if>
    </#if>
</#macro>
<#macro renderFormClose focusFieldName formName containerId hasRequiredField>
  </form><#lt/>
  <#if focusFieldName?has_content>
    <script language="JavaScript" type="text/javascript">
      var form = document.${formName};
      form.${focusFieldName}.focus();
      <#-- enable the validation plugin for all generated forms
      only enable the validation if min one field is marked as 'required' -->
      if (jQuery(form).find(".required").size() > 0) {
        jQuery(form).validate();
      }
    </script><#lt/>
  </#if>
  <#if containerId?has_content && hasRequiredField?has_content>
    <script type="text/javascript">
      jQuery("#${containerId}").validate({
        submitHandler:
          function(form) {
            form.submit();
          }
      });
    </script>
  </#if>
</#macro>
<#macro renderMultiFormClose>
  </form><#lt/>
</#macro>

<#macro renderFormatListWrapperOpen formName style columnStyles>
  <table cellspacing="0" class="<#if style?has_content>${style}<#else>basic-table form-widget-table dark-grid</#if>"><#lt/>
</#macro>

<#macro renderFormatListWrapperClose formName>
  </table><#lt/>
</#macro>

<#macro renderFormatHeaderRowOpen style>
<thead>
  <tr class="<#if style?has_content>${style}<#else>header-row</#if>">
</#macro>
<#macro renderFormatHeaderRowClose>
  </tr>
  </thead>
</#macro>
<#macro renderFormatHeaderRowCellOpen style positionSpan>
  <th <#if positionSpan?has_content && positionSpan gt 1>colspan="${positionSpan}"</#if><#if style?has_content>class="${style}"</#if>>
</#macro>
<#macro renderFormatHeaderRowCellClose>
  </th>
</#macro>

<#macro renderFormatHeaderRowFormCellOpen style>
  <th <#if style?has_content>class="${style}"</#if>>
</#macro>
<#macro renderFormatHeaderRowFormCellClose>
  </th>
</#macro>
<#macro renderFormatHeaderRowFormCellTitleSeparator style isLast>
  <#if style?has_content><span class="${style}"></#if> - <#if style?has_content></span></#if>
</#macro>

<#macro renderFormatItemRowOpen formName itemIndex altRowStyles evenRowStyle oddRowStyle>
  <tr <#if itemIndex?has_content><#if itemIndex%2==0><#if evenRowStyle?has_content>class="${evenRowStyle}<#if altRowStyles?has_content> ${altRowStyles}</#if>"<#elseif altRowStyles?has_content>class="${altRowStyles}"</#if><#else><#if oddRowStyle?has_content>class="${oddRowStyle}<#if altRowStyles?has_content> ${altRowStyles}</#if>"<#elseif altRowStyles?has_content>class="${altRowStyles}"</#if></#if></#if> >
</#macro>
<#macro renderFormatItemRowClose formName>
  </tr>
</#macro>
<#macro renderFormatItemRowCellOpen fieldName style positionSpan>
  <td <#if positionSpan?has_content && positionSpan gt 1>colspan="${positionSpan}"</#if><#if style?has_content>class="${style}"</#if>>
</#macro>
<#macro renderFormatItemRowCellClose fieldName>
  </td>
</#macro>
<#macro renderFormatItemRowFormCellOpen style="">
  <td<#if style?has_content> class="${style}"</#if>>
</#macro>
<#macro renderFormatItemRowFormCellClose>
  </td>
</#macro>

<#macro renderFormatSingleWrapperOpen formName style="">
  <#--<table cellspacing="0" <#if style?has_content>class="${style}"</#if>>-->
</#macro>
<#macro renderFormatSingleWrapperClose formName>
  <#--</table>-->
</#macro>

<#macro renderFormatFieldRowOpen collapse=false style="" positions="">
  <#global renderFormatFieldRow_gridUsed = 0>
  <div class="${styles.grid_row!} form-field-row">
    <div class="<#if style?has_content>${style}<#else>${styles.grid_large!}12</#if> ${styles.grid_cell!}">
      <div class="${styles.grid_row!}<#if collapse> ${styles.collapse!}</#if>">
</#macro>
<#macro renderFormatFieldRowClose>
      </div>
    </div>
  </div>
</#macro>
<#macro renderFormatFieldRowTitleCellOpen style="" collapse=false positions="" position="" positionSpan="" nextPositionInRow="" lastPositionInRow="">
  <#-- calculate position grid usage size for this field entry (recalc positionSpan ourselves) -->
  <!-- positions: ${positions!} position: ${position!} positionSpan: ${positionSpan!} nextPositionInRow: ${nextPositionInRow!} lastPositionInRow: ${lastPositionInRow!} -->
  <#local gridSize = 12>
  <#local markLast = false>
  <#local fieldEntryOffset = 0>
  <#if positions?has_content && positionSpan?has_content && position?has_content>
    <#-- note: positionSpan is one less than you'd expect -->
    <#local posSpan = positionSpan + 1>
    <#local fieldEntrySize = ((posSpan*gridSize) / positions)?floor>
    <#if !nextPositionInRow?has_content && (fieldEntrySize < gridSize)>
      <#local markLast = true>
    </#if>
    <#-- calc offset if needed -->
    <#if (position > 1)>
        <#local expectedFieldEntryOffset = (((position-1)*gridSize) / positions)?floor>
        <#local fieldEntryOffset = (expectedFieldEntryOffset - renderFormatFieldRow_gridUsed)>
    </#if>
    
    <#-- WARN: if style specified manually, isn't taken into account here -->
    <#global renderFormatFieldRow_gridUsed = renderFormatFieldRow_gridUsed + fieldEntrySize + fieldEntryOffset>
  <#else>
    <#local fieldEntrySize = gridSize>
  </#if>
  
  <#-- may be more than one title+widget in one row, so wrap each combo in another div - necessary for now... -->
  <div class="<#if style?has_content>${style}<#else>${styles.grid_large!}${fieldEntrySize}<#if (fieldEntryOffset > 0)> ${styles.grid_large_offset!}${fieldEntryOffset}</#if></#if> ${styles.grid_cell!}<#if markLast> ${styles.grid_end!}</#if>">
    <div class="${styles.grid_row!} form-field-entry">
      <div class="<#if style?has_content>${style}<#else>${styles.grid_small!}3 ${styles.grid_large!}2</#if> ${styles.grid_cell!}">
        <#if collapse><span class="prefix form-field-label"><#else><label class="form-field-label"></#if>
</#macro>
<#macro renderFormatFieldRowTitleCellClose collapse=false>
        <#if collapse></span><#else></label></#if>
      </div>
</#macro>
<#macro renderFormatFieldRowSpacerCell></#macro>
<#macro renderFormatFieldRowWidgetCellOpen positionSpan="" style="" positions="" position="" nextPositionInRow="" lastPositionInRow="">
      <div class="<#if style?has_content>${style}<#else>${styles.grid_small!}8 ${styles.grid_large!}9</#if> ${styles.grid_cell!} ${styles.grid_end!}">
</#macro>
<#macro renderFormatFieldRowWidgetCellClose>
      </div>
    </div>
  </div>
</#macro>


<#macro renderFormatEmptySpace>&nbsp;</#macro>

<#macro renderTextFindField name value defaultOption opEquals opBeginsWith opContains opIsEmpty opNotEqual className alert size maxlength autocomplete titleStyle hideIgnoreCase ignCase ignoreCase>
  <@row collapse=collapse!false>
  <#if opEquals?has_content>
            <#assign class1="${styles.grid_small!}3 ${styles.grid_large!}3"/>
            <#assign class2="${styles.grid_small!}6 ${styles.grid_large!}6"/>
            <#assign class3="${styles.grid_small!}3 ${styles.grid_large!}3"/>
            
        <#else>
            <#assign class1=""/>
            <#assign class2="${styles.grid_small!}9 ${styles.grid_large!}9"/>
            <#assign class3="${styles.grid_small!}3 ${styles.grid_large!}3"/>
      </#if>      
      <#if opEquals?has_content>
        <#assign newName = "${name}"/>
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
    <input type="text" <@renderClass className alert /> name="${name}"<#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
       
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

<#macro renderDateFindField className alert name localizedInputTitle value value2 size maxlength dateType formName defaultDateTimeString imgSrc localizedIconTitle titleStyle defaultOptionFrom defaultOptionThru opEquals opSameDay opGreaterThanFromDayStart opGreaterThan opGreaterThan opLessThan opUpToDay opUpThruDay opIsEmpty>
  <#local fdatepickerOptions>{format:"yyyy-mm-dd", forceParse:false}</#local>
  <#-- note: values of localizedInputTitle are: uiLabelMap.CommonFormatDate/Time/DateTime -->
  <#local dateFormat><#if dateType == "date">yyyy-MM-dd<#elseif dateType=="time">HH:mm:ss.SSS<#else>yyyy-MM-dd HH:mm:ss.SSS</#if></#local>
  <#local useTsFormat = (dateType != "date" && dateType != "time")>
  
  <div class="${styles.grid_row!} ${styles.collapse!} date" data-date="" data-date-format="${dateFormat}">
        <div class="${styles.grid_small!}5 ${styles.grid_cell!}">
        <input class="${styles.grid_small!}3 ${styles.grid_cell!}" id="${name?html}_fld0_value" type="text" <@renderClass className alert /><#if name?has_content> name="${name?html}_fld0_value"</#if><#if localizedInputTitle?has_content> title="${localizedInputTitle}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if>/><#rt/>
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
  
  
  <#--
  <span class="view-calendar">
    <input id="${name?html}_fld0_value" type="text" <@renderClass className alert /><#if name?has_content> name="${name?html}_fld0_value"</#if><#if localizedInputTitle?has_content> title="${localizedInputTitle}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if>/><#rt/>
    <#if dateType != "time">
      <script type="text/javascript">
        <#if dateType == "date">
          jQuery("#${name?html}_fld0_value").datepicker({
        <#else>
          jQuery("#${name?html}_fld0_value").datetimepicker({
            showSecond: true,
            timeFormat: 'HH:mm:ss',
            stepHour: 1,
            stepMinute: 5,
            stepSecond: 10,
        </#if>
            showOn: 'button',
            buttonImage: '',
            ${styles.button_default!}: '',
            buttonImageOnly: false,
            dateFormat: 'yy-mm-dd'
          });
      </script>
      <#rt/>
    </#if>
    <#if titleStyle?has_content>
      <span class="${titleStyle}"><#rt/>
    </#if>
    <select<#if name?has_content> name="${name}_fld0_op"</#if> class="selectBox"><#rt/>
      <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
      <option value="sameDay"<#if defaultOptionFrom=="sameDay"> selected="selected"</#if>>${opSameDay}</option><#rt/>
      <option value="greaterThanFromDayStart"<#if defaultOptionFrom=="greaterThanFromDayStart"> selected="selected"</#if>>${opGreaterThanFromDayStart}</option><#rt/>
      <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${opGreaterThan}</option><#rt/>
    </select><#rt/>
    <#if titleStyle?has_content>
      </span><#rt/>
    </#if>
    <#rt/>
    <input id="${name?html}_fld1_value" type="text" <@renderClass className alert /><#if name?has_content> name="${name}_fld1_value"</#if><#if localizedInputTitle??> title="${localizedInputTitle?html}"</#if><#if value2?has_content> value="${value2}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if>/><#rt/>
    <#if dateType != "time">
      <script type="text/javascript">
        <#if dateType == "date">
          jQuery("#${name?html}_fld1_value").datepicker({
        <#else>
          jQuery("#${name?html}_fld1_value").datetimepicker({
            showSecond: true,
            timeFormat: 'HH:mm:ss',
            stepHour: 1,
            stepMinute: 5,
            stepSecond: 10,
        </#if>
            showOn: 'button',
            buttonImage: '',
            ${styles.button_default!}: '',
            buttonImageOnly: false,
            dateFormat: 'yy-mm-dd'
          });
      </script>
      <#rt/>
    </#if>
    <#if titleStyle?has_content>
      <span class="${titleStyle}"><#rt/>
    </#if>
    <select name=<#if name?has_content>"${name}_fld1_op"</#if> class="selectBox"><#rt/>
      <option value="opLessThan"<#if defaultOptionThru=="opLessThan"> selected="selected"</#if>>${opLessThan}</option><#rt/>
      <option value="upToDay"<#if defaultOptionThru=="upToDay"> selected="selected"</#if>>${opUpToDay}</option><#rt/>
      <option value="upThruDay"<#if defaultOptionThru=="upThruDay"> selected="selected"</#if>>${opUpThruDay}</option><#rt/>
      <option value="empty"<#if defaultOptionFrom=="empty"> selected="selected"</#if>>${opIsEmpty}</option><#rt/>
    </select><#rt/>
    <#if titleStyle?has_content>
      </span>
    </#if>
  </span>
  -->
    
</#macro>

<#macro renderRangeFindField className alert name value size maxlength autocomplete titleStyle defaultOptionFrom opEquals opGreaterThan opGreaterThanEquals opLessThan opLessThanEquals value2 defaultOptionThru>
  <input type="text" <@renderClass className alert /> <#if name?has_content>name="${name}_fld0_value"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
  <#if titleStyle?has_content>
    <span class="${titleStyle}"><#rt/>
  </#if>
  <select <#if name?has_content>name="${name}_fld0_op"</#if> class="selectBox"><#rt/>
    <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${opEquals}</option><#rt/>
    <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${opGreaterThan}</option><#rt/>
    <option value="greaterThanEqualTo"<#if defaultOptionFrom=="greaterThanEqualTo"> selected="selected"</#if>>${opGreaterThanEquals}</option><#rt/>
  </select><#rt/>
  <#if titleStyle?has_content>
    </span><#rt/>
  </#if>
  <br /><#rt/>
  <input type="text" <@renderClass className alert /><#if name?has_content> name="${name}_fld1_value"</#if><#if value2?has_content> value="${value2}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
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
</#macro>

<#--
@renderLookupField

Description: Renders a text input field as a lookup field.

Parameter: name, String, required - The name of the lookup field.
Parameter: formName, String, required - The name of the form that contains the lookup field.
Parameter: fieldFormName, String, required - Contains the lookup window form name.
Parameter: className, String, optional - The CSS class name for the lookup field.
Parameter: alert, String, optional - If "true" then the "alert" CSS class will be added to the lookup field.
Parameter: value, Object, optional - The value of the lookup field.
Parameter: size, String, optional - The size of the lookup field.
Parameter: maxlength, String or Integer, optional - The max length of the lookup field.
Parameter: id, String, optional - The ID of the lookup field.
Parameter: event, String, optional - The lookup field event that invokes "action". If the event parameter is not empty, then the action parameter must be specified as well.
Parameter: action, String, optional - The action that is invoked on "event". If action parameter is not empty, then the event parameter must be specified as well.
Parameter: readonly, boolean, optional - If true, the lookup field is made read-only.
Parameter: autocomplete, String, optional - If not empty, autocomplete is turned off for the lookup field.
Parameter: descriptionFieldName, String, optional - If not empty and the presentation parameter contains "window", specifies an alternate input field for updating.
Parameter: targetParameterIter, List, optional - Contains a list of form field names whose values will be passed to the lookup window.
Parameter: imgSrc, Not used.
Parameter: ajaxUrl, String, optional - Contains the Ajax URL, used only when the ajaxEnabled parameter contains true.
Parameter: ajaxEnabled, boolean, optional - If true, invokes the Ajax auto-completer.
Parameter: presentation, String, optional - Contains the lookup window type, either "layer" or "window".
Parameter: width, String or Integer, optional - The width of the lookup field.
Parameter: height, String or Integer, optional - The height of the lookup field.
Parameter: position, String, optional - The position style of the lookup field.
Parameter: fadeBackground, ?
Parameter: clearText, String, optional - If the readonly parameter is true, clearText contains the text to be displayed in the field, default is CommonClear label.
Parameter: showDescription, String, optional - If the showDescription parameter is true, a special span with css class "tooltip" will be created at right of the lookup button and a description will fill in (see setLookDescription in selectall.js). For now not when the lookup is read only.
Parameter: initiallyCollapsed, Not used.
Parameter: lastViewName, String, optional - If the ajaxEnabled parameter is true, the contents of lastViewName will be appended to the Ajax URL.
-->
<#macro renderLookupField name formName fieldFormName className="" alert="false" value="" size="" maxlength="" id="" event="" action="" readonly=false autocomplete="" descriptionFieldName="" targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=javaScriptEnabled presentation="layer" width="" height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed="" lastViewName="main" >
  <#if Static["org.ofbiz.widget.model.ModelWidget"].widgetBoundaryCommentsEnabled(context)>
  <!-- @renderLookupField -->
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
      <input type="text" <@renderClass className alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if>
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
        <#assign defaultMinLength = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultMinLength")>
        <#assign defaultDelay = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("widget.properties", "widget.autocompleter.defaultDelay")>
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
                  <#assign isFirst = true>
                  <#lt/>[<#rt/>
                  <#list targetParameterIter as item>
                    <#if isFirst>
                      <#lt/>document.${formName}.${item}<#rt/>
                      <#assign isFirst = false>
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

<#function escapeUrlParamDelims url>
    <#if url?contains('&amp;')>
        <#return url>
    <#else>
        <#return url?replace('&', '&amp;')>
    </#if>
</#function>

<#-- Cato: new params: forcePost, viewIndexFirst, listItemsOnly -->
<#macro renderNextPrev paginateStyle paginateFirstStyle viewIndex highIndex listSize viewSize ajaxEnabled javaScriptEnabled ajaxFirstUrl firstUrl paginateFirstLabel paginatePreviousStyle ajaxPreviousUrl previousUrl paginatePreviousLabel pageLabel ajaxSelectUrl selectUrl ajaxSelectSizeUrl selectSizeUrl commonDisplaying paginateNextStyle ajaxNextUrl nextUrl paginateNextLabel paginateLastStyle ajaxLastUrl lastUrl paginateLastLabel paginateViewSizeLabel forcePost=false viewIndexFirst=0 listItemsOnly=false>
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
  
   <#if !listItemsOnly>
   <div class="${styles.grid_row!}">
   <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
        <div class="pagination-centered ${paginateStyle}">
          <ul class="pagination">
   </#if>
            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxFirstUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${firstUrl}')"</#if><#else>href="${firstUrl}"</#if></#local>
            <li class="${paginateFirstStyle}<#if (viewIndex> viewIndexFirst)>"><a ${actionStr}>${paginateFirstLabel}</a><#else> unavailable">${paginateFirstLabel}</#if></li>
            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxPreviousUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${previousUrl}')"</#if><#else>href="${previousUrl}"</#if></#local>
            <li class="${paginatePreviousStyle}<#if (viewIndex> viewIndexFirst)>"><a ${actionStr}>${paginatePreviousLabel}</a><#else> unavailable">${paginatePreviousLabel}</#if></li>
        <#local displayDots = true/>
        <#if (listSize > 0)> 
          <#assign x=(listSize/viewSize)?ceiling>
            <#list 1..x as i>
              <#local vi = viewIndexFirst + (i - 1)>
              <#if (vi gte viewIndexFirst && vi lte viewIndexFirst+itemRange) || (vi gte viewIndex-itemRange && vi lte viewIndex+itemRange)>
                <#local displayDots = true/>
                <#if vi == viewIndex>
                  <li class="current"><a href="javascript:void(0)">${i}</a></li>
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
            <li class="${paginateNextStyle}<#if (highIndex < listSize)>"><a ${actionStr}>${paginateNextLabel}</a><#else> unavailable">${paginateNextLabel}</#if></li>
            <#local actionStr><#if javaScriptEnabled><#if ajaxEnabled>href="javascript:void(0)" onclick="ajaxUpdateAreas('${ajaxLastUrl}')"<#else>href="javascript:void(0)" onclick="<#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${lastUrl}')"</#if><#else>href="${lastUrl}"</#if></#local>
            <li class="${paginateLastStyle}<#if (highIndex < listSize)>"><a ${actionStr}>${paginateLastLabel}</a><#else> unavailable">${paginateLastLabel}</#if></li>
            <li class="nav-displaying">${commonDisplaying}</li>
            
        <#if javaScriptEnabled>
            <#local actionStr>onchange="<#if ajaxEnabled>ajaxUpdateAreas('${ajaxSelectSizeUrl}')<#else><#if forcePost>submitPaginationPost<#else>submitPagination</#if>(this, '${selectSizeUrl}')</#if>"</#local>
            <li class=""><label for="pageSize">${paginateViewSizeLabel} <select name="pageSize" size="1" ${actionStr}><#rt/>
            
          <#local sufficientPs = false>
          <#list availPageSizes as ps>
            <#if !sufficientPs>
              <option <#if viewSize == ps> selected="selected" </#if> value="${ps}">${ps}</option>
              <#if (ps >= listSize)>
                <#local sufficientPs = true>
              </#if>
            </#if>
          </#list>
              </select></label></li>
        </#if>
    <#if !listItemsOnly>          
      </ul>
    </div>
        
      </div>
    </div>
    </#if>
  </#if>
</#macro>

<#macro renderFileField className alert name value size maxlength autocomplete>
  <input type="file" <@renderClass className alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#rt/>
</#macro>
<#macro renderPasswordField className alert name value size maxlength id autocomplete>
  <input type="password" <@renderClass className alert /><#if name?has_content> name="${name}"</#if><#if value?has_content> value="${value}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${id}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/>
</#macro>
<#macro renderImageField value description alternate style event action><img<#if value?has_content> src="${value}"</#if><#if description?has_content> title="${description}"</#if> alt="<#if alternate?has_content>${alternate}"</#if><#if style?has_content> class="${style}"</#if><#if event?has_content> ${event?html}="${action}" </#if>/></#macro>

<#macro renderBanner style leftStyle rightStyle leftText text rightText>
  <table width="100%">
    <tr><#rt/>
      <#if leftText?has_content><td align="left"><#if leftStyle?has_content><div class="${leftStyle}"></#if>${leftText}<#if leftStyle?has_content></div></#if></td><#rt/></#if>
      <#if text?has_content><td align="center"><#if style?has_content><div class="${style}"></#if>${text}<#if style?has_content></div></#if></td><#rt/></#if>
      <#if rightText?has_content><td align="right"><#if rightStyle?has_content><div class="${rightStyle}"></#if>${rightText}<#if rightStyle?has_content></div></#if></td><#rt/></#if>
    </tr>
  </table>
</#macro>

<#macro renderContainerField id className><div id="${id}" class="${className}"/></#macro>

<#macro renderFieldGroupOpen style id title collapsed collapsibleAreaId expandToolTip collapseToolTip collapsible>
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
</#macro>

<#macro renderFieldGroupClose style id title>
    </fieldset>
    </div>
</div>
</#macro>

<#macro renderHyperlinkTitle name title showSelectAll="N">
  <#if title?has_content>${title}<br /></#if>
  <#if showSelectAll="Y"><input type="checkbox" name="selectAll" value="Y" onclick="javascript:toggleAll(this, '${name}');"/></#if>
</#macro>

<#macro renderSortField style title linkUrl ajaxEnabled tooltip="">
  <a<#if style?has_content> class="${style}"</#if> href="<#if ajaxEnabled?has_content && ajaxEnabled>javascript:ajaxUpdateAreas('${linkUrl}')<#else>${linkUrl}</#if>"<#if tooltip?has_content> title="${tooltip}"</#if>>${title}</a>
</#macro>

<#macro formatBoundaryComment boundaryType widgetType widgetName><!-- ${boundaryType}  ${widgetType}  ${widgetName} --></#macro>

<#macro renderTooltip tooltip tooltipStyle>
  <#if tooltip?has_content><span class="<#if tooltipStyle?has_content>${tooltipStyle}<#else>tooltip</#if>">${tooltip}</span><#rt/></#if>
</#macro>

<#macro renderClass className alert="false">
  <#if className?? || (alert?has_content && alert=="true")> class="${className!}<#if alert?? && alert?string=="true"> alert</#if>" </#if>
</#macro>

<#macro renderAsterisks requiredField requiredStyle>
  <#if requiredField=="true"><#if !requiredStyle?has_content><span class="form-field-input-asterisk">*</span></#if></#if>
</#macro>

<#macro makeHiddenFormLinkForm actionUrl name parameters targetWindow>
  <form method="post" action="${actionUrl}" <#if targetWindow?has_content>target="${targetWindow}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${name}">
    <#list parameters as parameter>
      <input name="${parameter.name}" value="${parameter.value}" type="hidden"/>
    </#list>
  </form>
</#macro>
<#macro makeHiddenFormLinkAnchor linkStyle hiddenFormName event action imgSrc description confirmation>
  <a <#if linkStyle?has_content>class="${linkStyle}"</#if> href="javascript:document.${hiddenFormName}.submit()"
    <#if action?has_content && event?has_content> ${event}="${action}"</#if>
    <#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}')"</#if>>
      <#if imgSrc?has_content><img src="${imgSrc}" alt=""/></#if>${description}</a>
</#macro>
<#macro makeHyperlinkString linkStyle hiddenFormName event action imgSrc title alternate linkUrl targetWindow description confirmation>
    <a <#if linkStyle?has_content>class="${linkStyle}"</#if> 
      href="${linkUrl}"<#if targetWindow?has_content> target="${targetWindow}"</#if>
      <#if action?has_content && event?has_content> ${event}="${action}"</#if>
      <#if confirmation?has_content> onclick="return confirm('${confirmation?js_string}')"</#if>
      <#if imgSrc?length == 0 && title?has_content> title="${title}"</#if>>
        <#if imgSrc?has_content><img src="${imgSrc}" alt="${alternate}" title="${title}"/></#if>${description}</a>
</#macro>

<#macro renderNoResultText className text wrapperOpened headerRendered numOfColumns>
  <#-- note: numOfColumns may be zero when no header -->
  <#if wrapperOpened>
    <tr>
      <td<#if (numOfColumns > 1)> colspan="${numOfColumns}"</#if>>
        <@renderLabel text=text style=className id="" />
      </td>
    </tr>
  <#else>
    <@renderLabel text=text style=className id="" />
  </#if>
</#macro>

  