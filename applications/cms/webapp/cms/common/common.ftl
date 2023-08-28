<#-- SCIPIO: Common CMS templates utilities and definitions include
    NOTE: For reuse from other applications, please import *lib.ftl instead. -->

<#global CMS_SHORTDESC_DISPLAY_MAX_SIZE = 200>

<#macro compress_single_line><#local captured><#nested></#local>${captured?replace("^\\s+|\\s+$|\\n|\\r", "", "rm")}</#macro>

<#-- source data models are all over the place in too many formats, this helps -->
<#function toBooleanCms val>
  <#if val?is_boolean>
    <#return val>
  <#elseif val?has_content>
    <#if val == "true" || val == "Y">
      <#return true>
    <#elseif val == "false" || val == "N">
      <#return false>
    </#if>  
  </#if>
  <#-- return void -->
</#function>

<#function appendToSentence first second>
  <#if first?has_content>
    <#if second?has_content>
      <#if first?ends_with(".")>
        <#return first + " " + second>
      <#else>
        <#return first + ". " + second>
      </#if>
    <#else>
      <#return first>
    </#if>
  <#else>
    <#return second>
  </#if>
</#function>

<#function makeShortCmsDesc desc>
  <#local desc = raw(desc)>
  <#if (desc?length > CMS_SHORTDESC_DISPLAY_MAX_SIZE)>
    <#local desc = (desc[0..<CMS_SHORTDESC_DISPLAY_MAX_SIZE]) + "...">
  </#if>
  <#return rewrapString(desc)><#-- same as escapeVal -->
</#function>

<#macro webSiteSelectField name="webSiteId" label="" value="" valueUnsafe=false required=false container="" tooltip="" emptyLabel=true>
    <#-- We get cmsWebSiteList from: component://cms/script/com/ilscipio/scipio/cms/editor/CmsEditorCommon.groovy (app-wide auto-include)
    <#local websites = delegator.findByAnd("WebSite", null, UtilMisc.toList("siteName ASC"), false)>-->
    <#local websites = cmsWebSiteList![]>
    <#if !label?has_content>
      <#local label = uiLabelMap.CommonWebsite>
    </#if>
    <@field type="select" name=name label=label tooltip=tooltip required=required container=container>
      <#local siteFound = false>
      <#local siteOptions>
      <#if !required>
        <option value=""><#if !emptyLabel?is_boolean>${emptyLabel}<#else> - </#if></option>
      </#if>
        <#list websites as website>
          <#if value==website.webSiteId>
            <#local siteFound = true>
          </#if>
            <option value="${(website.webSiteId)!}"<#if value==website.webSiteId> selected="selected"</#if>>${(website.siteName)!(website.webSiteId)!}</option>
        </#list>
      </#local>
      <#if !valueUnsafe && !siteFound && value?has_content>
        <option value="${value}" selected="selected">${(delegator.findOne("WebSite", {"webSiteId":value}, true).siteName)!value}</option>
      </#if>
      ${siteOptions}
    </@field>
</#macro>

<#macro attributeFields attrTmpl={} formId="new-attribute-form">
  <#local defaultExpandLang = Static["com.ilscipio.scipio.cms.template.AttributeExpander$ExpandLang"].getDefaultExpandLang()!>
  <#if !(attributeFieldsScriptDefined!false)>
    <@script>
        function handleAttrInputTypeChange(inputTypeElem) {
            var inputType = jQuery(inputTypeElem).val();
            var form = jQuery(inputTypeElem).closest("form");
            
            <#-- determine a user-friendly default for lang & position -->
            var newExpandLang = "";
            var newExpandPos = "";
            if (inputType == "LONG_TEXT") {
                newExpandLang = "FTL";
                <#-- moot setting for FTL
                newExpandPos = "1000"; -->
            }
            var expandLangElem = jQuery('select[name=expandLang]', form);
            expandLangElem.val(newExpandLang);
            var expandPosElem = jQuery('select[name=expandPosition]', form);
            expandPosElem.val(newExpandPos);
            
            <#-- force refresh this  -->
            handleAttrExpandLangChangeImpl(form, newExpandLang);
            
            var stringTypes = <@objectAsScript lang='js' object=Static["com.ilscipio.scipio.cms.template.CmsAttributeTemplate$Type"].getStringTypesList() />
            
            <#-- disable FTL for any non-string types -->
            if (jQuery.inArray(inputType, stringTypes) != -1) {
                jQuery('select[name=expandLang] option[value=FTL]', form).prop('disabled', false);
            } else {
                jQuery('select[name=expandLang] option[value=FTL]', form).prop('disabled', true);
            }
        }
        
        function handleAttrExpandLangChange(expandLangElem) {
            var expandLang = jQuery(expandLangElem).val();
            var form = jQuery(expandLangElem).closest("form");
            handleAttrExpandLangChangeImpl(form, expandLang);
        }
        
        function handleAttrExpandLangChangeImpl(form, expandLang, noValSet) {
            <#-- disable the position for FTL, not because it doesn't work, but just because
                it's moot and the user shouldn't need to worry about it for FTL -->
            var expandPosElem = jQuery('select[name=expandPosition]', form);
            if (expandLang == "FTL") {
                if (noValSet !== true) {
                    expandPosElem.val("0");
                }
                <#-- causes update issues
                //expandPosElem.prop('disabled', true);-->
            } else {
                <#-- 
                expandPosElem.prop('disabled', false);-->
            }
        }
        
        jQuery(document).ready(function() {
            jQuery(".attr-expand-lang select").each(function() {
                handleAttrExpandLangChangeImpl(jQuery(this).closest("form"), jQuery(this).val(), true);
            });
        });
    </@script>
    <#global attributeFieldsScriptDefined = true>
  </#if>
    <#if attrTmpl.id?has_content>
      <input type="hidden" name="attributeTemplateId" value="${attrTmpl.id!""}" />
    </#if>
      <@field type="text" label=uiLabelMap.CommonImportName size="30" name="attributeName" required=true value=(attrTmpl.name!)/>
      <@field type="text" label="displayName" size="30" name="displayName" required=true value=(attrTmpl.displayName!)/>
      <@field type="select" name="inputType" label="inputType" required=true events={"change":"handleAttrInputTypeChange(this);"} containerClass="+attr-input-type">
        <#list Static["com.ilscipio.scipio.cms.template.CmsAttributeTemplate$Type"].getDisplayValues() as attrType>
          <option value="${attrType}"<#if attrType?string == (attrTmpl.type!)> selected="selected"</#if>>${attrType}</option>
        </#list>
      </@field>
      <#local javaTypeExamplesString = Static["com.ilscipio.scipio.cms.template.AttributeExpander"].getJavaTypeExampleList()?join(", ")>
      <@field type="text" label="targetType" name="targetType" size="255" required=false value=(attrTmpl.targetType!) 
        tooltip="${rawLabel('CmsAttributeTargetTypeDesc')} (${javaTypeExamplesString}, ...)"/>
      <@field type="select" name="expandLang" label="expandLang" required=false tooltip=uiLabelMap.CmsExpandLangDescription events={"change":"handleAttrExpandLangChange(this);"} containerClass="+attr-expand-lang">
        <option value=""<#if !(attrTmpl.expandLang?has_content)> selected="selected"</#if>>${uiLabelMap.CommonDefault} (${defaultExpandLang})</option>
        <#list Static["com.ilscipio.scipio.cms.template.AttributeExpander$ExpandLang"].getDisplayValues() as expandLang>
          <option value="${expandLang}"<#if expandLang?string == (attrTmpl.expandLang!)> selected="selected"</#if>>${expandLang}${expandLang.getDescriptionWithExample(locale, " - ")!}</option>
        </#list>
      </@field>
      <@field type="select" name="expandPosition" label="expandPosition" required=false tooltip=uiLabelMap.CmsExpandPositionDescription>
        <#-- DEV NOTE: the default will always be zero, but just in case, have empty value as default -->
        <option value=""<#if !(attrTmpl.expandPosition?has_content)> selected="selected"</#if>>${uiLabelMap.CommonDefault} (0 - ${uiLabelMap.CmsExpandBeforeScripts})</option>
        <option value="0"<#if "0" == (attrTmpl.expandPosition!?string)> selected="selected"</#if>>0 (${uiLabelMap.CmsExpandBeforeScripts})</option><#-- before all scripts -->
        <option value="1000"<#if "1000" == (attrTmpl.expandPosition!?string)> selected="selected"</#if>>1000 (${uiLabelMap.CmsExpandAfterScripts})</option><#-- after most scripts -->
      </@field>
      <@field type="text" label="inputPosition" name="inputPosition" required=false tooltip=uiLabelMap.CmsInputPositionDescription value=(attrTmpl.inputPosition!)/>
      <@field type="text" label="defaultValue" name="defaultValue" required=false value=(attrTmpl.defaultValue!)/>
      <@field type="select" label="inheritMode" name="inheritMode" required=false>
        <#list Static["com.ilscipio.scipio.cms.template.CmsAttributeTemplate$InheritMode"].values() as inheritMode>
            <option value="${inheritMode}"<#if (inheritMode?string == attrTmpl.inheritMode!) || (inheritMode?string == "NEVER" && !attrTmpl.inheritMode?has_content)> selected="selected"</#if>>${inheritMode.getLabel(locale)}</option>
        </#list>
      </@field>
      <@field type="text" label="inputHelp" name="inputHelp" required=false value=(attrTmpl.help!)/>
      <#-- Disabled for now - will be supported later.
      <@field type="text" label="permission" name="permission" required=false/>-->
      <@field type="text" label="maxLength" name="maxLength" size="20" required=false value=(attrTmpl.maxLength!)/>
      <@field type="text" label="regularExpression" name="regularExpression" size="255" required=false value=(attrTmpl.regularExpression!)/>
      <@field type="checkbox" label="required" name="required" size="20" required=false value="Y" altValue="N" checked=(attrTmpl.required!false)/>
</#macro>

<#macro assetAssocFields assetTmpl={}>
  <#local availAssets = delegator.findByAnd("CmsAssetTemplate", null, UtilMisc.toList("templateName ASC"), false)>
  <@field type="select" name="assetTemplateId" label=uiLabelMap.CmsAsset>
    <option value=""> - </option>
    <#list availAssets as asset>
        <option value="${(asset.assetTemplateId)!}"<#if (assetTmpl.id!) == (asset.assetTemplateId!)> selected="selected"</#if>>${(asset.templateName)!}</option>
    </#list>
  </@field>
  <@field type="text" label=uiLabelMap.CommonImportName size="30" name="importName" required=true value=(assetTmpl.assoc.importName)!/>
  <@field type="text" label=uiLabelMap.CmsDisplayName name="displayName" required=false value=(assetTmpl.assoc.displayName)!/>
  <@field type="text" label=uiLabelMap.CommonPosition size="30" name="inputPosition" required=false value=(assetTmpl.assoc.inputPosition)!/>
</#macro>

<#macro commonCmsScripts>

    function handleCmsEventResponse(data, successCallback) {
        if (data._ERROR_MESSAGE_LIST_ || data._ERROR_MESSAGE_) {
            if(data._ERROR_MESSAGE_LIST_){
                displayCmsErrorMessages(data._ERROR_MESSAGE_LIST_);
            }
            if(data._ERROR_MESSAGE_){
                displayCmsErrorMessage(data._ERROR_MESSAGE_);
            } 
        } else { <#-- no error message means success; DON'T rely on success message because not guaranteed to be one in stock ofbiz -->
            var eventMsgs = data._EVENT_MESSAGE_LIST_;
            if (eventMsgs) {
                eventMsgs = eventMsgs.slice(0); // clone
            } else {
                eventMsgs = [];
            }
            if (data._EVENT_MESSAGE_) {
                eventMsgs.push(data._EVENT_MESSAGE_);
            }
            if (successCallback) {
                successCallback(eventMsgs);
            } else {
                displayCmsEventMessages(eventMsgs);
            }
        }
    }

    function displayCmsErrorMessages(errorMsgs) {
        if (errorMsgs) {
            for(var i=0; i < errorMsgs.length ; i++) {
                displayCmsErrorMessage(errorMsgs[i]);
            }
        }
    }

    function displayCmsErrorMessage(errorMsg) {
        if (errorMsg) {
            var alertBox = '<@compress_single_line><@alert type="error">'+errorMsg+'</@alert></@compress_single_line>';
            var containerBox = $("#main-${styles.alert_wrap!}").append(alertBox);
            try {
                containerBox.foundation();
            } catch(err) {
            }
        }
    }
    
    function displayCmsEventMessages(eventMsgs) {
        if (eventMsgs) {
            for(var i=0; i < eventMsgs.length ; i++) {
                displayCmsEventMessage(eventMsgs[i]);
            }
        }
    }
    
    function displayCmsEventMessage(eventMsg) {
        if (eventMsg) {
            var alertBox = '<@compress_single_line><@alert type="success">'+eventMsg+'</@alert></@compress_single_line>';
            var containerBox = $("#main-${styles.alert_wrap!}").append(alertBox);
            try {
                containerBox.foundation();
            } catch(err) {
            }
        }
    }
    
    function doCmsSuccessRedirect(successUrl, eventMsgs) {
        <#--
        displayCmsEventMessages(eventMsgs);
        -->
        window.location.href = successUrl;
    }
    
    function getCmsFormInputsAsObject(formId) {
        var result = {};
        jQuery('#'+formId).find("input, textarea, select").each(function() {
            var inputType = this.tagName.toLowerCase() === "input" && this.type.toLowerCase();
            if (inputType !== "button" && inputType !== "submit") {
                result[this.name] = jQuery(this).val();
            }
        });
        return result;
    }
    
    function updateCmsElement(targetUrl, paramsOrFormId, successCallback) {
        var params;
        if ($.type(paramsOrFormId) === "string") {
            params = getCmsFormInputsAsObject(paramsOrFormId);
        } else {
            params = paramsOrFormId;
        }
        
        $.ajax({
            type: "POST",
            url: targetUrl,
            data: params,
            cache: false,
            async: true,
            success: function(data) { 
                handleCmsEventResponse(data, successCallback);
            }
        });
    }
    
    function deleteCmsElement(targetUrl, paramsOrFormId, successCallback) {
        updateCmsElement(targetUrl, paramsOrFormId, successCallback);
    }

    function setupCmsDeleteActionHooks() {
        <#-- Dialogs for Delete actions -->
        $("a.action_delete, li.action_delete > a").live("click", function(event){
            var t = event.target;
            event.preventDefault();
            var href = $(this).attr("href");
            var row = $(this).parent().parent();
            $("#delete-button").attr("href", href);
            try {
                let modalElem = $('#modal_delete-dialog');
                ${modalControl('modalElem','open')}
            } catch(err) {
                    t.dispatchEvent(event);

            }
        });
    }
    
    <#-- Replace or update query String parameters -->
    function updateQueryStringParameter(uri, key, value) {
      var re = new RegExp("([?&])" + key + "=.*?(&|$)", "i");
      var separator = uri.indexOf('?') !== -1 ? "&" : "?";
      if (uri.match(re)) {
        return uri.replace(re, '$1' + key + "=" + value + '$2');
      }
      else {
        return uri + separator + key + "=" + value;
      }
    }
    
    function cmsCheckSubmitFieldOnlyIfChanged(formId, fieldName, doTrim) {
        var form = jQuery('#' + formId);
        
        // remove previous hidden input just in case there was one
        jQuery('input[name=' + fieldName + ']', form).remove();
        
        var visiname = fieldName + '_visible';
        var visifield = jQuery('input[name=' + visiname + '], select[name=' + visiname + '], textarea[name=' + visiname + ']');
        if (visifield.length == 1) {
            var origValue = visifield.prop('defaultValue');
            if (doTrim !== false && origValue) origValue = origValue.trim();
            var curValue = visifield.val();
            if (doTrim !== false && curValue) curValue = curValue.trim();
            
            if (origValue === curValue) {
                //alert(fieldName + ' value same');
            } else {
                //alert(fieldName + ' value changed');
                var newfield = jQuery('<input type="hidden" name="' + fieldName + '" value=""/>');
                newfield.val(curValue);
                form.append(newfield);
            }
        } else {
            // debug
            if (visifield.length) {
                alert('ERROR: Too many fields with name "' + fieldName + '" in form "' + formId + '"');
            } else {
                alert('ERROR: Missing field "' + fieldName + '" in form "' + formId + '"');
            }
        }
    }

</#macro>

<#-- Creates a new association between a CmsScriptTemplate and a template record (type implied by form action and passed nested hidden inputs) -->
<#macro cmsScriptTemplateSelectForm formAction baseId="script-assoc-create" webSiteId="">
    <#-- 2020-03: we have to show all scripts for now, because re-adding accidentally removed scripts (that are not automatically deleted) may not work otherwise
    <#local scriptTmpls = standaloneScriptTemplates!>-->
    <#local scriptTmpls = allScriptTemplates!>

    <#-- should always show the existing scripts form, unless there are no existing standalone scripts in the system. -->
    <#local existingSelected = scriptTmpls?has_content>

    <div id="${(baseId+"-srcsel")}">
      <@form id=(baseId+"-srcsel-form")>
        <@field type="generic"><#-- self-explanatory: label="${rawLabel('CmsUse')}:"  -->
            <@field type="radio" name="scriptAssocSource" value="Existing" label=uiLabelMap.CmsExistingScriptTemplate inline=true 
                checked=existingSelected disabled=(!(scriptTmpls?has_content)) tooltip=uiLabelMap.CmsExistingScriptTemplateDescription/>
            <@field type="radio" name="scriptAssocSource" value="NewLocation" label=uiLabelMap.CmsNewScriptLocation inline=true 
                checked=!existingSelected tooltip=uiLabelMap.CmsNewScriptLocationDescription/>
        </@field>
      </@form>
      <@script>
        jQuery(document).ready(function() {
            jQuery('form#${escapeVal((baseId+"-srcsel-form"), 'js')} input[type=radio][name=scriptAssocSource]').change(function() {
                if (this.value == 'Existing') {
                    jQuery('#${escapeVal(baseId+"-newloc", 'js')}').fadeOut();
                    jQuery('#${escapeVal(baseId+"-existing", 'js')}').fadeIn();
                } else if (this.value == 'NewLocation') {
                    jQuery('#${escapeVal(baseId+"-existing", 'js')}').fadeOut();
                    jQuery('#${escapeVal(baseId+"-newloc", 'js')}').fadeIn();
                }
            });
        });
      </@script>
    </div>


    <div id="${(baseId+"-existing")}"<#if !existingSelected> style="display: none;"</#if>>
      <@form method="post" action=formAction id=(baseId+"-existing-form")>
        <@field type="select" label=uiLabelMap.CmsScriptTemplate size="30" name="scriptTemplateId" required=true>
            <option disabled="disabled">--- ${uiLabelMap.CmsStandalone}:</option>
            <#list (standaloneScriptTemplates!) as scriptTmpl>
              <@field type="option" value=scriptTmpl.id>${scriptTmpl.templateName!scriptTmpl.id} [${scriptTmpl.id}]</@field>
            </#list>
            <option disabled="disabled">--- ${uiLabelMap.CmsTemplateDependent}:</option>
            <#list (slaveScriptTemplates!) as scriptTmpl>
                <@field type="option" value=scriptTmpl.id>${scriptTmpl.templateName!scriptTmpl.id} [${scriptTmpl.id}]</@field>
            </#list>
        </@field>

        <@cmsScriptTemplateSelectFormCommonFields><#nested></@cmsScriptTemplateSelectFormCommonFields>  
      </@form>
    </div>
    
    <div id="${(baseId+"-newloc")}"<#if existingSelected> style="display: none;"</#if>>
      <@form method="post" action=formAction id=(baseId+"-newloc-form")>

        <input type="hidden" name="templateSource" value="Location"/>
        
        <#-- NON-STANDALONE means the CmsScriptTemplate will be automatically deleted once orphaned, 
            as long as the user has never edited it further (editing it switches it to "Y").
            without this auto-delete, we would probably just annoy users who make heavy use of script locations. -->
        <input type="hidden" name="standalone" value="N"/>

        <@field type="text" label=uiLabelMap.CmsScriptName size="30" name="templateName" required=true tooltip=uiLabelMap.CmsScriptNameDescription />
        <@field type="text" label=uiLabelMap.CmsScriptLocation size="30" name="templateLocation" required=true 
            placeholder="component://cms/cms-templates/actions/xxx.groovy"/>
        <#-- NOTE: 2019: this WebSite field has NO rendering impact; for organization purposes only -->
        <@webSiteSelectField name="webSiteId" value=webSiteId required=false
            tooltip="${rawLabel('CmsOnlyHookedWebSitesListed')} - ${rawLabel('CmsSettingNotUsedInRenderingNote')}"/>
        <input type="hidden" name="currentWebSiteId" value="${webSiteId}"/><#-- see CmsGetPage.groovy -->

        <@cmsScriptTemplateSelectFormCommonFields><#nested></@cmsScriptTemplateSelectFormCommonFields>
      </@form>
    </div>
</#macro>

<#macro cmsScriptTemplateSelectFormCommonFields baseId="script-assoc-create">
        <@field type="text" label=uiLabelMap.CmsInvokeName size="30" name="invokeName" required=false tooltip=uiLabelMap.CmsScriptInvokeNameDescription/>
        <@field type="text" label=uiLabelMap.CommonPosition size="30" name="inputPosition" required=false/>
        <#nested><#-- should contain at least template ID hidden input -->
        <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}" />
</#macro>

<#-- TODO?: we could support more editable fields, but to avoid issues and simplify, we are setting the script reference read-only here  -->
<#macro cmsScriptTemplateSelectFormEditFields scriptTmpl>
        <#local nameMarkup>${scriptTmpl.templateName!scriptTmpl.id} <#t/>
            (<a href="<@pageUrl>editScript?scriptTemplateId=${scriptTmpl.id}</@pageUrl>">${scriptTmpl.id}</a>)</#local>
        <@field type="display" label=uiLabelMap.CmsScriptName name="scriptName" value=wrapAsRaw({'raw':raw(scriptTmpl.templateName!scriptTmpl.id), 'htmlmarkup':nameMarkup})/>
        <@field type="display" label=uiLabelMap.CommonWebsite name="webSiteId" value=(scriptTmpl.webSiteId!"-")/>
        <@field type="display" label=uiLabelMap.CmsQualifiedName name="qualifiedName" value=(scriptTmpl.qualifiedName!"")/>
        <@field type="text" label=uiLabelMap.CmsInvokeName size="30" name="invokeName" required=false tooltip=uiLabelMap.CmsScriptInvokeNameDescription value=scriptTmpl.invokeName!/>
        <@field type="text" label=uiLabelMap.CommonPosition size="30" name="inputPosition" required=false value=scriptTmpl.inputPosition!/>
</#macro>

<#macro cmsScriptTemplateAssocTable scriptTemplates updateAction="" updateFields={} deleteAction="" includeForms=true envAssetType="">
    <#if scriptTemplates?has_content>
        <@section title=uiLabelMap.CmsScripts class="editorScripts">
          <@table type="data-complex" autoAltRows=true responsive=true scrollable=true fixedColumnsRight=1>
            <@thead>
                <@tr>
                    <@th width="32px">${uiLabelMap.CommonPosition}</@th>
                    <@th width="50px">${uiLabelMap.CommonId}</@th>
                    <@th>${uiLabelMap.CommonName}</@th>
                    <@th>${uiLabelMap.CmsQualifiedName}</@th>
                    <#-- redundant (part of qualified name): <@th>${uiLabelMap.CmsInvokeName}</@th>-->
                    <@th width="32px"></@th>
                </@tr>
            </@thead>
            <#list scriptTemplates as scriptTmpl><#-- NOTE: should be already sorted by LinkedHashMap -->
                <#local editUrlHtml = escapeVal(makePageUrl("editScript?scriptTemplateId="+raw(scriptTmpl.id)), "html")>
                <@tr>
                   <@td>${scriptTmpl.inputPosition!}</@td>
                   <@td><a href="${editUrlHtml}">${scriptTmpl.id!}</a></@td>
                   <@td><a href="${editUrlHtml}">${scriptTmpl.templateName!}</a></@td>
                   <#-- TODO: ONLY SUPPORT location for now, because body storage requires more advanced screens for reuse -->
                   <@td>${scriptTmpl.qualifiedName!""}</@td>
                   <#--<@td>${scriptTmpl.invokeName!""}</@td> -->
                   <@td>
                        <a href="javascript:$('#remove_script_${escapeVal(scriptTmpl.assocId, 'js-html')}').submit(); void(0);" class="action_delete">
                            <i class="${styles.text_color_alert} ${styles.icon!} ${styles.icon_prefix!}trash" style="font-size:16px;margin:4px;"></i>
                        </a>
                      <#if updateAction?has_content>
                        <a href="javascript:$('#modal_link_edit_script_${escapeVal(scriptTmpl.assocId, 'js-html')}').click(); void(0);">
                            <i class="${styles.text_color_alert} ${styles.icon!} ${styles.icon_prefix!}edit" style="font-size:16px;margin:4px;"></i>
                        </a>
                      </#if>
                   </@td>
                </@tr>
            </#list>
          </@table>
        </@section>
        <#if includeForms>
          <@cmsScriptTemplateAssocTableForms scriptTemplates=scriptTemplates updateAction=updateAction updateFields=updateFields deleteAction=deleteAction envAssetType=envAssetType/>
        </#if>
    </#if>
</#macro>

<#-- editPage has a single huge form so these have to be separated outside, really ugly -->
<#macro cmsScriptTemplateAssocTableForms scriptTemplates updateAction="" updateFields={} deleteAction="" envAssetType="">
    <#if scriptTemplates?has_content>
      <#if updateAction?has_content>
        <#list scriptTemplates as scriptTmpl>
          <@modal id="edit_script_${raw(scriptTmpl.assocId)}">
            <@heading>${uiLabelMap.CmsEditScript}</@heading>
            <form method="post" action="<@pageUrl>${updateAction}</@pageUrl>" id="edit-script-form-${escapeVal(scriptTmpl.assocId, 'html')}">
            <@fields type="default-compact">
              <input type="hidden" name="scriptAssocId" value="${scriptTmpl.assocId}" />
              <#if envAssetType?has_content>
                <@field type="hidden" name="envAssetType" value=envAssetType/>
              </#if>
                <#list updateFields?keys as fieldName>
              <input type="hidden" name="${escapeVal(fieldName, 'html')}" value="${escapeVal(updateFields[fieldName]!, 'html')}" />
            </#list>
              <@cmsScriptTemplateSelectFormEditFields scriptTmpl=scriptTmpl/>
              <@cmsUpdateAssocFormSubmitField/>
            </@fields>
            </form>
          </@modal>
        </#list>
      </#if>
      <#if deleteAction?has_content>
        <#list scriptTemplates as scriptTmpl>
            <form id="remove_script_${escapeVal(scriptTmpl.assocId, 'html')}" method="post" action="<@pageUrl>${deleteAction}</@pageUrl>">
                <input type="hidden" name="scriptAssocId" value="${scriptTmpl.assocId}"/>
                <input type="hidden" name="scriptTemplateId" value="${scriptTmpl.id}"/>
                <#if envAssetType?has_content>
                    <@field type="hidden" name="envAssetType" value=envAssetType/>
                </#if>
                <#list updateFields?keys as fieldName>
                <input type="hidden" name="${escapeVal(fieldName, 'html')}" value="${escapeVal(updateFields[fieldName]!, 'html')}" />
              </#list>
            </form>
        </#list>
      </#if>
    </#if>
</#macro>

<#-- DEEP content type options print that supports hierarchy -->
<#macro contentTypeOptions parentTypeId contentTypeId deep=true>
  <#list delegator.findByAnd("ContentType", {"parentTypeId":parentTypeId}, ["sequenceId"], true) as entry>
    <#if deep><@contentTypeOptions parentTypeId=entry.contentTypeId contentTypeId=contentTypeId /></#if>
    <option value="${entry.contentTypeId}"<#if contentTypeId == entry.contentTypeId> selected="selected"</#if>>${entry.get("description", locale)}</option>
  </#list>
</#macro>

<#macro cmsUpdateAssocFormSubmitField>
  <@field type="submit" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}" />
  <p><em><strong>${uiLabelMap.CommonWarning}:</strong> ${uiLabelMap.CmsUnsavedTemplateChangesMayBeLost}</em></p>
  <#-- TODO?: instead of this message, could find a way to preserve all other forms and fields...
      but it cannot be done easily via AJAX because the asset/script/attribute tables must be reordered
      for input position changes, so a page submit is needed... so the other form fields would have
      to be submitted as parameters and re-inserted... but this may in turn cause conflicts with the
      submit service... -->
</#macro>

<#macro cmsCommonExportScripts htmlwrap=true>
  <#-- TODO: formalize these and move out... -->
  <#if (getRequestVar('cmsCmnExpScriptsIncl')!false) == false>
    <@script htmlwrap=htmlwrap>
    if (typeof isJsonResultLoginNeeded === 'undefined') {
        var isJsonResultLoginNeeded = function(data) {
            <#-- FIXME: awful heuristic... ofbiz patch(es) needed -->
            if ($.type(data) === 'string') {
                if (data.indexOf("${escapeVal(uiLabelMap.CommonSessionTimeoutPleaseLogIn, 'js')}") >= 0) {
                    return true;
                }
            }
            return false;
        };
    }
    if (typeof getJsonResultErrorMsg === 'undefined') {
        var getJsonResultErrorMsg = function(data) {
            if (data._ERROR_MESSAGE_) {
                return data._ERROR_MESSAGE_;
            } else if (data._ERROR_MESSAGE_LIST_ && data._ERROR_MESSAGE_LIST_.length) {
                return data._ERROR_MESSAGE_LIST_.join(" ");
            } else if (isJsonResultLoginNeeded(data)) {
                return "${escapeVal(uiLabelMap.CommonSessionTimeoutPleaseLogIn, 'js')}";
            } else {
                return "${escapeVal(getLabel('PartyUnknown', 'PartyUiLabels'), 'js')}";
            }
        };
    }
    </@script>
    <#local dummy = setRequestVar('cmsCmnExpScriptsIncl', true)>
  </#if>
</#macro>

<#macro cmsObjectExportFormMenuItem presetConfigName="" pkField="" pkValue="" args...>
  <@menuitem type="generic">
    <@modal id="modal_export_object" label=uiLabelMap.CommonExport linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_show!}">
        <@heading>${uiLabelMap.CommonExport}</@heading>
            <div id="dataexport-errorbox" style="display:none;">
                <@alert type="error"><span id="export-object-errmsg"></span></@alert>
            </div>
        <@fields type="default-compact">
            <form method="post" action="#" id="export-object-form">
                <@field type="checkbox" label=uiLabelMap.CmsIncludeContentRecords name="includeContentRefs" size="20" required=false value="Y" checked=true/>
                <@field type="checkbox" name="exportFilesAsTextData" value="Y" label=uiLabelMap.CmsExportFilesAsTextData checked=false/>
                <@field type="checkbox" label=uiLabelMap.CmsIncludeDependencies name="includeMajorDeps" size="20" required=false value="Y" checked=false/>
              <div class="export-object-auto-inputs">
            <#if args?has_content>
              <#list args?keys as paramName>
                <input type="hidden" name="${escapeVal(paramName, 'html')}" value="${escapeVal(args[paramName]!, 'html')}"/>
              </#list>
            </#if>
                <input type="hidden" name="recordGrouping" value="MAJOR_OBJECT"/><#-- TODO?: maybe unhardcode, but probably not worth it -->
                <input type="hidden" name="presetConfigName" value="${escapeVal(presetConfigName, 'html')}"/>
                <input type="hidden" name="entityNoPkFindAll" value="false"/>
                <input type="hidden" name="singlePkfEntityName" value="${escapeVal(pkField, 'html')}"/>
                <input type="hidden" name="singlePkfIdList" value="${escapeVal(pkValue, 'html')}"/>
                <#nested>
              </div>
                
                <@field type="submit" submitType="link" href="javascript:fetchExportedObjectOutput(); void(0);" text=uiLabelMap.CommonExport class="${styles.link_run_sys!} ${styles.action_export!}" />
            </form>

            <form method="post" action="#" id="dataexport-output-form" style="display:none;">
                <@field type="textarea" id="exportResultText" label=uiLabelMap.CmsOutput name="output" value="" rows=30/>
            </form>
        </@fields>
        <@script>
            <@cmsCommonExportScripts htmlwrap=false/>
        
            function fetchExportedObjectOutput() {
                jQuery('#dataexport-output-form').fadeOut();
                jQuery('#dataexport-errorbox').fadeOut();
            
                var params = {};
                jQuery('#export-object-form .export-object-auto-inputs input').each(function() {
                    var input = jQuery(this);
                    var type = input.prop('type');
                    var name = input.prop('name');
                    if (name) {
                        if (type === 'checkbox') {
                            params[name] = input.is(":checked") ? input.val() : '';
                        } else {
                            params[name] = input.val();
                        }
                    }
                });
                params.includeMajorDeps = jQuery('#export-object-form input[name=includeMajorDeps]').is(":checked");
                params.exportFilesAsTextData = jQuery('#export-object-form input[name=exportFilesAsTextData]').is(":checked");
                params.includeContentRefs = jQuery('#export-object-form input[name=includeContentRefs]').is(":checked");

                $.ajax({
                      type: "POST",
                      url: "<@pageUrl escapeAs='js'>exportCmsDataAsXmlJson</@pageUrl>",
                      data: params,
                      cache:false,
                      async:true,
                      success: function(data) {
                          if (data.resultText) {
                              var textOut = data.resultText;
                              jQuery('#exportResultText').val(textOut);
                              jQuery('#dataexport-output-form').fadeIn();
                          } else {
                              var textOut = "${escapeVal(uiLabelMap.CommonError, 'js')}: " + getJsonResultErrorMsg(data);
                              jQuery('#export-object-errmsg').html(textOut);
                              jQuery('#dataexport-errorbox').fadeIn();
                          }
                      },
                      error: function(jqXHR, textStatus, errorThrown) {
                          var textOut = "${escapeVal(uiLabelMap.CommonError, 'js')}";
                          if (textStatus) { textOut += ": " + textStatus; }
                          if (errorThrown) { textOut += ": " + errorThrown; }
                          jQuery('#export-object-errmsg').html(textOut);
                          jQuery('#dataexport-errorbox').fadeIn();
                      }
                });
            }
        </@script>
    </@modal>
  </@menuitem>
</#macro>

<#macro cmsCopyMenuItem target title args...>
  <@menuitem type="generic">
    <@modal id="modal_copy_object" label=uiLabelMap.CommonCopy linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_copy!}">
        <@heading>${escapeVal(title, 'htmlmarkup')}</@heading>
        <@alert type="warning">${uiLabelMap.CommonFunctionalityWorkInProgressWarning} [2017-12-04]</@alert>
        <@fields type="default-compact">
            <form action="<@pageUrl uri=target/>" method="post" id="cms-copy-object-form">
                <#nested/>
                <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_copy!}" />
            </form>
        </@fields>
    </@modal>
  </@menuitem>
</#macro>

<#macro cmsCopyVersionSelect versionId args...>
    <@field type="select" name="srcVersionId" label=uiLabelMap.CommonVersion>
        <@field type="option" value="LATEST">${uiLabelMap.ContentLatest}</@field>
      <#if versionId?has_content>
        <@field type="option" value=versionId>${uiLabelMap.CommonSelected} (${versionId})</@field>
      </#if>
        <@field type="option" value="ACTIVE">${uiLabelMap.CommonActive}</@field>
    </@field>
</#macro>

<#macro customVariantSizeForm>
  <@fields type="default-manual" label=uiLabelMap.CmsMediaCustomSizeVariantsFromForm>
      <@row>
        <@cell columns=3>
          <label class="form-field-label">${uiLabelMap.ImageCustomVariantSizeName}</label>
          <input type="text" name="variantSizeName" class="field-inline required" size="20" required="required"/>
          <#-- <@field type="input" inline=true name="variantSizeName" label=uiLabelMap.ImageCustomVariantSizeName labelArea=true required=true value="" id=""/> -->
        </@cell>
        <@cell columns=1>
          <label class="form-field-label">${uiLabelMap.ImageCustomVariantSizeWidth}</label>
          <input type="text" name="variantSizeWidth" class="field-inline required" size="5" required="required"/>
          <#-- <@field type="text" inline=true name="variantSizeWidth" labelArea=true label=uiLabelMap.ImageCustomVariantSizeWidth required=true value="" id=""/> -->
        </@cell>
        <@cell columns=1>
          <label class="form-field-label">${uiLabelMap.ImageCustomVariantSizeHeight}</label>
          <input type="text" name="variantSizeHeight" class="field-inline required" size="5" required="required"/>
          <#-- <@field type="text" inline=true name="variantSizeHeight" labelArea=true label=uiLabelMap.ImageCustomVariantSizeHeight required=true value="" id=""/> -->
        </@cell>
        <@cell columns=2>
          <label class="form-field-label">${uiLabelMap.CommonFormat} (jpg, png)</label>
          <#-- FIXME: due to server-side processing this can't be empty, so "original" represents empty instead, checked server-side -->
          <input type="text" name="variantSizeFormat" class="field-inline" size="10" required="required" value="original"/>
        </@cell>
        <@cell columns=2>
          <label class="form-field-label">${uiLabelMap.ImageCustomVariantSizeUpscaleMode}</label>
          <@field type="select" name="variantSizeUpscaleMode" inline=true size="5" required=true>
            <option value="on" selected="selected">on</option>
            <option value="off">off</option>
            <option value="omit">omit</option>
          </@field>
        </@cell>
        <@cell columns=3>
          <input type="hidden" name="variantSizeSequenceNum" value="0"/>
        </@cell>
      </@row>
  </@fields>
</#macro>

<#macro commonCustomVariantSizeScript saveAsPreset=false>
    <@script>
        <#if saveAsPreset>
         var saveAsPreset = function() {
            var isChecked = jQuery(this).is(':checked');
            if (isChecked === true) {
                jQuery('#cmsmedia-customvariantsize-preset-name').show();
            } else {
                jQuery('#cmsmedia-customvariantsize-preset-name').hide();
            }
         };
         jQuery('#saveAsPreset').click(saveAsPreset);
        </#if>

        var customVariantSizeCount = 0;
        var customVariantSizeFieldCount = 0;
        var addCustomVariantSize = function() {
            customVariantSizeCount++;
            <#assign customVariantSizeForm><@customVariantSizeForm /></#assign>
            var customVariantSizeForm = $('${escapeVal(customVariantSizeForm, 'js')}');
            var variantSizeSequenceNum = customVariantSizeForm.find('input').each(function() {
                $(this, customVariantSizeForm).attr('id', 'customvariantsize-field-' + customVariantSizeFieldCount);
                if ($(this).attr('name') == 'variantSizeSequenceNum') {
                    $(this, customVariantSizeForm).val(customVariantSizeCount);
                }
                customVariantSizeFieldCount++;
                console.log('input #' + $(this).attr('id') + ' name[' + $(this).attr('name') + '] = ' + $(this).val());
            });
               
            jQuery('.cmsmedia-customvariantsize-add-cnt').before(customVariantSizeForm);
        };
        jQuery('.cmsmedia-customvariantsize-add').click(addCustomVariantSize);

        var customVariantSizeMethodElem = jQuery('#mediaForm input[name=customVariantSizeMethod]');
        jQuery(customVariantSizeMethodElem).click(function() {
            customVariantSizeMethod = $(this).val();
            if (customVariantSizeMethod == "customVariantSizesForm") {
                addCustomVariantSize();
            }
            jQuery('.cmsmedia-customvariantsize-method').hide();
            jQuery('.' + customVariantSizeMethod).show();
        });
    </@script>
</#macro>

<#macro responsiveImgForm>
     <@fields type="default-manual" label=uiLabelMap.CmsMediaResponsiveImgForm>
      <@row>
        <@cell columns=4>
          <label class="form-field-label">${uiLabelMap.ImageViewPortMediaQuery}</label>
          <input type="text" name="viewPortMediaQuery" class="field-inline required" size="20" required="required">
          <#-- <@field type="text" inline=true name="viewPortMediaQuery" labelArea=true label=uiLabelMap.ImageViewPortMediaQuery required=true value="" id=""/> -->
        </@cell>
        <@cell columns=4>
          <label class="form-field-label">${uiLabelMap.ImageViewPortLength}</label>
          <input type="text" name="viewPortLength" class="field-inline required" size="20" required="required">
          <#-- <@field type="text" inline=true name="viewPortLength" labelArea=true label=uiLabelMap.ImageViewPortLength required=true value="" id=""/> -->
        </@cell>
        <@cell columns=4>
          <input type="hidden" name="viewPortSequenceNum" required="required">
          <#-- <@field type="hidden" inline=true name="viewPortSequenceNum" labelArea=true required=true value="" id=""/> -->
        </@cell>
      </@row>
     </@fields>
</#macro>

<#macro responsiveImgScript>
    <@script>
        <#assign customResponsiveImgForm><@responsiveImgForm /></#assign>
        var responsiveImgCount = 0;
        var responsiveImgFieldCount = 0;
        var addResponsiveImgSize = function() {
            responsiveImgCount++;
            var customResponsiveImgForm = $('${escapeVal(customResponsiveImgForm, 'js')}');
            var viewPortSequenceNum = jQuery(customResponsiveImgForm).find('input').each(function() {
                $(this, customResponsiveImgForm).attr('id', 'responsiveimg-field-' + responsiveImgFieldCount);
                if ($(this).attr('name') == 'viewPortSequenceNum') {
                    $(this, customResponsiveImgForm).val(responsiveImgCount);
                }
                responsiveImgFieldCount++;
                console.log('input #' + $(this).attr('id') + ' name[' + $(this).attr('name') + '] = ' + $(this).val());
            });
            jQuery('.cmsmedia-responsiveimg-add-cnt').before(customResponsiveImgForm);
        };

        $('select[name=srcsetModeEnumId]').change(function() {
           if ($(this).val() != "IMG_SRCSET_VW") {
                jQuery('.cmsmedia-responsiveimg-mode').hide();
           } else {
                jQuery('.cmsmedia-responsiveimg-mode').show();
                addResponsiveImgSize();
           }
        });

       jQuery('.cmsmedia-responsiveimg-add').click(addResponsiveImgSize);
    </@script>
</#macro>

<#-- Imported from editPage.ftl -->
<#macro cmsContentEditorScript selector=".editor" onLoad=true>
    function cmsSetupContentEditor() {
        $('${escapeVal(selector, 'js')}').trumbowyg({
            autogrow: true,
            semantic: false,
            btnsDef: {
                // Customizables dropdowns
                image: {
                    dropdown: ['insertImage','scipio_media_image','upload','scipio_media_video','scipio_media_audio','scipio_media_file', 'base64', 'noEmbed'],
                    ico: 'insertImage'
                },
                link: {
                    dropdown: [
                        'createLink',
                        'unlink',
                        <#-- DEV NOTE: autourl tries to identify the macro in the given link and
                            open the right dialog; it falls back on createLink. but the ones below are
                            always needed also, in order to create new links.
                            also, autourl doesn't completely replace the stock 'createLink',
                            because some users may need/want to edit a cmsPageUrl or ofbizUrl using
                            the stock form instead of the helpers. -->
                        'scipio_links_autourl',
                        'scipio_links_cmspageurl',
                        'scipio_links_ofbizurl',
                        'scipio_links_ofbizcontenturl'
                    ]
                }
            },
            btns: [
                ['viewHTML'],
                ['formatting'],
                'btnGrp-semantic',
                ['superscript', 'subscript'],
                'btnGrp-justify',
                'btnGrp-lists',
                ['link'],
                ['image'],
                ['scipio_assets'],
                ['table'],
                ['horizontalRule'],
                ['removeformat'],
                ['fullscreen']
            ],
            plugins: {
                // Add imagur parameters to upload plugin
                scipio_media: {
                    serverPath: '<@pageUrl escapeAs='js'>getMediaFiles</@pageUrl>',
                    mediaUrl: '<@contentUrl escapeAs='js'>/cms/media</@contentUrl>'
                },
                scipio_links: {
                    getPagesServerPath: '<@pageUrl escapeAs='js'>getPages</@pageUrl>',
                    getWebSitesServerPath: '<@pageUrl escapeAs='js'>getCmsWebSites</@pageUrl>',
                    <#-- WARN: currentWebSiteId may become problem in future; see js source -->
                    currentWebSiteId: '${escapeVal(webSiteId!, 'js')}'
                },
                scipio_assets: {
                    getAssetTypesServerPath: '<@pageUrl escapeAs='js'>getAssetTypes</@pageUrl>',
                    getAssetsServerPath: '<@pageUrl escapeAs='js'>getAssets</@pageUrl>',
                    getAssetAttributesServerPath: '<@pageUrl escapeAs='js'>getAssetAttributes</@pageUrl>',
                    getWebSitesServerPath: '<@pageUrl escapeAs='js'>getCmsWebSites</@pageUrl>',
                    <#-- WARN: currentWebSiteId may become problem in future; see js source -->
                    currentWebSiteId: '${escapeVal(webSiteId!, 'js')}'
                }
            }
        });


        $('.trumbowyg').each(function(){
            var $trObj = $(this);
            $trObj.on('tbwinit ',function(){
                var $iframe = $('<iframe/>').css({
                        width: '100%',
                        minHeight: 300,
                        border: 0,
                        padding:0
                    }).attr('aria-label', 'visual editor').attr('role', 'document');

                $trObj.append($iframe);
                var contextIframe = $iframe.contents()[0];

                $('head', contextIframe).append($('<link>', {
                    href: '/base-theme/bower_components/trumbowyg/dist/ui/trumbowyg.min.css',
                    rel: 'stylesheet'
                }));
                var $treditor= $trObj.find('.trumbowyg-editor');
                $treditor.detach();
                $iframe.contents().find("body").append($treditor);
                $iframe.addClass('trumbowyg-editor');
            });

        });


    }
    <#if onLoad>
    $(document).ready(function() {
        cmsSetupContentEditor();
    });
    </#if>
</#macro>
