<#-- Scipio Script Editor  -->
<#include "../common/common.ftl">

<#-- 
NOTES: 2016-12: 
* Neither UI nor schema support versioning for scripts, but it could be implemented in the future.
* Body code editor will only support groovy in editing mode for the foreseeable future, 
  with read-only support for simple-method and screen-actions (thanks to "xml" mode) as 
  these can currently only be run from file locations using the ofbiz/scipio script util/API.
* The language selection does very little at the moment, because we do not yet allow
  overriding the language for file locations (TODO) in order to simplify our own code.
  This should be done later because the script language detection is extremely primitive 
  (see CmsScriptTemplate.ScriptExecutor factory methods).
-->

<#assign resolvedScriptLang = resolvedScriptLang!"invalid">
<#assign resolvedScriptLangEditable = resolvedScriptLangEditable!false>
<#assign editorLangMode = editorLangMode!"clike">

<#-- check if resolved script falls within the supported lang lists, so JS doesn't have to check this -->
<#if (supportedScriptBodyLangs![])?seq_contains(resolvedScriptLang)>
    <#assign currentScriptBodyLang = resolvedScriptLang>
    <#assign currDefScriptBodyLang = resolvedScriptLang>
<#else>
    <#assign currentScriptBodyLang = "">
    <#assign currDefScriptBodyLang = defaultScriptBodyLang>
</#if>

<#if (supportedScriptLocationLangs![])?seq_contains(resolvedScriptLang)>
    <#assign currentScriptLocationLang = resolvedScriptLang>
    <#assign currDefScriptLocationLang = resolvedScriptLang>
<#else>
    <#assign currentScriptLocationLang = "">
    <#assign currDefScriptLocationLang = defaultScriptLocationLang>
</#if>

<@script>
    
    <#-- Template Source auto-select code -->
    function setTemplateSource(val) {
        jQuery('#editorForm input[name=templateSource]').val([val]);
    }
    
    function populateScriptLang(langs, currDefLang) {
        var options = "";
        for(var i=0; i < langs.length; i++) {
            var lang = langs[i];
            var selStr = '';
            if (lang.value === currDefLang) {
                selStr = ' selected="selected"';
            }
            options += '<option value="' + lang.value + '"' + selStr + '>' + lang.description + '</option>';
        }
        jQuery('#scriptLang').html(options);
        <#-- do both selected and jQuery val(), for good measure -->
        jQuery('#scriptLang').val(currDefLang);
    }
    
    <#function normScriptLangValue lang>
        <#local lang = rawString(lang)>
        <#if !lang?has_content || lang == "auto">
            <#return "">
        <#else>
            <#return lang>
        </#if>
    </#function>
    
    <#macro scriptLangScriptList langList>
        [<#list langList as lang>
            {"value": "${escapeVal(normScriptLangValue(lang), 'js')}", "description": "${escapeVal(lang, 'js')}"}<#if lang_has_next>, </#if>
        </#list>]
    </#macro>

    var cmsScriptLangInfo = {

        "resolvedScriptLang" : "${escapeVal(normScriptLangValue(resolvedScriptLang), 'js')}",
        
        <#-- NOTE: Body requires an explicit language, whereas location supports and/or forces language auto-detect -->
        "scriptBodyLangs" : <@scriptLangScriptList langList=(supportedScriptBodyLangs![]) />,
        "currDefScriptBodyLang" : "${escapeVal(normScriptLangValue(currDefScriptBodyLang), 'js')}",
        
        "scriptLocationLangs" : <@scriptLangScriptList langList=(supportedScriptLocationLangs![]) />,
        "currDefScriptLocationLang" : "${escapeVal(normScriptLangValue(currDefScriptLocationLang), 'js')}",
        
        "populateScriptLangForSource" : function(srcName) {
            if (srcName == 'Location') {
                populateScriptLang(this.scriptLocationLangs, this.currDefScriptLocationLang);
            } else {
                populateScriptLang(this.scriptBodyLangs, this.currDefScriptBodyLang);
            }
        }
    
    };

    jQuery(document).ready(function() {
        jQuery('#templateLocation').on('change keyup paste', function() {
            setTemplateSource('Location');
            cmsScriptLangInfo.populateScriptLangForSource('Location');
        });
    
        jQuery('form#editorForm input[type=radio][name=templateSource]').change(function() {
            cmsScriptLangInfo.populateScriptLangForSource(this.value);
        });
        
        var templateSourceVal = jQuery('form#editorForm input[type=radio][name=templateSource]:checked').val();
        cmsScriptLangInfo.populateScriptLangForSource(templateSourceVal);
    });
    
</@script>


<#-- EDIT TEMPLATE -->
<#if scriptTemplateModel?has_content>
    <#assign editScriptUrl = makeOfbizUrl("editScript?scriptTemplateId=${scriptTemplateModel.id!}")>

    <#-- Javascript functions -->
    <@script>
        <@commonCmsScripts />
    
        <#-- To be loaded on pageload -->
        var pEdit = $( document ).ready(function() {
            CodeMirror.fromTextArea($('textarea#templateBody')[0], {
                readOnly: ${(!resolvedScriptLangEditable)?c},
                lineNumbers: true,
                matchBrackets: true,
                mode: "${escapeVal(editorLangMode, 'js')}",
                indentUnit: 4,
                indentWithTabs: true,
                foldGutter: true,
                gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
                extraKeys: {"Ctrl-Space": "autocomplete"}
                }).on('change', function(cMirror){
                    $('textarea#templateBody')[0].value = cMirror.getValue();
                    setTemplateSource('Body');
                    cmsScriptLangInfo.populateScriptLangForSource('Body');
                }
            );
            
            setupCmsDeleteActionHooks();
        });
        
        function updateScriptInfo() {
            cmsCheckSubmitFieldOnlyIfChanged('settingsForm', 'description');
            updateCmsElement("<@ofbizUrl escapeAs='js'>updateScriptInfo</@ofbizUrl>", 'settingsForm', 
                function(eventMsgs) {
                    doCmsSuccessRedirect("${escapeFullUrl(editScriptUrl, 'js')}", eventMsgs);
                }
            );
        }
        
        function deleteScript() {
            deleteCmsElement("<@ofbizUrl escapeAs='js'>deleteScript</@ofbizUrl>", 
                { scriptTemplateId : "${(scriptTemplateModel.id)!}" }, 
                function(eventMsgs) {
                    doCmsSuccessRedirect("<@ofbizUrl escapeAs='js'>scripts</@ofbizUrl>", eventMsgs);
                }
            );
        }
    </@script>
    
    <@modal id="delete-dialog">
        <@heading>${uiLabelMap.CommonWarning}</@heading>
        ${uiLabelMap.CmsConfirmDeleteAction}
        <div class="modal-footer ${styles.text_right}">
           <a id="delete-button" class="${styles.button} btn-ok">${uiLabelMap.CommonContinue}</a>
        </div>
    </@modal>
    <#-- Content -->
    <#macro menuContent menuArgs={}>
        <@menu args=menuArgs>
            <@menuitem type="link" href=makeOfbizUrl("editScript") class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsCreateScript/>
            <@menuitem type="link" href="javascript:$('form#editorForm').submit(); void(0);" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave/>
            <@menuitem type="link" href="javascript:deleteScript(); void(0);" class="+${styles.action_run_sys!} ${styles.action_remove!} action_delete" text=uiLabelMap.CommonDelete/>
            <@cmsObjectExportFormMenuItem presetConfigName="CmsScriptTemplates" pkField="CmsScriptTemplate" pkValue=(scriptTemplateModel.id!) />
        </@menu>  
    </#macro>
    <@section menuContent=menuContent class="+cms-edit-elem cms-edit-script"><#-- FIXME: get these classes on <body> instead... -->
        
            <@row>
                <@cell columns=12>
                    <@heading level=1>${scriptTemplateModel.name!}</@heading></a>
                </@cell>
            </@row>
            <@row>
                <@cell columns=9>
                  <@form method="post" id="editorForm" action=makeOfbizUrl("createUpdateScript")>
                    <@field type="hidden" name="scriptTemplateId" id="scriptTemplateId" value=(scriptTemplateModel.id!"")/>
                
                    <#-- General Content -->
                    <@section title=uiLabelMap.CmsTemplate class="+editorContent">
                      <@fields type="default-compact">
                        <#assign templateBodyLabel = rawLabel('CmsTemplateBody')>
                        <#if !resolvedScriptLangEditable>
                            <#assign templateBodyLabel = templateBodyLabel + " (" + rawLabel('CmsReadOnly') + ")">
                        </#if>
                        <@field label=templateBodyLabel type="textarea" class="+editor" name="templateBody"  
                            id="templateBody" value=(templateBody!"") rows=30 readOnly=(!resolvedScriptLangEditable)/>
                      </@fields>
                        <#-- Codemirrors default font-size is tiny for the hints, so resetting here -->
                        <style type="text/css">
                            .CodeMirror-hints {font-size:1em;}
                        </style>
                        
                        <@field type="input" name="templateLocation" value=(templateLocation!"") id="templateLocation" 
                            label=uiLabelMap.CmsTemplateLocation placeholder="component://cms/cms-templates/scripts/xxx.ftl"/>
                        
                        <#-- NOTE: this explicit switch makes it clear to the user which is going to be submitted, so less likely to accidentally lose work 
                            It is auto-selected (see JS above) -->
                        <@field type="generic" label="${rawLabel('CmsUse')}:">
                            <#assign templateLocationSelected = templateLocation?has_content || !resolvedScriptLangEditable>
                            <#assign templateBodyTooltip = "">
                            <#if !resolvedScriptLangEditable>
                              <#assign templateBodyTooltip = rawLabel('CommonDisabled') + ": " + rawLabel('CmsSystemDoesNotSupportLanguageData')>
                            </#if>
                            <@field type="radio" name="templateSource" value="Body" label=uiLabelMap.CmsTemplateBody inline=true 
                                checked=(!templateLocationSelected) disabled=(!resolvedScriptLangEditable) tooltip=templateBodyTooltip/>
                            <@field type="radio" name="templateSource" value="Location" label=uiLabelMap.CmsTemplateLocation 
                                inline=true checked=templateLocationSelected/>
                        </@field>
                        
                        <@field type="select" name="scriptLang" id="scriptLang" label=uiLabelMap.CommonLanguageTitle>
                            <#-- dynamically populated via JS -->
                        </@field>
                    </@section>
                    
                  </@form>
                </@cell>
              
                <@cell columns=3>
                  <@form method="post" id="settingsForm" action=makeOfbizUrl('updateScriptInfo')>
                    <@field type="hidden" name="scriptTemplateId" value=scriptTemplateModel.id!""/>
                    
                    <#-- Template Information -->
                    <@section title=uiLabelMap.CommonInformation class="+editorMeta">
                        <@field type="display" value=(scriptTemplateModel.id!) label=uiLabelMap.CommonId/>                        
                        <@field type="input" name="templateName" value=(scriptTemplateModel.name!) required=true label=uiLabelMap.CommonName/>
                        <@field type="textarea" name="description_visible" value=(scriptTemplateModel.getDescription(locale)!) required=false label=uiLabelMap.CommonDescription/>

                        <#-- this is display-only for now... location always auto-determines correctly currently... -->
                        <@field type="display" label=uiLabelMap.CommonLanguageTitle><strong>${resolvedScriptLang}</strong></@field>
                        <@field type="display" value=(scriptTemplateModel.standalone?string("Y", "N")) label=uiLabelMap.CmsStandalone tooltip=uiLabelMap.CmsStandaloneDescription/>

                        <@menu type="button">
                            <@menuitem type="link" href="javascript:updateScriptInfo(); void(0);" class="+${styles.action_run_sys!} ${styles.action_update!}" text="${rawLabel('CmsSaveSettings')}" />
                        </@menu> 
                    </@section>
                  </@form>
                </@cell>  
              
            </@row>

    </@section>
<#else>
    <#-- 
    <@script>
    </@script>
    -->

    <@section>
        <#-- NEW Script -->
        <@row>
            <@cell columns=6 last=true>
                <@form method="post" id="editorForm" action=makeOfbizUrl("createUpdateScript")>
                    <@section title=uiLabelMap.CmsCreateScript>
                      <input type="hidden" name="isCreate" value="Y"/>
                      <@fields type="default-compact">
                        <@field type="input" name="templateName" value=(parameters.templateName!) id="templateName" label=uiLabelMap.CmsTemplateName placeholder=uiLabelMap.CmsMyTemplateName required=true/>
                        <@field type="textarea" name="description" value=(parameters.description!) label=uiLabelMap.CommonDescription required=false/>
                      </@fields>
                      <@fields type="default">
                        <@field type="generic" label="${rawLabel('CmsUse')}:">
                            <@field type="radio" name="templateSource" value="Body" label=uiLabelMap.CmsTemplateBody inline=true checked=true />
                            <@field type="radio" name="templateSource" value="Location" label=uiLabelMap.CmsTemplateLocation inline=true checked=false/>
                        </@field>
                      </@fields>
                      <@fields type="default-compact">
                        <@field type="select" name="scriptLang" id="scriptLang" label=uiLabelMap.CommonLanguageTitle>
                            <#-- dynamically populated via JS -->
                        </@field>
                        
                        <@field type="input" name="templateLocation" value="" id="templateLocation" label=uiLabelMap.CmsTemplateLocation placeholder="component://cms/cms-templates/scripts/xxx.ftl"/>
                        
                        <@field type="submit" text=uiLabelMap.CommonCreate class="+${styles.link_run_sys!} ${styles.action_create!}"/>
                      </@fields>
                    </@section>
                </@form>
            </@cell>
        </@row>
    </@section>
</#if>