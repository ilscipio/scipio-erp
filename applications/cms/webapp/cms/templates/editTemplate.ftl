<#include "../common/common.ftl">

<#-- Scipio Template Editor  -->

<#-- Useful code for listing all available macros and functions. May be of use for future extensions of the editor
<#list .namespace?keys as var>
    <#if .namespace[var]?is_macro>${var}</#if>
</#list>-->

<#assign maxAssetTemplateDisplayLength = 200>

<#-- EDIT TEMPLATE -->
<#if pageTemplate?has_content>
    <#assign editTemplateUrl = makeOfbizUrl("editTemplate?pageTemplateId=${pageTemplate.pageTemplateId!''}")>

    <#-- Javascript functions -->
    <@script>
        <@commonCmsScripts />
    
        <#-- Activate Template version -->
        function activateVersion(versionId) {
            var pageTemplateId = $("#pageTemplateId").val();
            $.ajax({
                  type: "POST",
                  url: "<@ofbizUrl escapeAs='js'>activateTemplateVersion</@ofbizUrl>",
                  data: {pageTemplateId:pageTemplateId,versionId:versionId},
                  cache:false,
                  async:true,
                  success: function(data) { 
                      handleCmsEventResponse(data, function(eventMsgs) {
                          doCmsSuccessRedirect("${escapeFullUrl(editTemplateUrl, 'js')}", eventMsgs);
                      });
                  }
            });
        }
            
        <#-- Save current template -->
        <#-- NOTE: javascript: calls to this must end in void(0) otherwise breaks browsers (firefox) -->
        function addVersion(form, activate) {
            return $.ajax({
                  type: "POST",
                  url: "<@ofbizUrl escapeAs='js'>addTemplateVersion</@ofbizUrl>",
                  data: getFormData(),
                  cache:false,
                  async:true,
                  success: function(data) { 
                      handleCmsEventResponse(data, function(eventMsgs) {
                          var newUrl = "${escapeFullUrl(editTemplateUrl, 'js')}";
                          newUrl = updateQueryStringParameter(newUrl, "versionId", data.versionId);
                          doCmsSuccessRedirect(newUrl, eventMsgs);
                      });
                  }
            });
        }
        
        <#-- Save and activate current template -->
        function addAndActivateVersion() {
            addVersion($('form#editorForm'), true).done(function(data) {
                if(data.versionId) {
                    activateVersion(data.versionId);
                }
            });
            return false;
        }
                
        <#-- Serialize the content -->
        function getFormData() {            
            //serialize form info
            var formInfo = $("form#editorForm").serializeArray();
            return formInfo;
        }
        
        <#-- To be loaded on pageload -->
        $( document ).ready(function() {
            var pEdit= $( document ).ready(function() {
            CodeMirror.fromTextArea($('textarea#templateBody')[0], {
                lineNumbers: true,
                matchBrackets: true,
                mode: "freemarker",
                indentUnit: 4,
                indentWithTabs: true,
                foldGutter: true,
                gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
                extraKeys: {"Ctrl-Space": "autocomplete"}
                }).on('change', function(cMirror){
                    $('textarea#templateBody')[0].value = cMirror.getValue();
                    setTemplateSource('Body');
                }
            );
            
            setupCmsDeleteActionHooks();
        }); 
         
        $( "form#editorForm" ).on( "submit", function( event ) {
              event.preventDefault();
              addVersion(this,false);
            });
        });
        
        <#-- Template Source auto-select code -->
        var setTemplateSource = function(val) {
            jQuery('#editorForm input[name=templateSource]').val([val]);
        };
        jQuery(document).ready(function() {
            jQuery('#templateLocation').on('change keyup paste', function() {
                setTemplateSource('Location');
            });
        });
        
        function updateTemplateInfo() {
            cmsCheckSubmitFieldOnlyIfChanged('settingsForm', 'description');
            updateCmsElement("<@ofbizUrl escapeAs='js'>updateTemplateInfo</@ofbizUrl>", 'settingsForm', 
                function(eventMsgs) {
                    doCmsSuccessRedirect("${escapeFullUrl(editTemplateUrl, 'js')}", eventMsgs);
                }
            );
        }
        
        function deleteTemplate() {
            deleteCmsElement("<@ofbizUrl escapeAs='js'>deleteTemplate</@ofbizUrl>", 
                { pageTemplateId : "${(pageTemplate.pageTemplateId)!}" }, 
                function(eventMsgs) {
                    doCmsSuccessRedirect("<@ofbizUrl escapeAs='js'>templates</@ofbizUrl>", eventMsgs);
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
            <@menuitem type="link" href=makeOfbizUrl("editTemplate") class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsCreateTemplate/>
            <@cmsCopyMenuItem target="copyTemplate" title=uiLabelMap.CmsCopyTemplate>
                <@field type="input" name="templateName" value="" required=true label=uiLabelMap.CommonName />
                <@field type="textarea" name="description" value="" required=false label=uiLabelMap.CommonDescription />
            </@cmsCopyMenuItem>
            <@menuitem type="generic">
                <@modal id="modal_new_attr" label=uiLabelMap.CmsAddAttribute class="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                    <@heading>${uiLabelMap.CmsAddAttribute}</@heading>
                    <form method="post" action="<@ofbizUrl>addAttributeToTemplate</@ofbizUrl>" id="new-attribute-form">
                    <@fields type="default-compact">
                      <input type="hidden" name="pageTemplateId" value="${pageTemplate.pageTemplateId!""}" />
                      <@attributeFields />
                      <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}" />
                    </@fields>
                    </form>
                </@modal>
            </@menuitem>
            <@menuitem type="generic">
                <@modal id="modal_new_asset" label=uiLabelMap.CmsAddAsset class="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                    <@heading>${uiLabelMap.CmsAddAsset}</@heading>
                     <form method="post" action="<@ofbizUrl>addAssetToTemplate</@ofbizUrl>">
                        <@fields type="default-compact">
                          <input type="hidden" name="pageTemplateId" value="${pageTemplate.pageTemplateId!""}" />
                          <@assetAssocFields/>
                          <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}" />
                        </@fields>
                    </form>
                </@modal>
            </@menuitem>
            
            <@menuitem type="generic">
                <@modal id="modal_new_script" label=uiLabelMap.CmsAddScript class="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                    <@heading>${uiLabelMap.CmsAddScript}</@heading>
                    <@fields type="default-compact">
                        <@cmsScriptTemplateSelectForm formAction=makeOfbizUrl("addScriptToTemplate")>
                            <input type="hidden" name="pageTemplateId" value="${pageTemplate.pageTemplateId!""}" />
                        </@cmsScriptTemplateSelectForm>
                    </@fields>
                </@modal>
            </@menuitem>
            
            <#-- NOTE: void(0) MUST be at the end to prevent browser failure -->
            <@menuitem type="link" href="javascript:addVersion($('form#editorForm'),false); void(0);" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave/>
            <@menuitem type="link" href="javascript:activateVersion($('#versionId').val()); void(0);" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CmsPublish/>
            <@menuitem type="link" href="javascript:addAndActivateVersion(); void(0);" class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsSaveAndPublish/>

            <@menuitem type="link" href="javascript:deleteTemplate(); void(0);" class="+${styles.action_run_sys!} ${styles.action_remove!} action_delete" text=uiLabelMap.CommonDelete/>
            
            <@cmsObjectExportFormMenuItem presetConfigName="CmsPageTemplates" pkField="CmsPageTemplate" pkValue=(pageTemplate.pageTemplateId!"") />
        </@menu>  
    </#macro>
    <@section menuContent=menuContent class="+cms-edit-elem cms-edit-template"><#-- FIXME: get these classes on <body> instead... -->
            <@row>
                <@cell columns=12>
                    <@heading level=1>${pageTemplate.templateName!}</@heading></a>
                </@cell>
            </@row>
            <@row>
              
                <@cell columns=9>
                    <@form method="post" id="editorForm">
                        <@field type="hidden" name="pageTemplateId" id="pageTemplateId" value=(pageTemplate.pageTemplateId!"")/>
                        <#-- Page Template Info -->
                        <#if (pageTemplate.pageTmpVersions.active)?has_content>
                            <input type="hidden" name="activeVersionId" id="activeVersionId" value="${pageTemplate.pageTmpVersions.active.versionId}"/>
                        </#if>
                        <#if versionId?has_content>
                            <input type="hidden" name="versionId" id="versionId" value="${versionId}"/>
                        </#if> 
                        
                        <#-- General Content -->
                        <@section title=uiLabelMap.CmsTemplate class="+editorContent">
                          <@fields type="default-compact">
                            <@field label=uiLabelMap.CmsTemplateBody type="textarea" class="+editor" name="templateBody" id="templateBody" value=(pageTemplate.templateBody!"") rows=30/>
                          </@fields>
                            <#-- Codemirrors default font-size is tiny for the hints, so resetting here -->
                            <style type="text/css">
                                .CodeMirror-hints {font-size:1em;}
                            </style>
                            
                            <@field type="input" name="templateLocation" value=(pageTemplate.templateLocation!"") id="templateLocation" label=uiLabelMap.CmsTemplateLocation placeholder="component://cms/cms-templates/xxx.ftl"/>
                            
                            <#-- NOTE: this explicit switch makes it clear to the user which is going to be submitted, so less likely to accidentally lose work 
                                It is auto-selected (see JS above) -->
                            <@field type="generic" label="${rawLabel('CmsUse')}:">
                                <#assign templateLocationSelected = pageTemplate.templateLocation?has_content>
                                <@field type="radio" name="templateSource" value="Body" label=uiLabelMap.CmsTemplateBody inline=true checked=!templateLocationSelected/>
                                <@field type="radio" name="templateSource" value="Location" label=uiLabelMap.CmsTemplateLocation inline=true checked=templateLocationSelected/>
                            </@field>
                        </@section>
                    </@form>
                    
                    <#-- Attributes -->
                    <#assign attrTemplates = pageTemplate.attrTemplates![]>
                    
                    <#if attrTemplates?has_content>
                        <@section title=uiLabelMap.CmsTemplateAttributes class="+editorAttr">
                          <@table type="data-complex">
                            <@thead>
                                <@tr>
                                    <@th>inputPos</@th>
                                    <@th width="200px">attributeName</@th>
                                    <@th>displayName</@th>
                                    <@th>inputType</@th>
                                    <@th>targetType</@th>
                                    <@th>expandLang</@th>
                                    <@th>expandPos</@th>
                                    <@th>required</@th>
                                    <@th>defaultValue</@th>
                                    <@th>maxLength</@th>
                                    <@th>regularExpression</@th>
                                    <@th>inputHelp</@th>
                                    <@th width="32px"></@th>
                                </@tr>
                            </@thead>
                            <#list attrTemplates as attrTmpl>
                                <@tr>
                                   <@td>${attrTmpl.inputPosition!}</@td>
                                   <@td>${attrTmpl.name!}</@td>
                                   <@td>${attrTmpl.displayName!}</@td>
                                   <@td>${attrTmpl.type!}</@td>
                                   <@td>${attrTmpl.targetType!}</@td>
                                   <@td>${attrTmpl.expandLang!}</@td>
                                   <@td>${attrTmpl.expandPosition!}</@td>
                                   <@td>${(attrTmpl.required!false)?string("Y","N")}</@td>
                                   <@td>${attrTmpl.defaultValue!}</@td>
                                   <@td>${attrTmpl.maxLength!}</@td>
                                   <@td>${attrTmpl.regularExpression!}</@td>
                                   <@td>${attrTmpl.help!}</@td>
                                   <@td>
                                        <a href="javascript:$('#remove_attr_${escapeVal(attrTmpl.id, 'js-html')}').submit(); void(0);" class="action_delete">
                                            <i class="${styles.text_color_alert} ${styles.icon!} ${styles.icon_prefix!}trash" style="font-size:16px;margin:4px;"></i>
                                        </a>
                                        <a href="javascript:$('#modal_link_edit_attr_${attrTmpl_index}').click(); void(0);">
                                            <i class="${styles.text_color_alert} ${styles.icon!} ${styles.icon_prefix!}edit" style="font-size:16px;margin:4px;"></i>
                                        </a>
                                   </@td>
                                </@tr>
                            </#list>
                          </@table>
                        </@section>
                        <#-- FIXME: this hast to be rendered outside table and section because of an issue
                            with styles (.row .row?) -->
                        <#list attrTemplates as attrTmpl>
                          <@modal id="edit_attr_${attrTmpl_index}">
                            <@heading>${uiLabelMap.CmsEditAttribute}</@heading>
                            <form method="post" action="<@ofbizUrl>updateTemplateAttribute</@ofbizUrl>" id="edit-attribute-form-${attrTmpl_index}">
                            <@fields type="default-compact">
                              <input type="hidden" name="pageTemplateId" value="${pageTemplate.pageTemplateId!""}" />
                              <@attributeFields attrTmpl=attrTmpl/>
                              <@cmsUpdateAssocFormSubmitField/>
                            </@fields>
                            </form>
                          </@modal>
                        </#list>
                    </#if>
                    
                    <#-- Assets -->
                    <#assign assetTemplates = pageTemplate.assetTemplates![]>
                    
                    <#if assetTemplates?has_content>
                        <@section title=uiLabelMap.CmsTemplateAssets class="+editorAssets">
                          <@table type="data-complex">
                            <@thead>
                                <@tr>
                                    <#-- NOTE: we'll display the input position, but it has no actual function in rendering. -->
                                    <@th width="32px">${uiLabelMap.CommonPosition}</@th>
                                    <@th width="200px">${uiLabelMap.CommonImportName}</@th>
                                    <@th width="200px">${uiLabelMap.CmsDisplayName}</@th>
                                    <@th width="200px">${uiLabelMap.CmsAsset}</@th>
                                    <@th>${uiLabelMap.CmsTemplateBody}</@th>
                                    <@th width="200px">${uiLabelMap.CommonCode}</@th>
                                    <@th width="32px"></@th>
                                </@tr>
                            </@thead>
                              <#list assetTemplates as assetTmpl>
                                <@tr>
                                    <@td>${assetTmpl.inputPosition!0}</@td>
                                    <@td>${assetTmpl.importName!}</@td>
                                    <@td>${(assetTmpl.assoc.displayName)!assetTmpl.name!}</@td>
                                    <@td><a href="<@ofbizUrl>editAsset?assetTemplateId=${assetTmpl.id}</@ofbizUrl>">${assetTmpl.name!}</a></@td>
                                    <#assign assetTmplBody = rawString(assetTmpl.templateBody!"")>
                                    <#if (assetTmplBody?length > maxAssetTemplateDisplayLength)>
                                        <#assign assetTmplBody = assetTmplBody?substring(0, maxAssetTemplateDisplayLength)>
                                    </#if>
                                    <@td><@code type="html" class="+codeNonEdit"><#rt>${escapeVal(assetTmplBody, 'html')}</@code></@td>
                                    <@td><@code type="html" class="+codeNonEdit"><#rt>&lt;@asset name="${assetTmpl.importName!}"/&gt;</@code></@td>
                                    <@td>
                                        <a href="javascript:$('#remove_asset_${escapeVal(assetTmpl.assocId, 'js-html')}').submit(); void(0);" class="action_delete">
                                            <i class="${styles.text_color_alert} ${styles.icon!} ${styles.icon_prefix!}trash" style="font-size:16px;margin:4px;"></i>
                                        </a>
                                        <a href="javascript:$('#modal_link_edit_asset_${escapeVal(assetTmpl.assocId, 'js-html')}').click(); void(0);">
                                            <i class="${styles.text_color_alert} ${styles.icon!} ${styles.icon_prefix!}edit" style="font-size:16px;margin:4px;"></i>
                                        </a>
                                    </@td>
                                </@tr>
                              </#list>
                          </@table>
                        </@section>
                        <#-- FIXME: this hast to be rendered outside table and section because of an issue
                            with styles (.row .row?) -->
                        <#list assetTemplates as assetTmpl>
                          <@modal id="edit_asset_${rawString(assetTmpl.assocId)}">
                            <@heading>${uiLabelMap.CmsEditAsset}</@heading>
                            <form method="post" action="<@ofbizUrl>updateTemplateAsset</@ofbizUrl>" id="edit-attribute-form-${escapeVal(assetTmpl.assocId, 'html')}">
                            <@fields type="default-compact">
                              <input type="hidden" name="pageAssetTemplateAssocId" value="${(assetTmpl.assoc.id)!""}" />
                              <input type="hidden" name="pageTemplateId" value="${pageTemplate.pageTemplateId!""}" />
                              <@assetAssocFields assetTmpl=assetTmpl/>
                              <@cmsUpdateAssocFormSubmitField/>
                            </@fields>
                            </form>
                          </@modal>
                        </#list>
                    </#if>
                    
                    <#-- Scripts -->
                    <#assign scriptTemplates = pageTemplate.scriptTemplates![]>
                    <@cmsScriptTemplateAssocTable scriptTemplates=scriptTemplates
                        updateAction="updateTemplateScript" updateFields={"pageTemplateId":pageTemplate.pageTemplateId!""}
                        deleteAction="deleteScriptAndPageTemplateAssoc" />
                    
                    <#-- Versions -->
                    <#if (pageTemplate.pageTmpVersions.all)?has_content>
                        <@section title=uiLabelMap.CommonRevisions>
                            <@table type="data-complex">
                            <@thead>
                                <@tr>
                                    <@th width="32px"></@th>
                                    <@th width="200px"></@th>
                                    <@th width="200px"></@th>
                                    <@th></@th>
                                    <@th width="32px"></@th>
                                    <@th width="32px"></@th>
                                </@tr>
                            </@thead>
                            <#list pageTemplate.pageTmpVersions.all as version>
                                <#assign rowSelected = (version.versionId == (pageTemplate.versionId!""))>
                                <#-- FIXME?: remove the alt and style using selected only? -->
                                <@tr alt=rowSelected selected=rowSelected>
                                   <@td><i class="${styles.text_color_info} ${styles.icon!} ${styles.icon_user!}" style="font-size:16px;margin:4px;"/></@td>
                                   <@td>${version.createdBy!"Anonymous"}</@td>
                                   <@td><a href="<@ofbizUrl>editTemplate?versionId=${version.versionId!""}&pageTemplateId=${version.pageTemplateId}</@ofbizUrl>">${version.lastUpdatedStamp}</a></@td>
                                   <@td><#if version.versionComment?has_content>${version.versionComment!""}</#if></@td>
                                   <@td><a href="<@ofbizUrl>editTemplate?versionId=${version.versionId!""}&pageTemplateId=${version.pageTemplateId}</@ofbizUrl>"><i class="${styles.text_color_info} ${styles.icon!} ${styles.icon_edit!}" style="font-size:16px;margin:4px;"/></a></@td>
                                   <@td><#if version.isActive==true><i class="${styles.text_color_success} ${styles.icon!} ${styles.icon_check!}" style="font-size:16px;margin:4px;"/></#if></@td>      
                                </@tr>
                            </#list>
                            </@table>
                        </@section>
                    </#if>
                </@cell>
              
              
                <@cell columns=3>
                  <@form method="post" id="settingsForm" action=makeOfbizUrl('updateTemplateInfo')>
                    <@field type="hidden" name="pageTemplateId" value=(pageTemplate.pageTemplateId!"")/>
                    
                    <#-- Template Information -->
                    <@section title=uiLabelMap.CommonInformation class="+editorMeta">
                        <@field type="display" value=(pageTemplate.pageTemplateId!) label=uiLabelMap.CommonId/> 
                        <@field type="input" name="templateName" value=(pageTemplate.templateName!) required=true label=uiLabelMap.CommonName />
                        
                        <@field type="textarea" name="description_visible" value=(pageTemplate.description!) required=false label=uiLabelMap.CommonDescription />

                        <#-- NOTE: 2016: this WebSite field has NO rendering impact anymore; for organization purposes only 
                            DEV NOTE: Questionable whether this switch should really be here anymore -->
                        <@webSiteSelectField name="webSiteId" value=(pageTemplate.webSiteId!) required=false
                            tooltip="${rawLabel('CmsOnlyHookedWebSitesListed')} - ${rawLabel('CmsSettingNotUsedInRenderingNote')}"/>
                        
                        <#assign pageRevisions><@compress_single_line>
                            <#if pageTemplate.pageTmpVersions?has_content && pageTemplate.pageTmpVersions.all?has_content>
                                ${pageTemplate.pageTmpVersions.all?size}
                            <#else>
                                0
                            </#if></@compress_single_line>
                        </#assign>
                        <@field label=uiLabelMap.CmsRevisions type="display" value=pageRevisions/>
                        
                        <@menu type="button">
                            <@menuitem type="link" href="javascript:updateTemplateInfo(); void(0);" class="+${styles.action_run_sys!} ${styles.action_update!}" text="${rawLabel('CmsSaveSettings')}" />
                        </@menu> 
                    </@section>
                  </@form>
                </@cell>               
              
            </@row>
        
        
        <#-- Workaround for OFBIZ-2330 -->
        <#list attrTemplates as attrTmpl>
            <#-- DEV NOTE: in the form IDs, I (seemingly redundantly) call escapeVal with html 
                even if it's the only html attrib like that and it seems inconsistent; 
                it's because we need the output result to be the _exact_
                same as the one further above that was escaped as 'js-html' (so we must do the 'html' part
                with the exact same encoder - in other words the Freemarker one, and not the
                aggressive ofbiz screen auto encoder) - to be sure the JS call doesn't fail to find this form -->
            <form id="remove_attr_${escapeVal(attrTmpl.id, 'html')}" method="post" action="<@ofbizUrl>deleteAttributeFromTemplate</@ofbizUrl>">
                <input type="hidden" name="attributeTemplateId" value="${attrTmpl.id!}"/>
                <input type="hidden" name="pageTemplateId" value="${pageTemplate.pageTemplateId!""}"/>
            </form>
        </#list>
        <#list assetTemplates as assetTmpl>
            <form id="remove_asset_${escapeVal(assetTmpl.assocId, 'html')}" method="post" action="<@ofbizUrl>deleteAssetFromTemplate</@ofbizUrl>">
                <input type="hidden" name="pageAssetTemplateAssocId" value="${assetTmpl.assocId}"/>
                <input type="hidden" name="pageTemplateId" value="${pageTemplate.pageTemplateId!""}"/>
            </form>
        </#list>
    </@section>
<#else>
    <@section>
        <#-- NEW Template -->
        <#--<@alert type="info"><strong>General Info:</strong> Templates are the foundation for every webpage. For a tutorial on template-editing, head over to the <a href="http://www.scipioerp.com/community/developer/views-requests/Templates">template documentation</a>.</@alert>-->
        <@row>
            <@cell columns=6 last=true>
                <@form method="post" id="editorForm" action=makeOfbizUrl("createTemplate")>
                    <@section title=uiLabelMap.CmsCreateTemplate>
                      <@fields type="default-compact">
                        <input type="hidden" name="isCreate" value="Y"/>
                        <@field type="input" name="templateName" value=(parameters.templateName!) id="templateName" label=uiLabelMap.CmsTemplateName placeholder="My Template Name" required=true/>
                        <#-- NOTE: 2016: this WebSite field has NO rendering impact anymore; for organization purposes only 
                            DEV NOTE: Questionable whether this switch should really be here anymore -->
                        <@webSiteSelectField name="webSiteId" value=(parameters.webSiteId!) required=false
                            tooltip="${rawLabel('CmsOnlyHookedWebSitesListed')} - ${rawLabel('CmsSettingNotUsedInRenderingNote')}"/>
                        <@field type="textarea" name="description" value=(parameters.description!) label=uiLabelMap.CommonDescription required=false/>
                        <@field type="submit" text=uiLabelMap.CommonCreate class="+${styles.link_run_sys!} ${styles.action_create!}"/>
                      </@fields>
                    </@section>
                </@form>
            </@cell>
        </@row>
    </@section>
</#if>