<#include "../common/common.ftl">

<#assign formAction="createMedia">
<#assign sectionTitle=uiLabelMap.CmsCreateMedia>
<#if media?has_content>
    <#assign formAction="updateMedia">
    <#assign sectionTitle=uiLabelMap.CmsEditMedia>
    <#assign previewMediaType = previewHelper.getPreviewMediaType(media)>
</#if>

<#-- DEV NOTE: 2017-08-01: the way this page is written, 
    the @script must be placed INSIDE #createWrapper/#updateWrapper form reloaded 
    js inside them with ajax won't be able to access outside scripts (here) - e.g. delete button broken without this -->
<#macro editMediaScripts>
  <#if media?has_content>
    <#-- Javascript functions -->
    <@script>    
        <@commonCmsScripts />
        
        <#-- To be loaded on pageload -->
        $( document ).ready(function() {
            setupCmsDeleteActionHooks();
        }); 
            
        function deleteMediaAsset() {
            deleteCmsElement("<@ofbizUrl escapeAs='js'>deleteMedia</@ofbizUrl>", 
                { contentId : "${escapeVal(media.contentId!, 'js')}" }, 
                function(eventMsgs) {
                    doCmsSuccessRedirect("<@ofbizUrl escapeAs='js'>media</@ofbizUrl>", eventMsgs);
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
  </#if>
</#macro>

<#macro menuContent menuArgs={}>
    <#if media?has_content>
        <@menu args=menuArgs>
            <@menuitem type="link" href=makeOfbizUrl("editMedia") class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsCreateMedia/>
            <@menuitem type="link" href="javascript:deleteMediaAsset(); void(0);" class="+${styles.action_run_sys!} ${styles.action_remove!} action_delete" text=uiLabelMap.CommonDelete/>
            
            <#-- FIXME: labels -->
            <#-- DEV NOTE: these functions _could_ need extra parameters (a modal) in the future, but just need them to work for now -->
            <@menuitem type="link" href="javascript:jQuery('#mediaresizeform').submit(); void(0);" class="+${styles.action_run_sys!} ${styles.action_create!}" text="${rawLabel('CmsMediaVariants')}: ${rawLabel(hasVariantContent?string('CommonRecreate','CommonCreate'))}"/>
            <@menuitem type="link" href="javascript:jQuery('#mediaresizeremoveform').submit(); void(0);" class="+${styles.action_run_sys!} ${styles.action_remove!}" text="${rawLabel('CmsMediaVariants')}: ${rawLabel('CommonRemove')}"/>
        </@menu>
    </#if>
</#macro>

<@section id="mediaWrapper">

    <@section id=formAction menuContent=menuContent>  
     
      <#-- SCRIPTS INSIDE FORM needed due to ajax -->
      <@editMediaScripts/>
      
        <@row>
            <@cell id="mediaUpload" columns=6>
                <@section title=sectionTitle>
                    <form method="post"<#if !media?has_content> enctype="multipart/form-data"</#if> action="<@ofbizUrl>${formAction}</@ofbizUrl>" name="mediaForm" id="mediaForm">                                                       
                        <#if parameters.contentName?has_content>
                            <#assign contentName = parameters.contentName>    
                        <#elseif media?has_content>
                            <#assign contentName = media.contentName>
                        <#else>
                            <#assign contentName = "">
                        </#if>
                        <#if media?has_content>
                            <@field type="display" name="contentId" label=uiLabelMap.CommonId><a href="<@ofbizUrl>editMedia?contentId=${media.contentId}</@ofbizUrl>">${media.contentId}</a></@field>
                        </#if>
                        <@field type="text" name="contentName" label=uiLabelMap.CommonName value=contentName required=true />
                        <#if media?has_content>
                            <#if media.dataResourceTypeId == "IMAGE_OBJECT"><#assign dataResourceTypeIdVal = uiLabelMap.CommonImage></#if>                                   
                            <#if media.dataResourceTypeId == "VIDEO_OBJECT"><#assign dataResourceTypeIdVal = uiLabelMap.ContentResourceVideo></#if>                                    
                            <#if media.dataResourceTypeId == "AUDIO_OBJECT"><#assign dataResourceTypeIdVal = uiLabelMap.ContentResourceAudio></#if>                                    
                            <#if media.dataResourceTypeId == "DOCUMENT_OBJECT"><#assign dataResourceTypeIdVal = uiLabelMap.CommonDocument></#if>          
                            <@field type="hidden" name="contentId" value=media.contentId />
                            <@field type="hidden" name="dataResourceTypeId" value=media.dataResourceTypeId />
                            <#assign mediaUrl=makeOfbizWebappUrl({"uri": "/media?contentId=" + rawString(media.contentId!"")}) />

                            <@field type="display" label=uiLabelMap.CommonPath><a href="${escapeFullUrl(mediaUrl, 'html')}">${escapeFullUrl(mediaUrl, 'htmlmarkup')}</a></@field>
                            <#-- DEV NOTE: you must submit altValue otherwise the service didn't recognize properly -->
                            <@field type="checkbox" name="isPublic" label=uiLabelMap.FormFieldTitle_isPublic value="true" altValue="false" checked=((media.isPublic!)=="Y") />
                            <@field type="display" label=uiLabelMap.CommonMediaType value=(dataResourceTypeIdVal!"") />
                            <@field type="display" label=uiLabelMap.CmsMediaOriginalName value=(media.objectInfo!"") />                            

                            <#if fileSizeAttr?has_content>
                                <#assign fileSize = Static["com.ilscipio.scipio.cms.util.fileType.FileTypeUtil"].formatFileSize(fileSizeAttr.attrValue, dispatcher, locale) />
                                <@field type="display" label=uiLabelMap.CommonSize value=fileSize />
                            </#if>
                            <@field type="display" label=uiLabelMap.FormFieldTitle_createdDate value=media.createdDate valueType="date" />
                            
                            <#if previewMediaType?has_content && (rawString(previewMediaType) != rawString(media.mimeTypeId!""))>
                                <@field type="display" label=uiLabelMap.ContentMimeType value="${rawString(media.mimeTypeId!'')} (${rawString(previewMediaType)})" />
                            <#else>
                                <@field type="display" label=uiLabelMap.ContentMimeType value=(media.mimeTypeId!"") />
                            </#if>
                            
                            <#if media.dataResourceTypeId == "IMAGE_OBJECT">
                                <@field type="generic" label=uiLabelMap.CmsMediaResizedVariants>
                                  <#if hasVariantContent>
                                    <#list variantList?sort as variant>
                                        <#assign variantMediaUrl=makeOfbizWebappUrl({"uri": "/media?contentId=" + rawString(media.contentId!"") + "&variant=" + rawString(variant)}) />
                                        <a href="${escapeFullUrl(variantMediaUrl, 'html')}">${variant}</a> <#t/>
                                    </#list>
                                    <br/><span class="media-resized-urls-label">URLs</span>:
                                    <@fields type="default" ignoreParentField=true>
                                      <#assign variantMediaUrl=makeOfbizWebappUrl({"uri": "/media?contentId=" + rawString(media.contentId!"") + "&variant=" + rawString(variantList[0])}) />
                                      <@field type="display" label="variant"><a href="${escapeFullUrl(variantMediaUrl, 'html')}">${escapeFullUrl(variantMediaUrl, 'htmlmarkup')}</a></@field>
                                      <#assign variantMediaUrl=makeOfbizWebappUrl({"uri": "/media?contentId=" + rawString(media.contentId!"") + "&autoVariant=min&width=600&height=400"}) />
                                      <@field type="display" label="autoVariant min (css contain)"><a href="${escapeFullUrl(variantMediaUrl, 'html')}">${escapeFullUrl(variantMediaUrl, 'htmlmarkup')}</a></@field>
                                      <#assign variantMediaUrl=makeOfbizWebappUrl({"uri": "/media?contentId=" + rawString(media.contentId!"") + "&autoVariant=max&width=600&height=400"}) />
                                      <@field type="display" label="autoVariant max"><a href="${escapeFullUrl(variantMediaUrl, 'html')}">${escapeFullUrl(variantMediaUrl, 'htmlmarkup')}</a></@field>
                                    </@fields>
                                  <#else>
                                    -<#t/>
                                  </#if>
                                </@field>
                            </#if>
                        <#else>
                            <#assign imageSelected = ((parameters.dataResourceTypeId!) == "IMAGE_OBJECT")>
                            <@field type="select" label=uiLabelMap.CommonMediaTypes name="dataResourceTypeId" required=true>
                                <option value="IMAGE_OBJECT"<#if (parameters.dataResourceTypeId!) == "IMAGE_OBJECT"> selected</#if>>${uiLabelMap.CommonImage}</option>
                                <option value="VIDEO_OBJECT"<#if (parameters.dataResourceTypeId!) == "VIDEO_OBJECT"> selected</#if>>${uiLabelMap.ContentResourceVideo}</option>
                                <option value="AUDIO_OBJECT"<#if (parameters.dataResourceTypeId!) == "AUDIO_OBJECT"> selected</#if>>${uiLabelMap.ContentResourceAudio}</option>
                                <option value="DOCUMENT_OBJECT"<#if (parameters.dataResourceTypeId!) == "DOCUMENT_OBJECT"> selected</#if>>${uiLabelMap.CommonDocument}</option>                               
                            </@field>
                          <div class="cmsmedia-autoresize-area" style="<#if imageSelected>display:block;<#else>display:none;</#if>">
                            <@field type="checkbox" value="true" altValue="false" label=uiLabelMap.CmsMediaCreateAutoResizedImageVariants name="autoVariants" tooltip=uiLabelMap.CmsMediaAutoResizeDesc
                                checked=(!parameters.autoVariants?? || ("true" == parameters.autoVariants))/>  
                          </div>
                          <@script>
                            jQuery(document).ready(function() {
                                var dataResourceTypeIdElem = jQuery('#mediaForm select[name=dataResourceTypeId]');
                                var changeTypeElem = function(elem) {
                                    var val = jQuery(elem).val();
                                    if (val === "IMAGE_OBJECT") {
                                        jQuery('.cmsmedia-autoresize-area').show();
                                    } else {
                                        jQuery('.cmsmedia-autoresize-area').hide();
                                    }
                                };
                                changeTypeElem(dataResourceTypeIdElem);
                                dataResourceTypeIdElem.change(function() { changeTypeElem(this); });
                            });
                          </@script>
                        </#if>
                        <#if !media?has_content>                       
                            <@field type="file" name="uploadedFile" label=uiLabelMap.CommonMedia required=true />
                            <@field type="submitarea">
                                <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_create!}" />
                            </@field>
                        <#else>
                            <@field type="submitarea">
                                <input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}" />
                            </@field>
                        </#if>
                    </form>
                    <#if ((media.dataResourceTypeId)!) == "IMAGE_OBJECT">
                        <form method="post" action="<@ofbizUrl>rebuildMediaVariants?contentId=${media.contentId}</@ofbizUrl>" id="mediaresizeform">
                            <input type="hidden" name="contentIdList" value="[${media.contentId}]" />
                            <input type="hidden" name="force" value="true" />
                        </form>
                        <form method="post" action="<@ofbizUrl>removeMediaVariants?contentId=${media.contentId}</@ofbizUrl>" id="mediaresizeremoveform">
                            <input type="hidden" name="contentIdList" value="[${media.contentId}]" />
                        </form>
                    </#if>
                </@section>    
            </@cell>
            <#if media?has_content>
                <@cell id="mediaPreview" columns=6>                    
                    <@section title=uiLabelMap.CmsMediaPreview>
                        <#assign dataFile = makeOfbizWebappUrl({"uri": "/media?contentId=" + rawString(media.contentId)})!>
                        <#if media.dataResourceTypeId == "IMAGE_OBJECT">
                            <#assign imageFile = delegator.findOne("ImageDataResource", false, {"dataResourceId" : media.dataResourceId})!>
                            <#if imageFile?has_content>
                                <@img src=dataFile/>
                            </#if>
                        <#elseif media.dataResourceTypeId == "VIDEO_OBJECT">
                            <#assign videoFile = delegator.findOne("VideoDataResource", false, {"dataResourceId" : media.dataResourceId})!>                    
                            <#if videoFile?has_content>
                                <video controls>
                                    <source src="${escapeFullUrl(dataFile, 'html')}" type="${escapeVal(previewMediaType, 'html')}">
                                    ${uiLabelMap.CmsMimeTypeIdNotSupported}
                                </video>
                            </#if>
                        <#elseif media.dataResourceTypeId == "AUDIO_OBJECT">
                            <#assign audioFile = delegator.findOne("AudioDataResource", false, {"dataResourceId" : media.dataResourceId})!>                    
                            <#if audioFile?has_content>
                                <#assign mimeTypeId = rawString(media.mimeTypeId!"")>
                                <#-- FIXME: this needs a mime-type translating util: FF doesn't recognize audio/vorbis -->
                                <#if mimeTypeId == "audio/vorbis">
                                  <#assign mimeTypeId = "audio/ogg">
                                </#if>
                                <audio controls>
                                    <source src="${escapeFullUrl(dataFile, 'html')}" type="${escapeVal(previewMediaType, 'html')}">
                                    ${uiLabelMap.CmsMimeTypeIdNotSupported}
                                </audio>
                            </#if>
                        <#elseif media.dataResourceTypeId == "DOCUMENT_OBJECT">
                            <#assign documentFile = delegator.findOne("DocumentDataResource", false, {"dataResourceId" : media.dataResourceId})!>                    
                            <#if documentFile?has_content>
                                <@tile color="5" icon="${styles.icon} ${styles.icon_prefix}page ${styles.icon_prefix}file" link=dataFile title=(media.contentName!mediaFile.dataResourceId!"") />
                            </#if>
                        <#else>
                            (${uiLabelMap.PartyUnknown} ${uiLabelMap.CommonType})                      
                        </#if>
                    </@section>
                </@cell>
            </#if>
        </@row>
    </@section>
    
    <#if !media?has_content>
      <@row>
        <@cell columns=6 last=true>    
            <#assign progressOptions = {
                "formSel" : "#mediaForm",
                "progBarId" : "media_progress_bar",
                "progTextBoxId" : "mediaProgressMsgBox",
                
                "msgContainerParentSel" : "#mediaWrapper",
                "msgContainerInsertMode" : "prepend",
                
                "iframeParentSel" : "#iframePlaceHolder",
                "expectedResultContainerSel" : "#updateMedia",
                
                "successResultContainerSel" : "#main-${styles.alert_wrap!} .alert-box.info",
                "successResultAddWrapper" : true,
                
                "errorResultContainerSel" : "#main-${styles.alert_wrap!} .alert-box.alert",
                "errorResultAddWrapper" : true,                        
                 
                "resultContentReplace" : true,
                "contentContainerSel" : "#createMedia",
                "resultContentContainerSel" : "#updateMedia",
                
                "submitHook" : "validate"
            }>
            <@field type="submitarea" progressOptions=progressOptions label=uiLabelMap.CommonAdd />
        </@cell>
      </@row>
    </#if>
</@section>
<#if !media?has_content>
  <div id="iframePlaceHolder"></div>
</#if>
