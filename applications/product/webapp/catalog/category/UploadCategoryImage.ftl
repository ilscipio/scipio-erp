<#-- TODO: License -->

<#include "component://product/webapp/catalog/common/common.ftl">
<#if productCategoryId?has_content>
    <@section title=uiLabelMap.ProductCategoryUploadImage id="productCategoryAttachContent">
        <form method="post" enctype="multipart/form-data" action="<@pageUrl>UploadCategoryImage</@pageUrl>" name="imageUploadForm" id="imageUploadForm">
            <input type="hidden" name="productCategoryId" value="${productCategoryId}"/>
            <#assign progressOptions = {
                "formSel" : "#imageUploadForm",
                "progBarId" : "upc_progress_bar",
                "progTextBoxId" : "upcProgressMsgBox",
                
                "msgContainerParentSel" : "#productCategoryAttachContent_content",
                "msgContainerInsertMode" : "prepend",
                
                "iframeParentSel" : "#EditProductCategory",
                "expectedResultContainerSel" : "#EditProductCategory",
                "errorResultContainerSel" : "#main-${styles.alert_wrap!} .alert-box.alert",
                "errorResultAddWrapper" : true,
    
                "resultContentReplace" : true,
                "contentContainerSel" : "#ImageFields",
                "resultContentContainerSel" : "#ImageFields",
                
                "submitHook" : "validate"
            }>
            <#-- SCIPIO: the Link Images for categories are not supported any longer and are going to be dropped soon. For now we display a warning to the user. -->
            <div style="display:none;" id="deprecatedWarning">
                <@row>
                    <@cell columns=2>
                    </@cell>
                    <@cell columns=10>
                        <@alert type="warning">The selected type is deprecated. Please use at your own risk.</@alert>
                    </@cell>
                </@row>
            </div>
            <@field type="file" name="uploadedFile" label=uiLabelMap.ProductCategoryImage required=true class="+error" />            
            <#-- SCIPIO: category link images are deprecated -->
            <@field type="select" label=uiLabelMap.ProductCategoryImageType name="upload_file_type" onChange="deprecatedValidation(this);">
                <option value="category" selected=selected>${uiLabelMap.ProductCategoryImageUrl}</option>
                <option value="linkOne" >${uiLabelMap.ProductLinkOneImageUrl}</option>
                <option value="linkTwo" >${uiLabelMap.ProductLinkTwoImageUrl}</option>
            </@field>
            <@script>
                function deprecatedValidation(sel){
                    var val = $(sel).val();
                    if(val=="linkOne" || val=="linkTwo"){
                       $('#deprecatedWarning').show(); 
                    }else{
                        $('#deprecatedWarning').hide(); 
                    }
                }
            </@script>
            <@field type="submitarea" progressOptions=progressOptions>                
                <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_import!}" />
            </@field>
        </form>
    </@section>

    <@section title=uiLabelMap.ProductCategoryUploadImage>
        <@script>
            function setUploadUrl(newUrl) {
                var toExec = 'document.imageUploadForm.action="' + newUrl + '";';
                eval(toExec);
            };
        </@script>

        <form method="post" enctype="multipart/form-data" action="<@pageUrl>UploadCategoryImage?productId=${productId}&amp;upload_file_type=original</@pageUrl>" name="imageUploadForm">
            <@fields type="default-nolabelarea">
                <#-- SCIPIO: mediaProfile
            <p><em>${uiLabelMap.ProductOriginalImageMessage} : &dollar;{ofbiz.home}/applications/product/config/ImageProperties.xml</em></p>-->
                <@fields type="default">
                    <@cataloglib.imageProfileSelect fieldName="imageProfile" profileName=(parameters.mediaProfile!product.imageProfile!"") defaultProfileName="mediaProfile.IMAGE_CATEGORY"/>
                </@fields>
                <@field type="file" size="50" name="fname"/>
                <@field type="generic">
                    <@field type="radio" name="upload_file_type_bogus" value="small" onClick="setUploadUrl('${escapeVal(makePageUrl('UploadProductImage?productId=${productId}&upload_file_type=small'), 'js')}');" label=uiLabelMap.CommonSmall/>
                    <@field type="radio" name="upload_file_type_bogus" value="medium" onClick="setUploadUrl('${escapeVal(makePageUrl('UploadProductImage?productId=${productId}&upload_file_type=medium'), 'js')}');" label=uiLabelMap.CommonMedium/>
                    <@field type="radio" name="upload_file_type_bogus" value="large" onClick="setUploadUrl('${escapeVal(makePageUrl('UploadProductImage?productId=${productId}&upload_file_type=large'), 'js')}');" label=uiLabelMap.CommonLarge/>
                    <@field type="radio" name="upload_file_type_bogus" value="detail" onClick="setUploadUrl('${escapeVal(makePageUrl('UploadProductImage?productId=${productId}&upload_file_type=detail'), 'js')}');" label=uiLabelMap.CommonDetail/>
                    <@field type="radio" name="upload_file_type_bogus" value="original" checked=true onClick="setUploadUrl('${escapeVal(makePageUrl('UploadProductImage?productId=${productId}&upload_file_type=original'), 'js')}');" label=uiLabelMap.ProductOriginal/>
                </@field>
                <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_import!}" text=uiLabelMap.ProductUploadImage/>
            </@fields>
        </form>
    </@section>

</#if>