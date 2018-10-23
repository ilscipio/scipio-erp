<#-- TODO: License -->

<#if productCategoryId?has_content>
    <@section title=uiLabelMap.ProductCategoryUploadImage id="productCategoryAttachContent">
        <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>UploadCategoryImage</@ofbizUrl>" name="imageUploadForm" id="imageUploadForm">
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
</#if>