<#-- TODO: License -->

<#if productCategoryId?has_content>
    <@section title=uiLabelMap.ProductCategoryUploadImage id="productCategoryAttachContent">
        <@section id="productCategoryUploadImage">
            <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>UploadCategoryImage</@ofbizUrl>" name="imageUploadForm" id="imageUploadForm">
                <input type="hidden" name="productCategoryId" value="${productCategoryId}"/>
                <#assign progressOptions = {
                    "formSel" : "#imageUploadForm",
                    "progBarId" : "upc_progress_bar",
                    "progTextBoxId" : "upcProgressMsgBox",
                    
                    "msgContainerParentSel" : "#productCategoryAttachContent_content",
                    "msgContainerInsertMode" : "prepend",
                    
                    
                    "expectedResultContainerSel" : "#productCategoryUploadImage",
                    "errorResultContainerSel" : "#main-${styles.alert_wrap!}",
                    "errorResultAddWrapper" : false,
        
                    "resultContentReplace" : true,
                    "contentContainerSel" : "#productCategoryUploadImage",
                    "resultContentContainerSel" : "#productCategoryUploadImage",
                    
                    "submitHook" : "validate"
                }>
                
                <@field type="file" name="uploadedFile" label="${uiLabelMap.ProductCategoryImage}" required=true class="+error" />
                <@field type="generic" label=uiLabelMap.ProductCategoryImageType>
                    <@field type="radio" name="upload_file_type" label=uiLabelMap.ProductCategoryImageUrl value="category" checked=true />
                    <@field type="radio" name="upload_file_type" label=uiLabelMap.ProductLinkOneImageUrl value="linkOne" />
                    <@field type="radio" name="upload_file_type" label=uiLabelMap.ProductLinkTwoImageUrl value="linkTwo" />
                </@field>
                <@field type="submitarea" progressOptions=progressOptions>                
                    <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_import!}" />
                </@field>
             
            </form>
        </@section>
    </@section>
</#if>