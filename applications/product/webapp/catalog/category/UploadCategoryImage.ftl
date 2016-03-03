<#-- TODO: License -->

<#if productCategoryId?has_content>
    <@script>
        function setUploadUrl(newUrl) {
        var toExec = 'document.imageUploadForm.action="' + newUrl + '";';
        eval(toExec);
        };
    </@script>
    <@section title="${uiLabelMap.ProductCategoryUploadImage}">
        <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>UploadCategoryImage?productCategoryId=${productCategoryId!}&amp;upload_file_type=category</@ofbizUrl>" name="imageUploadForm">
          <@fields type="default-nolabels">
            <@field type="file" name="fname" size="50" />
            <@field type="generic">
                <@field type="radio" name="upload_file_type_bogus" value="category" checked=true onClick="setUploadUrl('${makeOfbizUrl('UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=category')}');"/>${uiLabelMap.ProductCategoryImageUrl}
                <@field type="radio" name="upload_file_type_bogus" value="linkOne" onClick="setUploadUrl('${makeOfbizUrl('UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=linkOne')}');"/>${uiLabelMap.ProductLinkOneImageUrl}
                <@field type="radio" name="upload_file_type_bogus" value="linkTwo" onClick="setUploadUrl('${makeOfbizUrl('UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=linkTwo')}');"/>${uiLabelMap.ProductLinkTwoImageUrl}
            </@field>
            <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_import!}" text="${uiLabelMap.ProductUploadImage}"/>
          </@fields>
        </form>
    </@section>
</#if>