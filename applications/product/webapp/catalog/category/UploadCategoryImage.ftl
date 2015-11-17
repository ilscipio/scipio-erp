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
                <input type="radio" name="upload_file_type_bogus" value="category" checked="checked" onclick='setUploadUrl("<@ofbizUrl>UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=category</@ofbizUrl>");'/>${uiLabelMap.ProductCategoryImageUrl}
                <input type="radio" name="upload_file_type_bogus" value="linkOne" onclick='setUploadUrl("<@ofbizUrl>UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=linkOne</@ofbizUrl>");'/>${uiLabelMap.ProductLinkOneImageUrl}
                <input type="radio" name="upload_file_type_bogus" value="linkTwo"onclick='setUploadUrl("<@ofbizUrl>UploadCategoryImage?productCategoryId=${productCategoryId}&amp;upload_file_type=linkTwo</@ofbizUrl>");'/>${uiLabelMap.ProductLinkTwoImageUrl}
            </@field>
            <@field type="submitarea">
                <input type="submit" class="${styles.link_action!}" value="${uiLabelMap.ProductUploadImage}"/>
            </@field>
          </@fields>
        </form>
    </@section>
</#if>