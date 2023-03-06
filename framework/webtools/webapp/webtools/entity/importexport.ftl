<@section title="Import i18n CategoryData">
    <@form name="excelI18nImport" method="POST" action="excelI18nImport" enctype="multipart/form-data">
        <@field type="select" name="templateName" label="Template" class="${styles.field_select_default!}">
            <option value="category">Category</option>
        </@field>
        <@field type="file" name="uploadedFile" label="*.xlsx" required=true attribs={"accept":"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"} />
        <@field type="submitarea">
            <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_add!}" />
        </@field>
    </@form>
</@section>