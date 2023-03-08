<@section title="Import ExcelData">
    <@form name="excelI18nImport" method="POST" action="excelI18nImport" enctype="multipart/form-data">
        <@field type="select" name="templateName" label="Template" class="${styles.field_select_default!}">
            <#assign xslxTemplateProperties = Static["org.ofbiz.base.util.UtilProperties"].getMergedPropertiesFromAllComponents("ExcelImport")/>
            <#if xslxTemplateProperties?has_content>
                <#assign templateNames = Static["org.ofbiz.base.util.UtilProperties"].getPropertiesWithPrefixSuffix(xslxTemplateProperties, "xlsx.",".xlsxTemplateName",true,false,false)/>
                <#if templateNames?has_content>
                    <#list templateNames.keySet() as key>
                        <#assign templateName = templateNames.get(key)!"na"/>
                        <option value="${key}"><#if getLabel(templateName)?has_content>${getLabel(templateName)}<#else>${templateName}</#if></option>
                    </#list>
                </#if>
            </#if>
        </@field>
        <@field type="file" name="uploadedFile" label="*.xlsx" required=true attribs={"accept":"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"} />
        <@field type="submitarea">
            <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_add!}" />
        </@field>
    </@form>
</@section>