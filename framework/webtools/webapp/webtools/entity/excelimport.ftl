<@section>
    <@form name="excelI18nImport" method="POST" action="excelI18nImport" enctype="multipart/form-data">
        <@row>
            <@cell columns=6>
                <@field type="select" name="templateName" id="excelTemplateName" required=true label="Template" class="${styles.field_select_default!}" onChange="loadCurrentTemplateExample();">
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
            </@cell>
        </@row>
        <@row>
            <@cell columns=6>
                <@field type="file" name="uploadedFile" label=getLabel("ContentFile") required=true attribs={"accept":"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"} />
            </@cell>
            <@cell columns=4>
                <@field type="generic" label=getLabel("Example","CommonUiLabels")>
                    <a href="#" target="_blank" id="excelDownloadAnchor" class="${styles.link_run_local_inline!} ${styles.text_color_primary!}">${getLabel("ContentDownload")}</a>
                </@field>
            </@cell>
        </@row>
        <@row>
            <@cell columns=6>
                <@field type="number" name="startRow" label=getLabel("EntityExcelImportStartRow") />
            </@cell>
            <@cell columns=4>
                <@field type="number" name="endRow" label=getLabel("EntityExcelImportEndRow") />
            </@cell>
        </@row>
        <@row>
            <@cell columns=6>
                <@field type="select" name="serviceMode" id="excelServiceMode" required=true label="Service Mode" class="${styles.field_select_default!}">
                    <option value="sync"<#if "sync" == parameters.serviceMode!> selected="selected"</#if>>sync</option>
                    <option value="async"<#if "async" == parameters.serviceMode!> selected="selected"</#if>>async</option>
                    <option value="async-persist"<#if "async-persist" == parameters.serviceMode!> selected="selected"</#if>>async-persist</option>
                </@field>
            </@cell>
        </@row>
        <@row>
            <@cell>
                <@field type="submitarea">
                    <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                </@field>
            </@cell>
        </@row>
    </@form>
</@section>

<@script>
    var templateDownloadLocations = {<#t>
    <#assign templateLocations = Static["org.ofbiz.base.util.UtilProperties"].getPropertiesWithPrefixSuffix(xslxTemplateProperties, "xlsx.",".xlsxReference",true,false,false)!""/><#t>
    <#if templateLocations?has_content>
        <#list templateLocations.keySet() as key>
            '${key}' : '${raw(templateLocations[key])}',
        </#list>
    </#if>
    };

    function loadCurrentTemplateExample(){
        document.getElementById("excelDownloadAnchor").href= templateDownloadLocations[document.getElementById("excelTemplateName").value];
    }
    loadCurrentTemplateExample();
</@script>