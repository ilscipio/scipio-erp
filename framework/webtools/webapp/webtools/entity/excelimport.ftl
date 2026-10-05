<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
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