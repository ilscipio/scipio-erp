<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
    function updateAndSaveLabel() {
        document.UpdateLabelForm.action="<@pageUrl>SaveLabelsToXmlFile</@pageUrl>";
        document.UpdateLabelForm.submit();
    }
</@script>
<@section>
    <form method="post" action="<@pageUrl>SaveLabelsToXmlFile</@pageUrl>" name="UpdateLabelForm">
        <@table type="fields" class="+${styles.table_spacing_small_hint!}">
            <@tr>
                <@td colspan="2">&nbsp;</@td>
            </@tr>
            <@tr>
                <@td>${uiLabelMap.WebtoolsLabelManagerKey}</@td>
                <@td>
                    <#if parameters.sourceKey??>
                        ${parameters.sourceKey}
                        <input type="hidden" name="key" value="${parameters.sourceKey}" />
                        <input type="hidden" name="update_label" value="Y" />
                    <#else>
                        <input type="text" name="key" size="70" />
                        <input type="hidden" name="update_label" value="N" />
                    </#if>
                </@td>
            </@tr>
            <@tr>
                <@td>${uiLabelMap.WebtoolsLabelManagerKeyComment}</@td>
                <@td>
                    <input type="text" name="keyComment" size="70" value="${parameters.sourceKeyComment!}" />
                </@td>
            </@tr>
            <@tr>
                <@td>${uiLabelMap.WebtoolsLabelManagerFileName}</@td>
                <@td>
                    <#if parameters.sourceFileName??>
                        ${parameters.sourceFileName}
                        <input type="hidden" name="fileName" value="${parameters.sourceFileName}" />
                    <#else>
                        <select name="fileName">
                            <#list filesFound as fileInfo>
                              <#assign fileName = fileInfo.file.getName()/>
                              <option <#if parameters.fileName?? && parameters.fileName == fileName>selected="selected"</#if> value="${fileName}">${fileName}</option>
                            </#list>
                        </select>
                    </#if>
                </@td>
            </@tr>
            <@tr>
                <@td colspan="2" align="center">
                    <input type="submit" value="${uiLabelMap.CommonBack}" class="${styles.link_nav_cancel!}"/>
                    <#if parameters.sourceKey??>
                        <input type="submit" value="${uiLabelMap.WebtoolsLabelManagerRemove}" name="removeLabel" class="${styles.link_run_sys!} ${styles.action_remove!}"/>
                        <input type="submit" value="${uiLabelMap.CommonUpdate}" name="confirm" onclick="javascript:updateAndSaveLabel()" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                    <#else>
                        <input type="submit" value="${uiLabelMap.CommonAdd}" name="confirm" class="${styles.link_run_sys!} ${styles.action_add!}"/>
                        <input type="submit" value="${uiLabelMap.WebtoolsLabelManagerUpdateAndSave}" name="confirm" onclick="javascript:updateAndSaveLabel()" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                    </#if>
                </@td>
            </@tr>
            <#list localesFound as localeFound>
                <#assign labelValue = "">
                <#assign labelComment = "">
                <#if parameters.sourceKey??>
                    <#assign value = (label.getLabelValue(localeFound))!>
                    <#if value?has_content>
                        <#assign labelValue = value.getLabelValue()>
                        <#assign labelComment = value.getLabelComment()>
                    </#if>
                </#if>
                <#assign showLocale = true>
                <#if parameters.labelLocaleName?? && parameters.labelLocaleName != "" && parameters.labelLocaleName != localeFound>
                    <#assign showLocale = false>
                </#if>
                <#if showLocale == true>
                    <@tr>
                        <#assign locale = UtilMisc.parseLocale(localeFound)!/>
                        <#if locale?? && locale?has_content>
                            <#assign langAttr = localeFound.toString()?replace("_", "-")>
                            <#assign langDir = "ltr">
                            <#if "ar.iw"?contains(langAttr?substring(0, 2))>
                                <#assign langDir = "rtl">
                            </#if>
                            <@td lang=langAttr dir=langDir>
                                ${locale.getDisplayName(locale)}
                            </@td>
                        <#else>
                            <@td>${localeFound}</@td>
                        </#if>
                        <@td>
                            <input type="hidden" name="localeNames" value="${localeFound}" />
                            <input type="text" name="localeValues" size="70" value="${labelValue!}" />
                            <input type="text" name="localeComments" size="70" value="${labelComment!}" />
                        </@td>
                    </@tr>
                </#if>
            </#list>
            <@tr>
                <@td colspan="2" align="center">
                    <input type="submit" value="${uiLabelMap.CommonBack}" class="${styles.link_nav_cancel!}"/>
                    <#if parameters.sourceKey??>
                        <input type="submit" value="${uiLabelMap.WebtoolsLabelManagerRemove}" name="removeLabel" class="${styles.link_run_sys!} ${styles.action_remove!}"/>
                        <input type="submit" value="${uiLabelMap.CommonUpdate}" name="confirm" onclick="javascript:updateAndSaveLabel()" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                    <#else>
                        <input type="submit" value="${uiLabelMap.CommonAdd}" name="confirm" class="${styles.link_run_sys!} ${styles.action_add!}"/>
                        <input type="submit" value="${uiLabelMap.WebtoolsLabelManagerUpdateAndSave}" name="confirm" onclick="javascript:updateAndSaveLabel()" class="${styles.link_run_sys!} ${styles.action_update!}"/>
                    </#if>
                </@td>
            </@tr>
        </@table>
    </form>
</@section>
