<#-- SCIPIO -->
<#import "component://common/webcommon/includes/listLocalesMacros.ftl" as listLocaleMacros>

<#-- DEV NOTE: does not support tenant delegator -->

<@section>
<@form id="label-resource-select-form" method="get" action=makePageUrl("EntityLabels")>
  <#assign submitMarkup>
    <@field type="submit" class="${styles.link_run_local!} ${styles.color_green!}" text=uiLabelMap.CommonSelect/>
  </#assign>
    <@field type="select" name="resourceId" label=uiLabelMap.CommonResource postfix=true postfixContent=submitMarkup postfixColumns=1>
      <#list toSimpleMap(resourceFiles!{}) as resName, resInfo>
        <option<#if raw(resourceId!) == raw(resName)> selected="selected"</#if>>${resName}</option>
      </#list>
    </@field>
</@form>
</@section>

<#if resourceInfo??>
<hr/>

<@script>
    function addLabelEntryRow(id) {
        var container = $('#'+id+' .label-entries');
        var lastEntry = $('.label-lang-entry:last', container);
        var entryRow = parseInt(lastEntry.data('label-entry-row')) + 1;
        var markup = $('#label-empty-entry-template').html().replaceAll("_ENTRY_ROW_", entryRow+"");
        container.append(markup);
    }
</@script>
<#macro labelEntries id="" staticLocaleValueMap={} entityLocaleValueMap={} display=true>
    <@field id=id class="label-entries-field" type="generic" style=display?then("", "display:none")>
        <div class="label-entries">
          <#local entryRow = 0>
          <#list (toSimpleMap(staticLocaleValueMap) + toSimpleMap(entityLocaleValueMap)) as lang, value>
              <@labelEntry entryRow=entryRow lang=lang staticValue=(staticLocaleValueMap[raw(lang)]!) entityValue=(entityLocaleValueMap[raw(lang)]!)/>
              <#local entryRow = entryRow + 1>
          </#list>
          <@labelEntry entryRow=entryRow/>
        </div>
        <div class="label-add-entry"><a href="javascript:void(0);" onClick="addLabelEntryRow('${escapeVal(id, 'js')}_container');">[+]</a></div>
    </@field>
</#macro>

<#macro labelEntry entryRow lang="" staticValue="" entityValue="">
  <div class="label-lang-entry" data-label-entry-row="${entryRow}">
    <#-- TODO: REVIEW: the row/cell styling doesn't work when this inserted with js -->
    <@fields type="default-compact">
    <@row>
        <@cell small=2>
            <#-- FIXME: these locale codes don't match the label ones well enough - they're obvious anyway
            <@field type="select" name=("lang_o_"+entryRow) class="label-lang">
                <@listLocaleMacros.availableLocalesOptions currentLocale=raw(lang) expandCountries=true allowEmpty=false allowExtra=true allowExtraEmpty=true/>
            </@field>-->
            <@field type="text" name=("lang_o_"+entryRow) value=lang class="label-lang"/>
        </@cell>
        <@cell small=10>
            <@field type="text" name=("value_o_"+entryRow) value=entityValue placeholder=staticValue class="label-value"/>
        </@cell>
    </@row>
    </@fields>
  </div>
</#macro>

<div id="label-empty-entry-template" style="display:none">
    <@labelEntry entryRow="_ENTRY_ROW_"/>
</div>
<div id="label-entry-template" style="display:none">
    <@labelEntry entryRow="_ENTRY_ROW_" staticValue="_STATIC_VALUE_" entityValue="_ENTITY_VALUE_"/>
</div>

<@section title=uiLabelMap.CommonCreate>
    <@form id="label-create-form" method="post" action=makePageUrl("updateLocalizedPropertyMulti")>
        <#assign submitMarkup>
    </#assign>
        <input type="hidden" name="resourceId" value="${resourceId}"/>
        <@field type="text" name="propertyId" label=uiLabelMap.CommonProperty required=true/>
        <@labelEntries id="labels-add-entries"/>
        <@field type="submit" class="${styles.link_run_local!} ${styles.action_add!}" text=uiLabelMap.CommonSubmit/>
    </@form>
</@section>

<@script>
    function selectOverrideLabelProperty() {
        var entries = $('#labels-ovrd-entries_container .label-entries');
        entries.empty();
        var propertyId = $('#label-ovrd-form select[name=propertyId]').val();
        console.log('propertyId: ' + propertyId);
        if (!propertyId) {
            return;
        }
        $.ajax({
            type: "POST",
            url: "<@pageUrl escapeAs='js'>getLocalizedPropertyValues</@pageUrl>",
            data: {
                resourceId: "${escapeVal(resourceId, 'js')}",
                propertyId: propertyId
            },
            cache:false,
            async:false,
            success: function(data) {
                entries.empty(); // again just in case
                var langValueMap = data.langValueMap;
                if (langValueMap != null) {
                    var entityLangValueMap = data.entityLangValueMap;
                    var staticLangValueMap = data.staticLangValueMap;
                    var entryRow = 0;
                    var tmpl = $('#label-entry-template').html();
                    $.each(langValueMap, function(lang, v) {
                        var staticValue = staticLangValueMap ? staticLangValueMap[lang] : null;
                        var entityValue = entityLangValueMap ? entityLangValueMap[lang] : null;
                        var newTmpl = tmpl.replaceAll('_ENTRY_ROW_', entryRow+"")
                            .replaceAll('_STATIC_VALUE_', staticValue ? staticValue : "")
                            .replaceAll('_ENTITY_VALUE_', entityValue ? entityValue : "");
                        newTmpl = $($.parseHTML(newTmpl));
                        $('.label-lang', newTmpl).val(lang);
                        if (entityValue) {
                            $('.label-value', newTmpl).val(entityValue);
                        }
                        entries.append(newTmpl);
                        entryRow++;
                    });
                    var emptyTmpl = $('#label-empty-entry-template').html().replaceAll('_ENTRY_ROW_', entryRow+"");
                    entries.append(emptyTmpl);
                }
            }
        });
    }
    $(document).ready(function() {
        selectOverrideLabelProperty();
    });
</@script>

<#assign allPropertyNames = resourceInfo.allPropertyNames!>
<#if allPropertyNames?has_content>

<@section title=getLabel('ProductOverride', 'ProductUiLabels')>
    <@form id="label-ovrd-form" method="post" action=makePageUrl("updateLocalizedPropertyMulti")>
        <input type="hidden" name="resourceId" value="${resourceId}"/>
        <@field type="select" name="propertyId" label=uiLabelMap.CommonProperty required=true onChange="selectOverrideLabelProperty();">
            <#list toSimpleMap(allPropertyNames) as propertyId>
                <option>${propertyId}</option>
            </#list>
        </@field>
        <@labelEntries id="labels-ovrd-entries"/>
        <@field type="submit" class="${styles.link_run_local!} ${styles.action_update!}" text=uiLabelMap.CommonSubmit/>
    </@form>
</@section>

</#if>

<@script>
    function submitLabelListEntry(id) {
        $('#'+id).submit();
        return false;
    }
</@script>
<@section title="${rawLabel('CommonEntityProperties')}: ${raw(resourceId)}">
    <@table type="data-list">
        <@thead>
            <@tr>
                <@th width="200px">${uiLabelMap.CommonProperty}</@th>
                <@th>${uiLabelMap.CommonValues}</@th>
                <@th width="50px"></@th>
            </@tr>
        </@thead>
        <@tbody>
            <#list toSimpleMap(resourceInfo.entityPropertyMap) as propertyId, entityPropMap>
                <@tr>
                    <@td>${propertyId}</@td>
                    <@td>
                      <@form id=("label-form-list-entries-"+raw(propertyId)) method="post" action=makePageUrl("updateLocalizedPropertyMulti")>
                          <input type="hidden" name="resourceId" value="${resourceId}"/>
                          <input type="hidden" name="propertyId" value="${propertyId}"/>
                          <@labelEntries id=("labels-list-entries-"+raw(propertyId)) display=true
                              staticLocaleValueMap=(resourceInfo.getStaticPropertyValues(raw(propertyId))!)
                              entityLocaleValueMap=entityPropMap/>
                      </@form>
                    </@td>
                    <@td>
                        <@field type="submit" submitType="link" class="${styles.link_run_local!} ${styles.action_update!}" text=uiLabelMap.CommonSubmit
                            href="javascript:submitLabelListEntry('label-form-list-entries-${escapeVal(propertyId, 'js')}');"/>
                    </@td>
                </@tr>
            </#list>
        </@tbody>
    </@table>
</@section>

</#if>