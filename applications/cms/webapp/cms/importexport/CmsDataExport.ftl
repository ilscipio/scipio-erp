<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#-- SCIPIO: based on component://webtools/webapp/webtools/entity/xmldsdump.ftl -->
<#assign entityPresetMap = rewrapMap(entityPresetMap!{}, "raw-simple")>

<style type="text/css"><#-- TODO: move out -->
.dataexport-entitysel .entityentry.entityprio-major label {
    font-weight:bold;
}
</style>
<#assign showAdvanced = (showAdvanced!parameters.showAdvanced!)?string>
<#if showAdvanced == "true">
  <#assign showAdvanced = true>
<#else>
  <#assign showAdvanced = false>
</#if>

<#macro advancedOptions>
  <div class="dataexport-advanced" style="<#if showAdvanced>visibility:visible; position:static;<#else>visibility:hidden; position:absolute;</#if>">
    <#nested>
  </div>
</#macro>

<#if doExport && (outputMode!?string) == "SF_DL">
  <@section><#-- title=uiLabelMap.PageTitleEntityExport -->
    <p>${uiLabelMap.CmsExportFuncInfo}</p>
    <#--<hr />-->
    <#if (useEntityMaintCheck!false)==false || security.hasPermission("ENTITY_MAINT", session)>
      <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("CmsDataExportRaw.xml") target="_blank" text=uiLabelMap.CmsClickHereToGetData class="+${styles.action_run_sys!} ${styles.action_export!}" />
      </@menu>
      <#-- SCIPIO: show options that were requested; screen looks deserted and gets confusing without -->
      <@section title=uiLabelMap.CommonOptions>
      <div>
        <#assign selectValue = (parameters.singleFileRecGrp!"")?string>
        <@field type="display" name="singleFileRecGrp" label=uiLabelMap.CmsRecordGrouping 
            value=(Static["com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker$RecordGrouping"].fromStringOrDefaultSafe(selectValue).getFullLabel(locale)!selectValue)/>
      </div>
      <div>
        <@field type="display" label=uiLabelMap.CmsExportFilesAsTextData name="exportFilesAsTextData" value=(parameters.exportFilesAsTextData!)/>
        <@field type="display" label=uiLabelMap.WebtoolsRecordsUpdatedSince name="entityFrom" value=(entityFrom!parameters.entityFrom!"") />
        <@field type="display" label=uiLabelMap.WebtoolsRecordsUpdatedBefore name="entityThru" value=(entityThru!parameters.entityThru!"") />
      </div>
      <div>
        <#-- TODO: human-readable values -->
        <@field type="display" name="includeContentRefs" label=uiLabelMap.CmsIncludeContentRecords value=(includeContentRefs!"") />
        <@field type="display" name="pmpsMappingTypeId" label=uiLabelMap.CmsProcessMappingPageSpecialMappingType value=(pmpsMappingTypeId!"") />
        <@field type="display" name="attribTmplAssocType" label=uiLabelMap.CmsAttributeTemplateAssocType value=(attribTmplAssocType!"") />
      </div>
      <div>
        <@field type="display" label=uiLabelMap.WebtoolsEntityNames value=(passedEntityNames!?join(", ")) />
      </div>
      </@section>
    <#else>
      <@commonMsg type="error">${uiLabelMap.WebtoolsPermissionMaint}</@commonMsg>
    </#if>
  </@section>
<#else>

<p>${uiLabelMap.CmsExportFuncInfo}</p>
<#--<hr />-->

<#if (useEntityMaintCheck!false)==false || security.hasPermission("ENTITY_MAINT", session)>

  <@script>
    jQuery(document).ready(function() {
    
        var checkAllEntityNames = function(state) {
            jQuery('#entityExportForm input[name=entityName]').each(function() {
                jQuery(this).prop('checked', state);
            });
        };
        var toggleAdvanced = function() {
            var showAdvanced = jQuery('#entityExportForm input[name=showAdvanced]').val() == "true" ? false : true;
            jQuery('#entityExportForm input[name=showAdvanced]').val(showAdvanced+'');
            jQuery('.dataexport-advanced').each(function() {
                var elem = jQuery(this);
                if (elem.css('visibility') == 'hidden') {
                    elem.css('position', 'static');
                    elem.css('visibility', 'visible');
                } else {
                    elem.css('visibility', 'hidden');
                    elem.css('position', 'absolute');
                }
            });
            var presetElem = jQuery('#preConfiguredSetName');
            var presetVal = presetElem.val();
            if (showAdvanced) {
                presetElem.html(jQuery('#preConfiguredSetNameAdvanced').html());
            } else {
                presetElem.html(jQuery('#preConfiguredSetNameSimple').html());
            }
            var exists = false;
            presetElem = jQuery('#preConfiguredSetName');
            for(var i = 0, opts = presetElem[0].options; i < opts.length; ++i) {
                if(opts[i].value === presetVal) {
                    exists = true;
                    presetElem.val(presetVal);
                    break;
                }
            }
            if (!exists) {
                presetElem.val('');
                presetElem.change();
            }
        };
        jQuery('.dataexport-entitysel-checkall').click(function() { checkAllEntityNames(true); });
        jQuery('.dataexport-entitysel-uncheckall').click(function() { checkAllEntityNames(false); });
        jQuery('.dataexport-toggle-advanced').click(function() { toggleAdvanced(); });
    
        var isMajorObjGrpMode = function() {
            var singleFileRecGrp = jQuery('#entityExportForm select[name=singleFileRecGrp]').val();
            var outputMode = jQuery('#entityExportForm input[name=outputMode]:checked').val();
            return (singleFileRecGrp === 'MAJOR_OBJECT' && (outputMode.indexOf("SF_") === 0));
        };
    
        jQuery("#entityExportForm").submit(function(event) {
            <#-- make sure entities selected -->
            var errmsg = null;
            if (isMajorObjGrpMode()) {
                if (jQuery('.dataexport-entitysel .entityentry.entityprio-major input:checked').length <= 0) {
                    errmsg = "${escapeVal(uiLabelMap.CmsPleaseSelectPresetOrMajorEntity, 'js')}";
                }
            } else {
                if (jQuery('.dataexport-entitysel .entityentry input:checked').length <= 0) {
                    errmsg = "${escapeVal(uiLabelMap.CmsPleaseSelectPresetOrEntity, 'js')}";
                }
            }
            if (errmsg) {
                event.preventDefault();
                alert(errmsg); <#-- TODO?: better box? -->
            }
        });

        var updateObjGrpMsgDisplay = function() {
            if (isMajorObjGrpMode()) {
                jQuery('#dataexport-entitysel-footnote').html("${escapeVal(uiLabelMap.CmsObjectGroupingSelectionsNote, 'js')}");
                jQuery('#dataexport-entitysel-footnote').fadeIn();
                jQuery('#entityExportForm .dataexport-opts-sfrecgrp-objgrp').fadeIn();
            } else {
                jQuery('#dataexport-entitysel-footnote').fadeOut();
                jQuery('#dataexport-entitysel-footnote').html('');
                jQuery('#entityExportForm .dataexport-opts-sfrecgrp-objgrp').fadeOut();
            }
        };

        var setDivDisabled = function(divClass, disabled) {
            if (disabled) {
                jQuery('#entityExportForm .' + divClass).fadeOut();
            } else {
                jQuery('#entityExportForm .' + divClass).fadeIn();
            }
            jQuery('#entityExportForm .' + divClass + ' input').prop('disabled', disabled);
            jQuery('#entityExportForm .' + divClass + ' select').prop('disabled', disabled);
        };

        <#-- Output Mode -->
        var changeOutputMode = function() {
            var outputMode = jQuery('#entityExportForm input[name=outputMode]:checked').val();
            setDivDisabled('dataexport-opts-spec', true);
            if (outputMode === "MF_FS") {
                setDivDisabled('dataexport-opts-mf', false);
                jQuery('#entityExportForm select[name=singleFileRecGrp]').prop('disabled', true);
            } else if (outputMode.indexOf('SF_') === 0) {
                jQuery('#entityExportForm select[name=singleFileRecGrp]').prop('disabled', false);
                setDivDisabled('dataexport-opts-sf-common', false);
                if (outputMode === "SF_FS") {
                    setDivDisabled('dataexport-opts-sf-fs', false);
                } else if (outputMode === "SF_IL") {
                    setDivDisabled('dataexport-opts-sf-il', false);
                }
            }
            updateObjGrpMsgDisplay();
        };
        changeOutputMode();
        jQuery('#entityExportForm input[name=outputMode]').change(changeOutputMode);
        jQuery('#entityExportForm select[name=singleFileRecGrp]').change(updateObjGrpMsgDisplay);

        <#-- Presets -->
        var entityPresetMap = <@objectAsScript lang='js' object=entityPresetMap />;
    
        var getPresetEntityNamesMap = function(presetEntry) {
            var namesToCheck = presetEntry ? presetEntry.entityNames : [];
            if (!namesToCheck) namesToCheck = [];
            var namesToCheckMap = {};
            jQuery.each(namesToCheck, function(i, e) {
                namesToCheckMap[e] = true;
            });
            return namesToCheckMap;
        };
    
        var applyCheckEntityNames = function(namesToCheckMap) {
            jQuery('#entityExportForm input[name=entityName]').each(function() {
                if (namesToCheckMap[this.value]) {
                    jQuery(this).prop('checked', true);
                } else {
                    jQuery(this).prop('checked', false);
                }
            });
        };
    
        var changePreConfiguredSetName = function(selElem) {
            selElem = jQuery(selElem);
            var presetEntry = entityPresetMap[selElem.val()];
            
            var namesToCheckMap = getPresetEntityNamesMap(presetEntry);
            applyCheckEntityNames(namesToCheckMap);
            
            var pmpsMappingTypeId = '';
            if (presetEntry && presetEntry.pmpsMappingTypeId) {
                pmpsMappingTypeId = presetEntry.pmpsMappingTypeId;
            }
            jQuery('#entityExportForm select[name=pmpsMappingTypeId]').val(pmpsMappingTypeId);
            
            var attribTmplAssocType = '';
            if (presetEntry && presetEntry.attribTmplAssocType) {
                attribTmplAssocType = presetEntry.attribTmplAssocType;
            }
            jQuery('#entityExportForm select[name=attribTmplAssocType]').val(attribTmplAssocType);
        };
        
        var selElem = jQuery('#preConfiguredSetName');
        <#-- don't do this, it will mess with user manual selections;
        changePreConfiguredSetName(selElem); -->
        <#-- don't clear it either, not needed
        selElem.val('');-->
        selElem.change(function() { changePreConfiguredSetName(this); });
    });
  </@script>

  <#-- SCIPIO: DEV NOTE: I've added this because it helps to debug faster and oftentime the queries are small -->
  <#if doExport && (outputMode!?string) == "SF_IL" && resultText?has_content>
    <form method="post" action="#" id="dataexport-output-form">
    <@fields type="default-compact">
      <@field type="textarea" id="exportResultText" label=uiLabelMap.CmsOutput name="output" value=resultText rows=30/>
    </@fields>
    </form>
  </#if>

  <form method="post" action="<@ofbizUrl>CmsDataExport</@ofbizUrl>" name="entityExport" id="entityExportForm">
    <input type="hidden" name="doExport" value="Y"/>
    <input type="hidden" name="showAdvanced" value="${showAdvanced?string}"/>
  <@section title=uiLabelMap.WebtoolsExport>
  <@row>
    <@cell columns=6>
      <@field type="generic" label=uiLabelMap.CmsOutput>
        <#list outputModeList as listOutputMode>
          <#assign fieldLabel = listOutputMode.getLabel(locale)!listOutputMode?string>
          <#assign fieldTooltip = listOutputMode.getDescLabel(locale)!>
          <@field type="radio" name="outputMode" inlineItems=false label=fieldLabel tooltip=fieldTooltip value=listOutputMode?string currentValue=(selOutputMode!?string)/>
        </#list>
      </@field>
      <div class="dataexport-opts-spec dataexport-opts-mf"<#if selOutputMode != "MF_FS"> style="display:none;"</#if>>
        <@field type="input" label=uiLabelMap.WebtoolsOutputDirectory size="60" name="outpath" value=(parameters.outpath!) required=true disabled=(selOutputMode != "MF_FS")/>
        <@field type="input" label=uiLabelMap.WebtoolsMaxRecordsPerFile size="10" name="maxrecords" value=(parameters.maxrecords!) disabled=(selOutputMode != "MF_FS")/>
        <p>${uiLabelMap.CmsExportFuncMultiFileInfo}</p>
      </div>
      <div class="dataexport-opts-spec dataexport-opts-sf-fs"<#if selOutputMode != "SF_FS"> style="display:none;"</#if>>
        <@field type="input" label=uiLabelMap.WebtoolsSingleFilename size="60" name="filename" value=(parameters.filename!) required=true disabled=(selOutputMode != "SF_FS")/>
      </div>
      <div class="dataexport-opts-spec dataexport-opts-sf-il"<#if selOutputMode != "SF_IL"> style="display:none;"</#if>>
        <p>${uiLabelMap.CmsLargeResultsSlowBrowser}</p>
      </div>
    </@cell>
    <@cell columns=6>
    <#-- readd this if get more field common to sf methods
      <div class="dataexport-opts-spec dataexport-opts-sf-common"<#if selOutputMode.isMulti()> style="display:none;"</#if>>
      </div>
    -->
      <div class="dataexport-opts-common">
        <@field type="select" name="singleFileRecGrp" label=uiLabelMap.CmsRecordGrouping disabled=(selOutputMode.isMulti()) tooltip=uiLabelMap.CmsRecordGroupingDesc>
          <#assign singleFileRecGrp = (parameters.singleFileRecGrp!"")?string>
          <#if !singleFileRecGrp?has_content>
            <#assign singleFileRecGrp = "NONE">
          </#if>
        <#list Static["com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker$RecordGrouping"].getDisplayValues() as recGrp>
          <@field type="option" value=recGrp?string selected=(singleFileRecGrp == recGrp?string) disabled=true>${recGrp.getFullLabel(locale)!recGrp?string}</@field>
        </#list>
        </@field>
        <div class="dataexport-opts-sfrecgrp-objgrp">
          <#-- DEV NOTE: The reason users may want this is because it affects the export order in a way that may be desirable (maybe it's more natural)
            Otherwise having includeMajorDeps here isn't that useful without a PK filter, but we need it for testing anyway... -->
          <@field type="checkbox" name="includeMajorDeps" value="Y" label=uiLabelMap.CmsReorderByDependencies tooltip=uiLabelMap.CmsReorderByDependenciesDesc checked=("Y" == parameters.includeMajorDeps!)/>
        </div>
        <@field type="checkbox" name="exportFilesAsTextData" value="Y" label=uiLabelMap.CmsExportFilesAsTextData tooltip=uiLabelMap.CmsExportFilesAsTextDataDesc checked=("Y" == parameters.exportFilesAsTextData!)/>
      </div>
    </@cell>
  </@row>
  </@section>
  <#--<hr />-->
  <@section title=uiLabelMap.CmsSelectionsAndFilters>
    <@row>
        <@cell columns=6>
          <#--<@field type="input" name="entitySyncId" size="30" value=(entitySyncId!) label=uiLabelMap.WebtoolsEntitySyncDump/>-->
          <#assign preConfiguredSetName = preConfiguredSetName!parameters.preConfiguredSetName!"">
          <@field type="select" label=uiLabelMap.WebtoolsPreConfiguredSet name="preConfiguredSetName" id="preConfiguredSetName">
            <@field type="option" value="" selected=(!preConfiguredSetName?has_content)>${uiLabelMap.CommonNone}</@field>
            <#if showAdvanced>
              <#assign presetNames = entityAllPresetNames>
            <#else>
              <#assign presetNames = entitySimplePresetNames>
            </#if>
            <#list presetNames as presetName>
              <#assign preset = entityPresetMap[presetName]>
              <@field type="option" value=(preset.presetName!) selected=(preConfiguredSetName == preset.presetName!)><#if preset.labelName?has_content>${uiLabelMap[preset.labelName]}</#if> [${preset.presetName!}]</@field>
            </#list>
          </@field>
          <#-- these are just here to hold the different options for simple vs advanced, to simplify the JS -->
          <div style="display:none;">
            <@field type="select" label=uiLabelMap.WebtoolsPreConfiguredSet name="preConfiguredSetNameSimple" id="preConfiguredSetNameSimple">
              <@field type="option" value="">${uiLabelMap.CommonNone}</@field>
              <#list entitySimplePresetNames as presetName>
                <#assign preset = entityPresetMap[presetName]>
                <@field type="option" value=(preset.presetName!)><#if preset.labelName?has_content>${uiLabelMap[preset.labelName]}</#if> [${preset.presetName!}]</@field>
              </#list>
            </@field>
            <@field type="select" label=uiLabelMap.WebtoolsPreConfiguredSet name="preConfiguredSetNameAdvanced" id="preConfiguredSetNameAdvanced">
              <@field type="option" value="">${uiLabelMap.CommonNone}</@field>
              <#list entityAllPresetNames as presetName>
                <#assign preset = entityPresetMap[presetName]>
                <@field type="option" value=(preset.presetName!)><#if preset.labelName?has_content>${uiLabelMap[preset.labelName]}</#if> [${preset.presetName!}]</@field>
              </#list>
            </@field>
          </div>
        <@advancedOptions>
          <@field type="datetime" label=uiLabelMap.WebtoolsRecordsUpdatedSince name="entityFrom" value=(entityFrom!parameters.entityFrom!"") size="25" maxlength="30" id="entityFrom1" />
          <@field type="datetime" label=uiLabelMap.WebtoolsRecordsUpdatedBefore name="entityThru" value=(entityThru!parameters.entityThru!"") size="25" maxlength="30" id="entityThru1" />
        </@advancedOptions>
        </@cell>
        <@cell columns=6>
        <@advancedOptions>
          <#--<@section title=uiLabelMap.CmsSpecialFilters>-->
            <#-- DEV NOTE: to simplify the code and allow easier additions, just make all these drop-downs, not checkbox (may add more 3rd values at any moment) -->
            <#-- NOTE: includeContentRefs is always true by default, because everything depends on it, and it automatically filters itself -->
            <@field type="select" name="includeContentRefs" label=uiLabelMap.CmsIncludeContentRecords tooltip=uiLabelMap.CmsIncludeContentRecordsDesc
                items=includeContentRefsOpts currentValue=(includeContentRefs!"") />
            <@field type="select" name="pmpsMappingTypeId" label=uiLabelMap.CmsProcessMappingPageSpecialMappingType tooltip=uiLabelMap.CmsProcessMappingPageSpecialMappingTypeDesc
                items=([{"value":"", "description":uiLabelMap.CommonAny}]+pmpsMappingTypeOpts) currentValue=(pmpsMappingTypeId!"") />
            <@field type="select" name="attribTmplAssocType" label=uiLabelMap.CmsAttributeTemplateAssocType tooltip=uiLabelMap.CmsAttributeTemplateAssocTypeDesc
                items=([{"value":"", "description":uiLabelMap.CommonAny}]+attribTmplAssocTypeOpts) currentValue=(attribTmplAssocType!"") />
          <#--</@section>-->
        </@advancedOptions>
        </@cell>
    </@row>
    
    
    <#macro displayButtonBar menuArgs={} includeCheckAll=true includeToggleAdv=false>
      <@menu args=menuArgs><#-- type="button" -->
        <@menuitem type="submit" text=uiLabelMap.WebtoolsExport class="+${styles.action_run_sys!} ${styles.action_export!}" />
      <#if includeToggleAdv>
         <@menuitem type="link" text=uiLabelMap.CommonAdvanced class="+${styles.action_run_local!} ${styles.action_show!} dataexport-toggle-advanced" />
      </#if>
      <#if includeCheckAll>
        <@menuitem type="link" text=uiLabelMap.WebtoolsCheckAllPlain class="+${styles.action_run_local!} ${styles.action_select!} dataexport-entitysel-checkall" />
        <@menuitem type="link" text=uiLabelMap.WebtoolsUnCheckAll class="+${styles.action_run_local!} ${styles.action_select!} dataexport-entitysel-uncheckall" />
      </#if>
      </@menu>
    </#macro>
  <@advancedOptions>
    <#--<@displayButtonBar menuArgs={"type":"button"}/>-->
    <@section title=uiLabelMap.WebtoolsEntityNames menuContent=displayButtonBar containerClass="+dataexport-entitysel">
      <#assign entCount = 0>
      <#assign checkAll = parameters.checkAll!false>
      <#if !checkAll?is_boolean>
        <#if checkAll?is_string>
          <#if checkAll?string == "true">
            <#assign checkAll = true>
          <#else>
            <#assign checkAll = false>
          </#if>
        <#else>
          <#assign checkAll = false>
        </#if>
      </#if>
      <#assign entityNamesByPkg = toSimpleMap(entityNamesByPkg)>
      <#assign passedEntityNames = passedEntityNames![]>
      <#list entityNamesByPkg?keys as subPkg>
        <#assign subEntities = entityNamesByPkg[rawString(subPkg)]>
        <#if (subPkg_index > 0)><hr/></#if>
        <@heading relLevel=+1>${subPkg}</@heading>
        <@grid>
          <#list subEntities as entityName>
            <#assign curEntityName = rawString(entityName)>
            <#assign entCount = entCount + 1>
            <#assign check = checkAll/>
            <#if passedEntityNames?seq_contains(curEntityName)>
              <#assign check = true/>
            </#if>
            <#assign fieldTooltip = "">
            <#assign fieldLabel = rawLabel('CmsEntityEntryLabel_'+curEntityName)!>
            <#if !fieldLabel?has_content>
              <#assign fieldLabel = curEntityName>
            </#if>
            <#assign cntrClass = "entityentry entityname-"+curEntityName>
            <#if majorEntityNames?seq_contains(curEntityName)>
                <#assign fieldTooltip = uiLabelMap.CmsMajorEntityInfo>
                <#assign cntrClass = cntrClass + " entityprio-major">
            <#else>
                <#assign cntrClass = cntrClass + " entityprio-minor">
            </#if>
            <li class="${escapeVal(cntrClass, 'html')}"><@field type="checkbox" name="entityName" checked=check id=curEntityName value=curEntityName label=fieldLabel norows=true nocells=true tooltip=fieldTooltip/></li>
          </#list>
        </@grid>
      </#list>
      <#-- FIXME: get rid of duplication... -->
      <p id="dataexport-entitysel-footnote" style="display:none;"></p>
    </@section>
  </@advancedOptions>
  
    <@displayButtonBar menuArgs={"type":"button"} includeCheckAll=false includeToggleAdv=true/>
  </@section>
  </form>
  
<#else>
    <@commonMsg type="error">${uiLabelMap.WebtoolsPermissionMaint}</@commonMsg>
</#if>

</#if>