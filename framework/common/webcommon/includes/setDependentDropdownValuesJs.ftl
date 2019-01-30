<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign requestName = makePageUrl(requestName)/>
<@script>
jQuery(document).ready(function() {
    <#-- SCIPIO: added depFormFieldPrefix as workaround for some form name issues -->
    <#if !depFormFieldPrefix??>
      <#assign depFormFieldPrefix = rawString(dependentForm) + "_">
    </#if>
    <#assign mainIdFull = rawString(depFormFieldPrefix) + rawString(mainId)>
    <#assign depIdFull = rawString(depFormFieldPrefix) + rawString(dependentId)>
    if (jQuery('#${escapeVal(dependentForm, 'js')}').length && jQuery('#${escapeVal(mainIdFull, 'js')}').length) {
      jQuery('#${escapeVal(mainIdFull, 'js')}').change(function(e, data) {
          getDependentDropdownValues('${escapeVal(requestName, 'js')}', 
            '${escapeVal(paramKey, 'js')}', 
            '${escapeVal(mainIdFull, 'js')}', 
            '${escapeVal(depIdFull, 'js')}', 
            '${escapeVal(responseName, 'js')}', 
            '${escapeVal(dependentKeyName, 'js')}', 
            '${escapeVal(descName, 'js')}', 
            '_previous_');
      });
      getDependentDropdownValues('${escapeVal(requestName, 'js')}', 
        '${escapeVal(paramKey, 'js')}', 
        '${escapeVal(mainIdFull, 'js')}', 
        '${escapeVal(depIdFull, 'js')}', 
        '${escapeVal(responseName, 'js')}', 
        '${escapeVal(dependentKeyName, 'js')}', 
        '${escapeVal(descName, 'js')}', 
        '${escapeVal(selectedDependentOption, 'js')}');
      <#if (focusFieldName??)>
        jQuery('#${escapeVal(focusFieldName, 'js')}').focus();
      </#if>
    }
})
</@script>
