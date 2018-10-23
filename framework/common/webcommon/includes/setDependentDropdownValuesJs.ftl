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
<#assign requestName = makeOfbizUrl(requestName)/>
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
