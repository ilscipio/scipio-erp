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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<#assign requestName = makePageUrl(requestName)/>
<@script>
jQuery(document).ready(function() {
    <#-- SCIPIO: added depFormFieldPrefix as workaround for some form name issues -->
    <#if !depFormFieldPrefix??>
      <#assign depFormFieldPrefix = raw(dependentForm) + "_">
    </#if>
    <#assign mainIdFull = raw(depFormFieldPrefix) + raw(mainId)>
    <#assign depIdFull = raw(depFormFieldPrefix) + raw(dependentId)>
    if (jQuery('[id^=${escapeVal(dependentForm, 'js')}]').length && jQuery('select[id^=${escapeVal(mainIdFull, 'js')}], input[id^=${escapeVal(mainIdFull, 'js')}]').length) {
      <#-- SCIPIO: 4.0.0: bind the control, not its container (id + "_container"); change bubbles, so both would fire -->
      jQuery('select[id^=${escapeVal(mainIdFull, 'js')}], input[id^=${escapeVal(mainIdFull, 'js')}]').change(function(e, data) {
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
});
</@script>
