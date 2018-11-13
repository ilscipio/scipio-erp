<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if description??>
    <#if autocompleteOptions??>
        <#list autocompleteOptions as autocompleteOption>
            <#assign displayString = ""/>
            <#list displayFieldsSet as key>
                <#assign field = autocompleteOption.get(key)!>
                <#if field?has_content>
                    <#if (key != context.returnField)>
                        <#assign displayString = displayString + field + " ">
                    </#if>
                </#if>
            </#list>
            <#if (displayString?trim?has_content)>${displayString?trim}</#if>
        </#list>
    </#if>
<#else>
<@script>
var autocomp = [
    <#if autocompleteOptions?has_content>
        <#if !displayReturnField??>
            <#assign displayReturnField = getPropertyValue("widget.properties", "widget.autocompleter.displayReturnField")!"">
        </#if>
        <#list autocompleteOptions as autocompleteOption>
            {
            <#assign displayString = ""/>
            <#assign returnField = ""/>
            <#list displayFieldsSet as key>
              <#assign field = rawString(autocompleteOption.get(key)!)>
              <#if field?has_content>
                  <#if (key == context.returnField)>
                      <#assign returnField = field/>
                  <#else>
                      <#assign displayString = displayString + rawString(field) + " ">
                  </#if>
              </#if>
            </#list>
            <#if ("Y" == displayReturnField)>
                <#assign displayString = displayString +  "[" + returnField + "]">
            </#if>
            "id": "${escapeVal(returnField, 'js')}",
            "label": "<#if (displayString?trim?has_content)>${escapeVal(displayString?trim, 'js')}<#else>${escapeVal(returnField, 'js')}</#if>",
            "value": "${escapeVal(returnField, 'js')}"
            }<#if autocompleteOption_has_next>,</#if>
        </#list>
    <#else>
      {
         "id": "",
         "label": "${escapeVal(uiLabelMap.CommonNoRecordFound, 'js')}",
         "value": ""
      }
    </#if>
    ];
</@script>
</#if>
