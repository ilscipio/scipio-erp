<#-- SCIPIO -->

<#include "component://webtools/webapp/webtools/common/common.ftl">

<#macro eiUnsafeEntityField values>
    <#-- SCIPIO: NOTE: the submitted disallowDangerousEntitiesWarn is the opposite boolean of the label text, 
        for service interface reasons -->
    <#-- TODO: better checked logic, right now inconsistent with rest of forms: checked=((values.disallowUnsafeEntityWarn!) == "false")  -->
    <@field type="checkbox" name="disallowUnsafeEntityWarn" 
        value="false" altValue="true"
        checked=false
        label=getLabel('WebtoolsAllowUnsafeEntitiesNamed', '', 
            {"unsafeEntityNames": Static["org.ofbiz.entity.util.EntityUtil"].getUnsafeEntitiesForUpdate(delegator)?join(", ")}) 
        tooltip=uiLabelMap.WebtoolsAllowUnsafeEntitiesDesc/>
</#macro>