<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

${topLine.textBegin!}
<#assign listSize = topLine.dropDownList.size()>
<#if topLine.dropDownList.size() gt 1>
<form method="post" action="<@ofbizUrl>${topLine.action}</@ofbizUrl>" onsubmit="javascript:submitFormDisableSubmits(this)" name="${topLine.action}" id="${topLine.action}">
  <#if topLine.hiddenFieldList??>
    <#list topLine.hiddenFieldList as field>
      <input type="hidden" name="${field.name}" value="${field.value}"/>
    </#list>
  </#if>
  <select name="${topLine.selectionName!}" onchange="javascript:document['${escapeVal(topLine.action, 'js-html')}'].submit();">
    <#list topLine.dropDownList as option>
      <option <#if option.key == topLine.selectedKey>selected="selected"</#if> value="${option.key!}">${option.value!}</option>
    </#list>
  </select>
</form>
<#else>
  ${topLine.dropDownList[0].value!}
</#if>
${topLine.textEnd!}
