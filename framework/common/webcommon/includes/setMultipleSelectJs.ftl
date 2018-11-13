<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#if asm_multipleSelect??> <#-- we check only this var and suppose the others are also present -->
  <@dynamicSelectFieldScript id=asm_multipleSelect!"" title=asm_title!"" sortable=asm_sortable!false formId=asm_multipleSelectForm!""
    relatedFieldId=asm_relatedField!"" relatedTypeName=asm_type!"" relatedTypeFieldId=asm_typeField!""
    paramKey=asm_paramKey!"" requestName=asm_requestName!"" responseName=asm_responseName!"" />

<#-- SCIPIO: FIXME: this greaks grid 
<style type="text/css">
#${asm_multipleSelectForm} {
    width: ${asm_formSize!700}px; 
    position: relative;
}

.asmListItem {
  width: ${asm_asmListItemPercentOfForm!95}%; 
}
</style>
-->
</#if>
