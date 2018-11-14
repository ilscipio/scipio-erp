<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if asm_listField??> <#-- we check only this var and suppose the others are also present -->
    <#list asm_listField as row>
      <#-- SCIPIO: we've taken this over in macro form so more reusable -->
      <@dynamicSelectFieldScript id=row.asm_multipleSelect!"" title=row.asm_title!"" sortable=row.asm_sortable!false formId=asm_multipleSelectForm!""
        relatedFieldId=row.asm_relatedField!"" relatedTypeName=row.asm_type!"" relatedTypeFieldId=row.asm_typeField!""
        paramKey=row.asm_paramKey!"" requestName=row.asm_requestName!"" responseName=row.asm_responseName!"" />
    </#list>
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
