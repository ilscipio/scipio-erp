<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
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
