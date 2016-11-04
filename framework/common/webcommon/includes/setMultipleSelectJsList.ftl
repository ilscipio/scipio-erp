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
<#if asm_listField??> <#-- we check only this var and suppose the others are also present -->
    <#list asm_listField as row>
      <#-- SCIPIO: we've taken this over in macro form so more reusable -->
      <@asmSelectScript id=row.asm_multipleSelect!"" title=row.asm_title!"" sortable=row.asm_sortable!false formId=asm_multipleSelectForm!""
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
