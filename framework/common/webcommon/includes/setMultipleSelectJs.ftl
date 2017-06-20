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
