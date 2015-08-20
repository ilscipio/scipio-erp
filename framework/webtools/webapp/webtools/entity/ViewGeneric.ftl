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
<#assign enableEdit = parameters.enableEdit?default("false")>
<#--
<script language="JavaScript" type="text/javascript">
var numTabs=${(entity.getRelationsSize()+1)};
function ShowTab(lname) {
  for(inc=1; inc <= numTabs; inc++) {
    // document.getElementById('area' + inc).className = (lname == 'tab' + inc) ? 'screenlet' : 'topcontainerhidden';
    // style of topcontainerhidden 
    // .topcontainerhidden { POSITION: absolute; VISIBILITY: hidden; }
    var elem = document.getElementById('area' + inc);
    if (lname == 'tab' + inc){
      elem.className = 'screenlet';
    }
    else {
      elem.className = 'topcontainerhidden';
      elem.style.position = 'absolute';
      elem.style.visibility = 'hidden';
    }
  }
}
</script>-->
<@section title="${uiLabelMap.WebtoolsViewValue} ${uiLabelMap.WebtoolsForEntity} ${entityName}">

    <ul class="button-group">
      <li><a href='<@ofbizUrl>FindGeneric?entityName=${entityName}&amp;find=true&amp;VIEW_SIZE=50&amp;VIEW_INDEX=0</@ofbizUrl>' class="${styles.button_default!}">${uiLabelMap.WebtoolsBackToFindScreen}</a></li>
      <#if enableEdit = "false">
        <#if hasCreatePermission>
          <li><a href='<@ofbizUrl>ViewGeneric?entityName=${entityName}&amp;enableEdit=true</@ofbizUrl>' class="${styles.button_default!} create">${uiLabelMap.CommonCreateNew}</a></li>
          <li><a href="<@ofbizUrl>ViewGeneric?${curFindString}&amp;enableEdit=true</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonEdit}</a></li>
        </#if>
        <#if value?has_content>
          <#if hasDeletePermission>
            <li><a href='<@ofbizUrl>UpdateGeneric?UPDATE_MODE=DELETE&amp;${curFindString}</@ofbizUrl>' class="${styles.button_default!} delete">${uiLabelMap.WebtoolsDeleteThisValue}</a></li>
          </#if>
        </#if>
      </#if>
    </ul>
    <br/>
    <@nav type="magellan">
        <#if value?has_content><@mli arrival="xml-view"><a href="#xml-view">${uiLabelMap.WebtoolsEntityXMLRepresentation}</a></@mli></#if>
        <#--<@mli arrival="common-view"><a href="#common-view">${uiLabelMap.CommonView}</a></@mli>-->
        <@mli arrival="current-view"><a href="#current-view">${uiLabelMap.WebtoolsEntityCurrentValue}</a></@mli>
        <@mli arrival="related-view"><a href="#related-view">${uiLabelMap.WebtoolsRelatedEntity}</a></@mli>
    </@nav>
    <br/>
    
    <#if value?has_content>
    <@row>
        <@cell>
          <h3 data-magellan-destination="xml-view" id="xml-view">${uiLabelMap.WebtoolsEntityXMLRepresentation}</h3>
        
          <@code type="html">
              <#assign valueXmlDoc = Static["org.ofbiz.entity.GenericValue"].makeXmlDocument([value]) />${Static["org.ofbiz.base.util.UtilXml"].writeXmlDocument(valueXmlDoc)}
          </@code>
        
        </@cell>
    </@row>
</#if>    
    
    <#--
    <#if value?has_content>
    <@row>
            <@cell columns=6>
      <form name="relationForm" onchange="javascript:ShowTab(this.options[this.selectedIndex].value)">
         
        <h3 data-magellan-destination="common-view" id="common-view">${uiLabelMap.CommonView}</h3>
        <@field type="select" name="viewRelated">
          <option value="tab1">${entityName}</option>
          <#list relationFieldList as relation>
            <option value="tab${(relation_index+2)}">${relation.title}${relation.relEntityName} (${relation.type})</option>
          </#list>
        </@field>
      </form>
          </@cell>
    </@row>
    
    </#if>-->
    

   
    <#if enableEdit = "true" && ( hasUpdatePermission || hasCreatePermission)>
        <#assign alt_row = false>
           <@row>
            <@cell>
            <h3 data-magellan-destination="current-view" id="current-view">${uiLabelMap.WebtoolsEntityEditValue}</h3>
          <#if pkNotFound>
            <p>${uiLabelMap.WebtoolsEntityName} ${entityName} ${uiLabelMap.WebtoolsWithPk} ${findByPk} ${uiLabelMap.WebtoolsSpecifiedEntity2}.</p>
          </#if>
          <form action='<@ofbizUrl>UpdateGeneric?entityName=${entityName}</@ofbizUrl>' method="post" name="updateForm">
            <#assign showFields = true>
            <#assign alt_row = false>
            <table class="basic-table" cellspacing="0">
              <#if value?has_content>
                <#if hasUpdatePermission>
                  <#if newFieldPkList?has_content>
                    <input type="hidden" name="UPDATE_MODE" value="UPDATE"/>
                    <#list newFieldPkList as field>
                      <tr<@dataRowClassStr alt=alt_row />>
                        <td class=>${field.name}</td>
                        <td>
                          <input type="hidden" name="${field.name}" value="${field.value}"/>
                          ${field.value}
                        </td>
                      </tr>
                      <#assign alt_row = !alt_row>
                    </#list>
                  </#if>
                <#else>
                  <@alert type="error">${uiLabelMap.WebtoolsEntityCretePermissionError} ${entityName} ${plainTableName}</@alert>
                  <#assign showFields = false>
                </#if>
              <#else>
                <#if hasCreatePermission>
                  <#if newFieldPkList?has_content>
                    <p>${uiLabelMap.WebtoolsYouMayCreateAnEntity}</p>
                    <input type="hidden" name="UPDATE_MODE" value="CREATE"/>
                    <#list newFieldPkList as field>
                      <tr<@dataRowClassStr alt=alt_row />>
                        <td class=>${field.name}</td>
                        <td>
                          <#if field.fieldType == 'DateTime'>
                            DateTime(YYYY-MM-DD HH:mm:SS.sss):<@htmlTemplate.renderDateTimeField name="${field.name}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${field.value}" size="25" maxlength="30" id="${field.name}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                          <#elseif field.fieldType == 'Date'>
                            Date(YYYY-MM-DD):<@htmlTemplate.renderDateTimeField name="${field.name}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${field.value}" size="25" maxlength="30" id="${field.name}" dateType="date" shortDateInput=true timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                          <#elseif field.fieldType == 'Time'>
                            Time(HH:mm:SS.sss):<input type="text" size="6" maxlength="10" name="${field.name}" value="${field.value}" />
                          <#elseif field.fieldType == 'Integer'>
                            <input type="text" size="20" name="${field.name}" value="${field.value}" />
                          <#elseif field.fieldType == 'Long'>
                            <input type="text" size="20" name="${field.name}" value="${field.value}" />
                          <#elseif field.fieldType == 'Double'>
                            <input type="text" size="20" name="${field.name}" value="${field.value}" />
                          <#elseif field.fieldType == 'Float'>
                            <input type="text" size="20" name="${field.name}" value="${field.value}" />
                          <#elseif field.fieldType == 'StringOneRow'>
                            <input type="text" size="${field.stringLength}" maxlength="${field.stringLength}" name="${field.name}" value="${field.value}" />
                          <#elseif field.fieldType == 'String'>
                            <input type="text" size="80" maxlength="${field.stringLength}" name="${field.name}" value="${field.value}" />
                          <#elseif field.fieldType == 'Textarea'>
                            <textarea cols="60" rows="3" maxlength="${field.stringLength}" name="${field.name}">${field.value}</textarea>
                          <#else>
                            <input type="text" size="20" name="${field.name}" value="${field.value}" />
                          </#if>
                        </td>
                      </tr>
                      <#assign alt_row = !alt_row>
                    </#list>
                  </#if>
                <#else>
                  <@alert type="error">${uiLabelMap.WebtoolsEntityCretePermissionError} ${entityName} ${plainTableName}</@alert>
                  <#assign showFields = false>
                </#if>
              </#if>
              <#if showFields>
                <#if newFieldNoPkList?has_content>
                  <#assign alt_row = false>
                  <#list newFieldNoPkList as field>
                    <tr<@dataRowClassStr alt=alt_row />>
                      <td class=>${field.name}</td>
                      <td>
                        <#if field.fieldType == 'DateTime'>
                          DateTime(YYYY-MM-DD HH:mm:SS.sss):<@htmlTemplate.renderDateTimeField name="${field.name}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${field.value}" size="25" maxlength="30" id="${field.name}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                        <#elseif field.fieldType == 'Date'>
                          Date(YYYY-MM-DD):<@htmlTemplate.renderDateTimeField name="${field.name}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${field.value}" size="25" maxlength="30" id="${field.name}" dateType="date" shortDateInput=true timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                        <#elseif field.fieldType == 'Time'>
                          Time(HH:mm:SS.sss):<input type="text" size="6" maxlength="10" name="${field.name}" value="${field.value}" />
                        <#elseif field.fieldType == 'Integer'>
                          <input type="text" size="20" name="${field.name}" value="${field.value}" />
                        <#elseif field.fieldType == 'Long'>
                          <input type="text" size="20" name="${field.name}" value="${field.value}" />
                        <#elseif field.fieldType == 'Double'>
                          <input type="text" size="20" name="${field.name}" value="${field.value}" />
                        <#elseif field.fieldType == 'Float'>
                          <input type="text" size="20" name="${field.name}" value="${field.value}" />
                        <#elseif field.fieldType == 'StringOneRow'>
                          <input type="text" size="${field.stringLength}" maxlength="${field.stringLength}" name="${field.name}" value="${field.value}" />
                        <#elseif field.fieldType == 'String'>
                          <input type="text" size="80" maxlength="${field.stringLength}" name="${field.name}" value="${field.value}" />
                        <#elseif field.fieldType == 'Textarea'>
                          <textarea cols="60" rows="3" maxlength="${field.stringLength}" name="${field.name}">${field.value}</textarea>
                        <#else>
                          <input type="text" size="20" name="${field.name}" value="${field.value}" />
                        </#if>
                      </td>
                    </tr>
                    <#assign alt_row = !alt_row>
                  </#list>
                  <#if value?has_content>
                    <#assign button = "${uiLabelMap.CommonUpdate}">
                  <#else>
                    <#assign button = "${uiLabelMap.CommonCreate}">
                  </#if>
                  <tr<@dataRowClassStr alt=alt_row />>
                    <td>&nbsp;</td>
                    <td>
                      <input type="submit" name="Update" value="${button}" />
                      <a href="<@ofbizUrl>ViewGeneric?${curFindString}</@ofbizUrl>" class="smallSubmit">${uiLabelMap.CommonCancel}</a>
                    </td>
                  </tr>
                </#if>
              </#if>
            </table>
          </form>
        </@cell>
        </@row>
      <#else>
          <@row>
            <@cell>
              <h3 data-magellan-destination="current-view" id="current-view">${uiLabelMap.WebtoolsEntityCurrentValue}</h3>
              <#if value?has_content>
                <#assign alt_row = false>
                <table class="${styles.grid_large!}12" cellspacing="0">
                  <thead>
                  <tr>
                    <th class="${styles.grid_large!}3">${uiLabelMap.WebtoolsFieldName}</th>
                    <th class="${styles.grid_large!}9">${uiLabelMap.CommonValue}</th>
                  </tr>
                  </thead>
                  <#list fields as field>
                    <tr<@dataRowClassStr alt=alt_row />>
                      <td>${field.name}</td>
                      <td>${field.value}</td>
                    </tr>
                    <#assign alt_row = !alt_row>
                  </#list>
                </table>
              <#else>
                ${uiLabelMap.WebtoolsSpecifiedEntity1} ${entityName} ${uiLabelMap.WebtoolsSpecifiedEntity2}.
      </#if>
            </@cell>
        </@row>
    </#if>
      
    <#if relationFieldList?has_content>
    <@row>
        <@cell>
            <h3 data-magellan-destination="related-view" id="related-view">${uiLabelMap.WebtoolsRelatedEntity}</h3>
            <@grid>  
      <#list relationFieldList as relation>
                    <li>
                            <@pul title="${relation.title}${relation.relatedTable}">
                            <@pli type="description">${relation.type}</@pli>
                <#if relation.valueRelated?has_content>
                              <@pli><a href="<@ofbizUrl>ViewGeneric?${relation.encodeRelatedEntityFindString}</@ofbizUrl>">${uiLabelMap.CommonView}</a></@pli>
                </#if>
                <#if hasAllCreate || relCreate>
                              <@pli><a href="<@ofbizUrl>ViewGeneric?${relation.encodeRelatedEntityFindString}&amp;enableEdit=true</@ofbizUrl>">${uiLabelMap.CommonCreate}</a></@pli>
                </#if>

            <#if relation.valueRelated?has_content>
                            <@pli>
                              <@modal id="rel_${relation.relatedTable}" label="${uiLabelMap.CommonValues}">                                
                                  <table cellspacing="0" class="${styles.grid_large!}12">
                                    <thead>
                                        <tr>
                                            <th class="${styles.grid_large!}3">${uiLabelMap.WebtoolsFieldName}</th>
                                            <th class="${styles.grid_large!}9">${uiLabelMap.CommonValue}</th>
                                        </tr>
                                    </thead>
                <#assign alt_row = false>
                <tr<@dataRowClassStr alt=alt_row />>
                                      <td class="">${uiLabelMap.WebtoolsPk}</td>
                  <td>${relation.valueRelatedPk}</td>
                </tr>
                <#list relation.relatedFieldsList as relatedField>
                  <tr<@dataRowClassStr alt=alt_row />>
                                        <td class="">${relatedField.name}</td>
                    <td>${relatedField.value}</td>
                  </tr>
                  <#assign alt_row = !alt_row>
                </#list>
              </table>
                              </@modal>
                          </@pli>
            
            <#else>
              <#if "one" = relation.type>
                                <#--
                                    <@pli>
              <b>${uiLabelMap.WebtoolsNoValueFoundFor}</b> ${relation.title}${relation.relatedTable}.
                                   </@pli>
                               -->
                               <#else>
                               <@pli>
                                <a href="<@ofbizUrl>FindGeneric?${relation.encodeRelatedEntityFindString}&amp;find=true</@ofbizUrl>">${uiLabelMap.CommonFind}</a>
                               </@pli>                       
              </#if>
            </#if>
                        </@pul>
                    </li>
      </#list>

                </@grid>
            </@cell>
      </@row> 
    </#if>
</@section>
