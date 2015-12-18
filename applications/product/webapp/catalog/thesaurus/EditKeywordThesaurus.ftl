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
<@section title="${uiLabelMap.ProductAlternateKeyWordThesaurus}">
        <form method="post" action="<@ofbizUrl>createKeywordThesaurus</@ofbizUrl>">
            <div>
                <span>${uiLabelMap.ProductKeyword}</span><input type="text" name="enteredKeyword" size="10"/>
                <span>${uiLabelMap.ProductAlternate}</span><input type="text" name="alternateKeyword" size="10"/>
                <span>${uiLabelMap.ProductRelationship}</span><select name="relationshipEnumId">
                <#list relationshipEnums as relationshipEnum>
                <option value="${relationshipEnum.enumId}">${relationshipEnum.get("description",locale)}</option>
                </#list>
                </select>
                <input type="submit" value="${uiLabelMap.CommonAdd}"/>
            </div>
        </form>
        <div>
            <#list letterList as letter>
              <#if letter == firstLetter><#assign highlight=true><#else><#assign highlight=false></#if>
              <a href="<@ofbizUrl>editKeywordThesaurus?firstLetter=${letter}</@ofbizUrl>" class="${styles.link_action!}"><#if highlight>[</#if>${letter}<#if highlight>]</#if></a>
            </#list>
        </div>
        
      <#assign lastkeyword = "">
      <#if keywordThesauruses?has_content>
        <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
            <#-- Cato: TODO: rewrite this somehow without need for open/close -->
            <#list keywordThesauruses as keyword>
              <#assign relationship = keyword.getRelatedOne("RelationshipEnumeration", true)>
              <#if keyword.enteredKeyword == lastkeyword><#assign sameRow=true><#else><#assign lastkeyword=keyword.enteredKeyword><#assign sameRow=false></#if>
              <#if sameRow == false>
                <#if (keyword_index > 0)>
                  <@td close=true open=false />
                <@tr close=true open=false />
                </#if>
                <@tr valign="middle" open=true close=false />
                  <@td open=true close=false />
                      <div>
                        ${keyword.enteredKeyword}
                        <form method="post" action="<@ofbizUrl>deleteKeywordThesaurus</@ofbizUrl>" name="deleteKeywordThesaurus">
                          <input type="hidden" name="enteredKeyword" value="${keyword.enteredKeyword}" />
                          <input type="hidden" name="alternateKeyword" value="${keyword.alternateKeyword}" />
                          <input type="submit" value="${uiLabelMap.CommonDeleteAll}" />
                        </form>
                      </div>
                      <div>
                      <form method="post" action="<@ofbizUrl>createKeywordThesaurus</@ofbizUrl>">
                        <input type="hidden" name="enteredKeyword" value="${keyword.enteredKeyword}" />
                        <span>${uiLabelMap.ProductAlternate}</span><input type="text" name="alternateKeyword" size="10" />
                        <span>${uiLabelMap.ProductRelationship}</span><select name="relationshipEnumId"><#list relationshipEnums as relationshipEnum><option value="${relationshipEnum.enumId}">${relationshipEnum.get("description",locale)}</option></#list></select>
                        <input type="submit" value="${uiLabelMap.CommonAdd}" />
                      </form>
                      </div>
                  <@td close=true open=false />
                  <@td open=true close=false />
              </#if>
              <div>
                <form method="post" action="<@ofbizUrl>deleteKeywordThesaurus</@ofbizUrl>" name="deleteKeywordThesaurus">
                  <input type="hidden" name="enteredKeyword" value="${keyword.enteredKeyword}" />
                  <input type="hidden" name="alternateKeyword" value="${keyword.alternateKeyword}" />
                  <input type="submit" value="X" />
                </form>
                ${keyword.alternateKeyword}&nbsp;(${uiLabelMap.ProductRelationship}:${(relationship.get("description",locale))?default(keyword.relationshipEnumId!)})
              </div>
            </#list>
              <@td close=true open=false />
            <@tr close=true open=false />
        </@table>
      </#if>
</@section>