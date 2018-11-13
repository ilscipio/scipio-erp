<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section title=uiLabelMap.ProductAlternateKeyWordThesaurus>
        <form method="post" action="<@ofbizUrl>createKeywordThesaurus</@ofbizUrl>">
            <div>
                <span>${uiLabelMap.ProductKeyword}</span><input type="text" name="enteredKeyword" size="10"/>
                <span>${uiLabelMap.ProductAlternate}</span><input type="text" name="alternateKeyword" size="10"/>
                <span>${uiLabelMap.ProductRelationship}</span><select name="relationshipEnumId">
                <#list relationshipEnums as relationshipEnum>
                <option value="${relationshipEnum.enumId}">${relationshipEnum.get("description",locale)}</option>
                </#list>
                </select>
                <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
            </div>
        </form>
        <div>
            <#list letterList as letter>
              <#if letter == firstLetter><#assign highlight=true><#else><#assign highlight=false></#if>
              <a href="<@ofbizUrl>editKeywordThesaurus?firstLetter=${letter}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}"><#if highlight>[</#if>${letter}<#if highlight>]</#if></a>
            </#list>
        </div>
        
      <#assign lastkeyword = "">
      <#if keywordThesauruses?has_content>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <#-- SCIPIO: TODO: rewrite this somehow without need for open/close -->
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
                          <input type="submit" value="${uiLabelMap.CommonDeleteAll}" class="${styles.link_run_sys!} ${styles.action_remove!}" />
                        </form>
                      </div>
                      <div>
                      <form method="post" action="<@ofbizUrl>createKeywordThesaurus</@ofbizUrl>">
                        <input type="hidden" name="enteredKeyword" value="${keyword.enteredKeyword}" />
                        <span>${uiLabelMap.ProductAlternate}</span><input type="text" name="alternateKeyword" size="10" />
                        <span>${uiLabelMap.ProductRelationship}</span><select name="relationshipEnumId"><#list relationshipEnums as relationshipEnum><option value="${relationshipEnum.enumId}">${relationshipEnum.get("description",locale)}</option></#list></select>
                        <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                      </form>
                      </div>
                  <@td close=true open=false />
                  <@td open=true close=false />
              </#if>
              <div>
                <form method="post" action="<@ofbizUrl>deleteKeywordThesaurus</@ofbizUrl>" name="deleteKeywordThesaurus">
                  <input type="hidden" name="enteredKeyword" value="${keyword.enteredKeyword}" />
                  <input type="hidden" name="alternateKeyword" value="${keyword.alternateKeyword}" />
                  <input type="submit" value="X" class="${styles.link_run_sys!} ${styles.action_remove!}" />
                </form>
                ${keyword.alternateKeyword}&nbsp;(${uiLabelMap.ProductRelationship}:${(relationship.get("description",locale))?default(keyword.relationshipEnumId!)})
              </div>
            </#list>
              <@td close=true open=false />
            <@tr close=true open=false />
        </@table>
      </#if>
</@section>