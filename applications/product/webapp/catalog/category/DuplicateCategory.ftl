<#-- TODO: License -->

<#if productCategoryId?has_content>
    <@section title="${uiLabelMap.ProductDuplicateProductCategory}">
            <form action="<@ofbizUrl>DuplicateProductCategory</@ofbizUrl>" method="post">
                <input type="hidden" name="oldProductCategoryId" value="${productCategoryId}"/>
                <@field type="generic" label="${uiLabelMap.ProductDuplicateProductCategorySelected}">
                  <input type="text" size="20" maxlength="20" name="productCategoryId"/>
                </@field>
                         
                <@field type="generic" label="${uiLabelMap.CommonDuplicate}">
                    ${uiLabelMap.ProductCategoryContent}&nbsp;<input type="checkbox" name="duplicateContent" value="Y" checked="checked" />
                    ${uiLabelMap.ProductCategoryRollupParentCategories}&nbsp;<input type="checkbox" name="duplicateParentRollup" value="Y" checked="checked" />
                    ${uiLabelMap.ProductCategoryRollupChildCategories}&nbsp;<input type="checkbox" name="duplicateChildRollup" value="Y" />
                    ${uiLabelMap.ProductProducts}&nbsp;<input type="checkbox" name="duplicateMembers" value="Y" checked="checked" />
                    ${uiLabelMap.ProductCatalogs}&nbsp;<input type="checkbox" name="duplicateCatalogs" value="Y" checked="checked" />
                    ${uiLabelMap.ProductFeatures}&nbsp;<input type="checkbox" name="duplicateFeatures" value="Y" checked="checked" />
                    ${uiLabelMap.PartyParties}&nbsp;<input type="checkbox" name="duplicateRoles" value="Y" checked="checked" />
                    ${uiLabelMap.ProductAttributes}&nbsp;<input type="checkbox" name="duplicateAttributes" value="Y" checked="checked" />
                </@field> 
          
                <@field type="submit" class="${styles.link_run_sys!} ${styles.action_copy!}" text="${uiLabelMap.CommonGo}"/>  
            </form>
    </@section>
</#if>