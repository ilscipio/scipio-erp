<#-- TODO: License -->

<@modal id="duplicateProductCategory">
<#if productCategoryId?has_content>
    <@section title=uiLabelMap.ProductDuplicateCategory>
        <form action="<@ofbizUrl>DuplicateProductCategory</@ofbizUrl>" method="post">
            <input type="hidden" name="oldProductCategoryId" value="${productCategoryId}"/>
            <@field type="input" label=uiLabelMap.ProductDuplicateProductCategorySelected size="20" maxlength="20" name="productCategoryId"/>
                     
            <@field type="generic" label=uiLabelMap.CommonDuplicate>
                <@field type="checkbox" name="duplicateContent" value="Y" checked=true  label=uiLabelMap.ProductCategoryContent/>
                <@field type="checkbox" name="duplicateParentRollup" value="Y" checked=true  label=uiLabelMap.ProductCategoryRollupParentCategories/>
                <@field type="checkbox" name="duplicateChildRollup" value="Y"  label=uiLabelMap.ProductCategoryRollupChildCategories/>
                <@field type="checkbox" name="duplicateMembers" value="Y" checked=true  label=uiLabelMap.ProductProducts/>
                <@field type="checkbox" name="duplicateCatalogs" value="Y" checked=true  label=uiLabelMap.ProductCatalogs/>
                <@field type="checkbox" name="duplicateFeatures" value="Y" checked=true  label=uiLabelMap.ProductFeatures/>
                <@field type="checkbox" name="duplicateRoles" value="Y" checked=true  label=uiLabelMap.PartyParties/>
                <@field type="checkbox" name="duplicateAttributes" value="Y" checked=true  label=uiLabelMap.ProductAttributes/>
            </@field> 
      
            <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_copy!}" text=uiLabelMap.CommonSubmit/>  
        </form>
    </@section>
</#if>
</@modal>
<@script>
    $(document).ready(function() {
        $('#modal_duplicateProductCategory').foundation('reveal','open');
    });
</@script>