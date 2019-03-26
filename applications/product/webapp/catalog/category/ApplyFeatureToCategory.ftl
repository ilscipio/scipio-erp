<#-- TODO: License -->

    <@section title=uiLabelMap.ProductApplyFeatureGroupToCategory>
        <form method="post" action="<@pageUrl>createProductFeatureCategoryAppl</@pageUrl>" name="addNewCategoryForm">
          <@fields type="default-nolabelarea">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <@row>
                <@cell columns=6>
                    <@field type="select" label=uiLabelMap.ProductFeature name="productFeatureCategoryId">
                            <#list productFeatureCategories as productFeatureCategory>
                                <option value="${(productFeatureCategory.productFeatureCategoryId)!}">${(productFeatureCategory.description)!} [${(productFeatureCategory.productFeatureCategoryId)!}]</option>
                            </#list>
                    </@field>
                </@cell>
                <@cell columns=6>
                    <@field type="datetime" label=uiLabelMap.CommonFrom name="fromDate" value="" size="25" maxlength="30" id="fromDate2"/>
                </@cell>
            </@row>
            <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
          </@fields>
        </form>
    </@section>