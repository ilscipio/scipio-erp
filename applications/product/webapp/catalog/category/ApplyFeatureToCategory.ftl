<#-- TODO: License -->

    <@section title="${uiLabelMap.ProductApplyFeatureGroupToCategory}">
        <form method="post" action="<@ofbizUrl>createProductFeatureCategoryAppl</@ofbizUrl>" name="addNewCategoryForm">
          <@fields type="default-nolabels">
            <input type="hidden" name="productCategoryId" value="${productCategoryId!}" />
            <@row>
                <@cell columns=6>
                    <@field type="select" label="${uiLabelMap.ProductFeature}" name="productFeatureCategoryId">
                            <#list productFeatureCategories as productFeatureCategory>
                                <option value="${(productFeatureCategory.productFeatureCategoryId)!}">${(productFeatureCategory.description)!} [${(productFeatureCategory.productFeatureCategoryId)!}]</option>
                            </#list>
                    </@field>
                </@cell>
                <@cell columns=6>
                    <@field type="generic" label="${uiLabelMap.CommonFrom}">
                        <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" value="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="fromDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                    </@field>
                </@cell>
            </@row>
            <@field type="submit" text="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
          </@fields>
        </form>
    </@section>