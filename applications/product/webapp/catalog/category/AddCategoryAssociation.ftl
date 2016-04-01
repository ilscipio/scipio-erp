<#-- TODO: License -->

<#if productCategoryAssociationMode?has_content>
    <@section>
        <form method="post" action="<@ofbizUrl>addProductCategoryToCategory</@ofbizUrl>" name="AddCategory">        
            <input type="hidden" name="originalProductCategoryId" value="${originalProductCategoryId!}" />
            <@row>
                <@cell columns=12>
                    <#if productCategoryAssociationMode == "parent">
                        <input type="hidden" name="parentProductCategoryId" value="${originalProductCategoryId!}" />
                        <@field type="lookup" id="productCategoryId" name="productCategoryId" label=uiLabelMap.ProductCategory required=true formName="AddCategory" fieldFormName="LookupProductCategory" />
                    <#elseif productCategoryAssociationMode == "child">
                        <input type="hidden" name="productCategoryId" value="${originalProductCategoryId!}" />
                        <@field type="lookup" id="parentProductCategoryId" name="parentProductCategoryId" label=uiLabelMap.ProductCategory required=true formName="AddCategory" fieldFormName="LookupProductCategory" />
                    </#if>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate"  size="25" maxlength="30" id="fromDateAdd1_${productCategoryAssociationMode}"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate"  size="25" maxlength="30" id="fromDateAdd2_${productCategoryAssociationMode}"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequenceNum  size=20 maxlength=40 />
                </@cell>
            </@row>
            <@field type="submit" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_add!}" />
        </form>
    </@section>
</#if>