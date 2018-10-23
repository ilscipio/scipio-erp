<@section title=sectionTitle>
    <#assign actionUri = "createProdCatalog" />
    <#assign actionLabel = uiLabelMap.CommonCreate/>
    <#if prodCatalog?has_content>
        <#assign actionUri = "updateProdCatalog" />
        <#assign actionLabel = uiLabelMap.CommonUpdate />
    </#if>
        <form method="post" action="<@ofbizUrl>${actionUri}</@ofbizUrl>" name="addProductCategoryMemberForm">        
            <#if !prodCatalog?has_content>
              <input type="hidden" name="isCreate" value="true" />
            </#if>

            <@row>
                <@cell columns=12>
                    <#if prodCatalog?has_content>
                        <@field type="display" name="prodCatalogId" label=uiLabelMap.CommonId tooltip=uiLabelMap.ProductNotModificationRecreatingProductCatalog>
                            <strong>${(prodCatalog.prodCatalogId)}</strong>
                        </@field>
                        <input type="hidden" name="prodCatalogId" value="${prodCatalogId}"/>      
                    <#else>                        
                        <#if parameters.prodCatalogId?has_content>
                            <#assign tooltip>${rawLabel('ProductCouldNotFindProductCatalogWithId')} [${rawString(prodCatalogId)}]</#assign>
                        </#if>
                        <@field type="input" name="prodCatalogId" label=uiLabelMap.CommonId tooltip=((tooltip)!) />
                    </#if>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="catalogName" label=uiLabelMap.CommonName value=((prodCatalog.catalogName)!) size=20 maxlength=40 />
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="styleSheet" label=uiLabelMap.ProductStyleSheet value=((prodCatalog.styleSheet)!) size=20 maxlength=250 />                      
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="headerLogo" label=uiLabelMap.ProductHeaderLogo value=((prodCatalog.headerLogo)!) size=20 maxlength=250 />
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="contentPathPrefix" label=uiLabelMap.ProductContentPathPrefix value=((prodCatalog.contentPathPrefix)!) tooltip=uiLabelMap.ProductPrependedImageContentPaths size=20 maxlength=250 />                      
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="templatePathPrefix" label=uiLabelMap.ProductTemplatePathPrefix value=((prodCatalog.templatePathPrefix)!) tooltip=uiLabelMap.ProductPrependedTemplatePaths size=20 maxlength=250 />
                </@cell>
            </@row>

            <@row>
                <@cell columns=12>
                    <@field type="checkbox" name="useQuickAdd" label=uiLabelMap.ProductUseQuickAdd value="Y" checked=(prodCatalog?has_content && (prodCatalog.useQuickAdd!) == "Y") />
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="checkbox" name="viewAllowPermReqd" label=uiLabelMap.ProductCategoryViewAllowPermReqd value="Y"  checked=(prodCatalog?has_content && prodCatalog.viewAllowPermReqd?has_content && prodCatalog.viewAllowPermReqd == "Y")  />
                </@cell>
            </@row>

            <@row>
                <@cell columns=12>
                    <@field type="checkbox" name="purchaseAllowPermReqd" label=uiLabelMap.ProductCategoryPurchaseAllowPermReqd value="Y" checked=(prodCatalog?has_content && prodCatalog.purchaseAllowPermReqd?has_content && prodCatalog.purchaseAllowPermReqd == "Y") />
                </@cell>
            </@row>

            <@row>
                <@cell>
                    <@field type="submit" name="Update" text=actionLabel class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                </@cell>
            </@row>            
        </form>
    
</@section>