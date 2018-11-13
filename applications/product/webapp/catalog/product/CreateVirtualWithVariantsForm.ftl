<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@section title=uiLabelMap.ProductQuickCreateVirtualFromVariants>
    <form action="<@ofbizUrl>quickCreateVirtualWithVariants</@ofbizUrl>" method="post" name="quickCreateVirtualWithVariants">
        <@field type="textarea" label=uiLabelMap.ProductVariantProductIds name="variantProductIdsBag" rows="6" cols="20"></@field>
        
        <@field type="select" label="Hazmat" name="productFeatureIdOne">
                <option value="">- ${uiLabelMap.CommonNone} -</option>
                <#list hazmatFeatures as hazmatFeature>
                    <option value="${hazmatFeature.productFeatureId}">${hazmatFeature.description}</option>
                </#list>
        
        </@field>  
        
        <@field type="submit" text=uiLabelMap.ProductCreateVirtualProduct class="+${styles.link_run_sys!} ${styles.action_add!}"/> 
    </form>
</@section>