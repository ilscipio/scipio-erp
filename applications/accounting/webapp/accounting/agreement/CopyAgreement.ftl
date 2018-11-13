<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#if agreement??>
<@section title=uiLabelMap.PageTitleCopyAgreement>
    <form action="<@ofbizUrl>copyAgreement</@ofbizUrl>" method="post">
        <input type="hidden" name="agreementId" value="${agreementId}"/>    
        <@field type="checkbox" label=uiLabelMap.AccountingAgreementTerms name="copyAgreementTerms" value="Y" checked=true />
        <@field type="checkbox" label=uiLabelMap.ProductProducts name="copyAgreementProducts" value="Y" checked=true />
        <@field type="checkbox" label=uiLabelMap.Party name="copyAgreementParties" value="Y" checked=true />
        <@field type="checkbox" label=uiLabelMap.ProductFacilities name="copyAgreementFacilities" value="Y" checked=true />
        
        <@field type="submit" text=uiLabelMap.CommonCopy class="+${styles.link_run_sys!} ${styles.action_copy!}" />
    </form>
</@section>
</#if>