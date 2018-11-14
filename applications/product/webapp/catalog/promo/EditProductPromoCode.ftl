<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if productPromoCode??>
    <@section title=uiLabelMap.ProductPromoCodeEmails>
            <#list productPromoCodeEmails as productPromoCodeEmail>
              <div>
                <form name="deleteProductPromoCodeEmail_${productPromoCodeEmail_index}" method="post" action="<@ofbizUrl>deleteProductPromoCodeEmail</@ofbizUrl>">
                  <input type="hidden" name="productPromoCodeId" value="${productPromoCodeEmail.productPromoCodeId}"/>                
                  <input type="hidden" name="emailAddress" value="${productPromoCodeEmail.emailAddress}"/>                
                  <input type="hidden" name="productPromoId" value="${productPromoId}"/>                
                  <a href="javascript:document.deleteProductPromoCodeEmail_${productPromoCodeEmail_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>&nbsp;${productPromoCodeEmail.emailAddress}
                </form>
              </div>                
            </#list>
            <div>
                <form method="post" action="<@ofbizUrl>createProductPromoCodeEmail</@ofbizUrl>">
                    <input type="hidden" name="productPromoCodeId" value="${productPromoCodeId!}"/>
                    <input type="hidden" name="productPromoId" value="${productPromoId}"/>
                    <span>${uiLabelMap.ProductAddEmail}:</span><input type="text" size="40" name="emailAddress" />
                    <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                </form>
                <#if (productPromoCode.requireEmailOrParty!) == "N">
                    <div class="tooltip">${uiLabelMap.ProductNoteRequireEmailParty}</div>
                </#if>
                <form method="post" action="<@ofbizUrl>createBulkProductPromoCodeEmail?productPromoCodeId=${productPromoCodeId!}</@ofbizUrl>" enctype="multipart/form-data">
                    <input type="hidden" name="productPromoCodeId" value="${productPromoCodeId!}"/>
                    <input type="hidden" name="productPromoId" value="${productPromoId}"/>
                    <input type="file" size="40" name="uploadedFile" />
                    <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_import!}" />
                </form>
            </div>
    </@section>
    
    <@section title=uiLabelMap.ProductPromoCodeParties>
            <#list productPromoCodeParties as productPromoCodeParty>
                <div><a href="<@ofbizUrl>deleteProductPromoCodeParty?productPromoCodeId=${productPromoCodeParty.productPromoCodeId}&amp;partyId=${productPromoCodeParty.partyId}&amp;productPromoId=${productPromoId}</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">X</a>&nbsp;${productPromoCodeParty.partyId}</div>
            </#list>
            <div>
                <form method="post" action="<@ofbizUrl>createProductPromoCodeParty</@ofbizUrl>">
                    <input type="hidden" name="productPromoCodeId" value="${productPromoCodeId!}"/>
                    <input type="hidden" name="productPromoId" value="${productPromoId}"/>
                    <span>${uiLabelMap.ProductAddPartyId}:</span><input type="text" size="10" name="partyId" />
                    <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                </form>
            </div>
    </@section>
</#if>
