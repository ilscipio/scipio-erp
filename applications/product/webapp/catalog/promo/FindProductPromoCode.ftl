<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

                     
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("FindProductPromoCode") text=uiLabelMap.CommonShowAll class="+${styles.action_nav!} ${styles.action_find!}"/>

        <@menuitem type="generic">
            <@modal id="modal_new_importcodeset_${productPromoId!}" label=uiLabelMap.ProductPromotionUploadSetOfPromotionCodes anchorClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                <@heading>${uiLabelMap.ProductPromotionUploadSetOfPromotionCodes}</@heading>
                 <form method="post" action="<@ofbizUrl>createBulkProductPromoCode</@ofbizUrl>" enctype="multipart/form-data">
                    <input type="hidden" name="productPromoId" value="${productPromoId!}"/>
                    <span>${uiLabelMap.ProductPromoUserEntered}:</span>
                        <select name="userEntered">
                            <option value="Y">${uiLabelMap.CommonY}</option>
                            <option value="N">${uiLabelMap.CommonN}</option>
                        </select>
                    <span>${uiLabelMap.ProductPromotionReqEmailOrParty}:</span>
                        <select name="requireEmailOrParty">
                            <option value="N">${uiLabelMap.CommonN}</option>
                            <option value="Y">${uiLabelMap.CommonY}</option>
                        </select>
                    <span>${uiLabelMap.ProductPromotionUseLimits}:
                    ${uiLabelMap.ProductPromotionPerCode}</span><input type="text" size="5" name="useLimitPerCode" />
                    <span>${uiLabelMap.ProductPromotionPerCustomer}</span><input type="text" size="5" name="useLimitPerCustomer" />
                    <div>
                      <input type="file" size="40" name="uploadedFile" />
                      <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_import!}" />
                    </div>
                </form>
             </@modal>
        </@menuitem>

        <@menuitem type="generic">
            <@modal id="modal_new_promocodes_${productPromoId!}" label=uiLabelMap.ProductPromotionAddSetOfPromotionCodes anchorClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                <@heading>${uiLabelMap.ProductPromotionAddSetOfPromotionCodes}</@heading>
                <form method="post" action="<@ofbizUrl>createProductPromoCodeSet</@ofbizUrl>">
                    <input type="hidden" name="productPromoId" value="${productPromoId!}"/>
                    <span>${uiLabelMap.CommonQuantity}:</span><input type="text" size="5" name="quantity" />
                    <span>${uiLabelMap.ProductPromoCodeLength}:</span><input type="text" size="12" name="codeLength" />
                    <span>${uiLabelMap.ProductPromoCodeLayout}:</span>
                        <select name="promoCodeLayout">
                            <option value="smart">${uiLabelMap.ProductPromoLayoutSmart}</option>
                            <option value="normal">${uiLabelMap.ProductPromoLayoutNormal}</option>
                            <option value="sequence">${uiLabelMap.ProductPromoLayoutSeqNum}</option>
                        </select>
                    <span class="tooltip">${uiLabelMap.ProductPromoCodeLayoutTooltip}</span>
                    <br />
                    <span>${uiLabelMap.ProductPromoUserEntered}:</span>
                        <select name="userEntered">
                            <option value="Y">${uiLabelMap.CommonY}</option>
                            <option value="N">${uiLabelMap.CommonN}</option>
                        </select>
                    <span>${uiLabelMap.ProductPromotionReqEmailOrParty}:</span>
                        <select name="requireEmailOrParty">
                            <option value="N">${uiLabelMap.CommonN}</option>
                            <option value="Y">${uiLabelMap.CommonY}</option>
                        </select>
                    <span>${uiLabelMap.ProductPromotionUseLimits}:
                    ${uiLabelMap.ProductPromotionPerCode}</span><input type="text" size="5" name="useLimitPerCode" />
                    <span>${uiLabelMap.ProductPromotionPerCustomer}</span><input type="text" size="5" name="useLimitPerCustomer" />
                    <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                </form>
                 
             </@modal>
        </@menuitem>

        <#if productPromoId?has_content>
            <#assign targetUrl = makeOfbizUrl("EditProductPromoCode?productPromoId=${productPromoId}")>
        <#else>
            <#assign targetUrl = makeOfbizUrl("EditProductPromoCode")>
        </#if>
      <#if (isCreateProductPromoCode!false) != true>
        <@menuitem type="link" href=targetUrl text=uiLabelMap.ProductNewPromotionCode class="+${styles.action_nav!} ${styles.action_add!}"/>
      </#if>
    </@menu>
</#macro> 
<#--
<@section menuContent=menuContent>
</@section>
-->
    <@menuContent menuArgs={"type":"section"} />

    <@alert type="info">${uiLabelMap.ProductPromoCodeAddInstructions}</@alert>
