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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<#if productPromoCode??>
    <@section title=uiLabelMap.ProductPromoCodeEmails>
            <#list productPromoCodeEmails as productPromoCodeEmail>
              <div>
                <form name="deleteProductPromoCodeEmail_${productPromoCodeEmail_index}" method="post" action="<@pageUrl>deleteProductPromoCodeEmail</@pageUrl>">
                  <input type="hidden" name="productPromoCodeId" value="${productPromoCodeEmail.productPromoCodeId}"/>                
                  <input type="hidden" name="emailAddress" value="${productPromoCodeEmail.emailAddress}"/>                
                  <input type="hidden" name="productPromoId" value="${productPromoId}"/>                
                  <a href="javascript:document.deleteProductPromoCodeEmail_${productPromoCodeEmail_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>&nbsp;${productPromoCodeEmail.emailAddress}
                </form>
              </div>                
            </#list>
            <div>
                <form method="post" action="<@pageUrl>createProductPromoCodeEmail</@pageUrl>">
                    <input type="hidden" name="productPromoCodeId" value="${productPromoCodeId!}"/>
                    <input type="hidden" name="productPromoId" value="${productPromoId}"/>
                    <span>${uiLabelMap.ProductAddEmail}:</span><input type="text" size="40" name="emailAddress" />
                    <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                </form>
                <#if (productPromoCode.requireEmailOrParty!) == "N">
                    <div class="tooltip">${uiLabelMap.ProductNoteRequireEmailParty}</div>
                </#if>
                <form method="post" action="<@pageUrl>createBulkProductPromoCodeEmail?productPromoCodeId=${productPromoCodeId!}</@pageUrl>" enctype="multipart/form-data">
                    <input type="hidden" name="productPromoCodeId" value="${productPromoCodeId!}"/>
                    <input type="hidden" name="productPromoId" value="${productPromoId}"/>
                    <input type="file" size="40" name="uploadedFile" />
                    <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_import!}" />
                </form>
            </div>
    </@section>
    
    <@section title=uiLabelMap.ProductPromoCodeParties>
            <#list productPromoCodeParties as productPromoCodeParty>
                <div><a href="<@pageUrl>deleteProductPromoCodeParty?productPromoCodeId=${productPromoCodeParty.productPromoCodeId}&amp;partyId=${productPromoCodeParty.partyId}&amp;productPromoId=${productPromoId}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">X</a>&nbsp;${productPromoCodeParty.partyId}</div>
            </#list>
            <div>
                <form method="post" action="<@pageUrl>createProductPromoCodeParty</@pageUrl>">
                    <input type="hidden" name="productPromoCodeId" value="${productPromoCodeId!}"/>
                    <input type="hidden" name="productPromoId" value="${productPromoId}"/>
                    <span>${uiLabelMap.ProductAddPartyId}:</span><input type="text" size="10" name="partyId" />
                    <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                </form>
            </div>
    </@section>
</#if>
