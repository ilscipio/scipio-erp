<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if productPromoId?? && productPromo??>

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="generic">
            <@modal id="modal_new_rule_${productPromoId}" label=uiLabelMap.ProductAddPromoRule linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                <@heading>${uiLabelMap.ProductAddPromoRule}</@heading>
                 <form method="post" action="<@ofbizUrl>createProductPromoRule</@ofbizUrl>">
                    <@fields type="default-compact">
                      <input type="hidden" name="productPromoId" value="${productPromoId!}" />
                      <@field type="text" label=uiLabelMap.ProductName size="30" name="ruleName" required=true/>
                      <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}" />
                    </@fields>
                    </form>
             </@modal>
        </@menuitem>

        <@menuitem type="generic">
            <@modal id="modal_new_category_${productPromoId}" label=uiLabelMap.ProductAddPromoCategory linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                <@heading>${uiLabelMap.ProductAddPromoCategory}</@heading>
                <form method="post" action="<@ofbizUrl>createProductPromoCategory</@ofbizUrl>" name="createProductPromoCategoryPromotions">
                    <@fields type="default-compact">
                        <input type="hidden" name="productPromoId" value="${productPromoId}" />
                        <input type="hidden" name="productPromoRuleId" value="_NA_" />
                        <input type="hidden" name="productPromoActionSeqId" value="_NA_" />
                        <input type="hidden" name="productPromoCondSeqId" value="_NA_" />
                        <@field type="lookup" formName="createProductPromoCategoryPromotions" name="productCategoryId" id="productCategoryId_03" 
                            fieldFormName="LookupProductCategory" label=uiLabelMap.ProductCategoryId required=true/>
                        <@field type="select" name="productPromoApplEnumId" label="Product Promo Appl Enum Id"><#--FIXME: label-->
                          <#list productPromoApplEnums as productPromoApplEnum>
                            <option value="${productPromoApplEnum.enumId}">${productPromoApplEnum.get("description",locale)}</option>
                          </#list>
                        </@field>
                        <@field type="select" name="includeSubCategories" label=uiLabelMap.ProductIncludeAllSubCategories>
                          <option value="N">${uiLabelMap.CommonN}</option>
                          <option value="Y">${uiLabelMap.CommonY}</option>
                        </@field>
                        <@field type="text" label="${rawLabel('CommonAnd')} ${rawLabel('CommonGroup')}" required=true size="10" maxlength="20" name="andGroupId" value="_NA_"/>
                        <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}" />
                  </@fields>
               </form>
           </@modal>
        </@menuitem>

        <@menuitem type="generic">
            <@modal id="modal_new_product_${productPromoId}" label=uiLabelMap.ProductAddPromoProduct linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                <@heading>${uiLabelMap.ProductAddPromoProduct}</@heading>
                 <form method="post" name="createpromoproductform" action="<@ofbizUrl>createProductPromoProduct</@ofbizUrl>">
                      <@fields type="default-compact">
                        <input type="hidden" name="productPromoId" value="${productPromoId}" />
                        <input type="hidden" name="productPromoRuleId" value="_NA_" />
                        <input type="hidden" name="productPromoActionSeqId" value="_NA_" />
                        <input type="hidden" name="productPromoCondSeqId" value="_NA_" />
                        <@field type="lookup" required=true label=uiLabelMap.ProductProductId formName="createpromoproductform" name="productId" id="productId" fieldFormName="LookupProduct"/>
                        <@field type="select" name="productPromoApplEnumId" label="Product Promo Appl Enum Id">
                          <#list productPromoApplEnums as productPromoApplEnum>
                            <option value="${productPromoApplEnum.enumId}">${productPromoApplEnum.get("description",locale)}</option>
                          </#list>
                        </@field>
                        <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}" />
                      </@fields>
                  </form>
             </@modal>
        </@menuitem>

    </@menu>
</#macro>


<@section menuContent=menuContent>
  <#--  <@fields type="default-manual-widgetonly"> -->
  <#-- ======================= Rules ======================== -->
    <@table type="data-complex" autoAltRows=true>
      <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.ProductRule}</@th>
          <@th>${uiLabelMap.ProductConditions}</@th>
          <@th>${uiLabelMap.ProductActions}</@th>
          <@th></@th>
        </@tr>
      </@thead>
  <#assign ruleClass = "2">
  <#list productPromoRules as productPromoRule>
    <#assign productPromoConds = productPromoRule.getRelated("ProductPromoCond", null, null, false)>
    <#assign productPromoActions = productPromoRule.getRelated("ProductPromoAction", null, null, false)>
      <#if productPromoRule_index != 0>
        <@tr type="util"><@td colspan="4"><hr /></@td></@tr>
      </#if>
      <@tr valign="middle" class="row-level-one" alt=(ruleClass == "1")>
        <@td><strong><@modal id="modal_rule_${productPromoRule.productPromoRuleId}" label=productPromoRule.ruleName>
              <@heading>${uiLabelMap.ProductRuleName}</@heading>
              <form method="post" action="<@ofbizUrl>updateProductPromoRule</@ofbizUrl>">
                <input type="hidden" name="productPromoId" value="${(productPromoRule.productPromoId)!}" />
                <input type="hidden" name="productPromoRuleId" value="${(productPromoRule.productPromoRuleId)!}" />
                <@field type="input" size="30" name="ruleName" value=(productPromoRule.ruleName)! required=true />
                <@field type="submit" text=uiLabelMap.CommonUpdate class="+${styles.link_run_sys!} ${styles.action_update!}" />
              </form>
          </@modal></strong>
        </@td>


        <#-- Rule Conditions-->
        <@td>
            <ol>
            <#list productPromoConds as productPromoCond>
                <li>
                <#assign ruleLabel>
                    <#-- Rule -->
                    <#if (productPromoCond.inputParamEnumId)??>
                        <#assign inputParamEnum = productPromoCond.getRelatedOne("InputParamEnumeration", true) />
                         ${(inputParamEnum.get("description",locale))!}</#if>
                    <#if (productPromoCond.operatorEnumId)??>
                        <#assign operatorEnum = productPromoCond.getRelatedOne("OperatorEnumeration", true)/>
                        <#if operatorEnum??>${(operatorEnum.get("description",locale))!}<#else>${(productPromoCond.operatorEnumId)!}</#if>
                    </#if>
                    <#assign otherValue = productPromoCond.otherValue!>
                    <#if otherValue?has_content>
                          <#if otherValue?has_content && otherValue.contains("@")>
                            <#assign carrierShippingMethod = productPromoCond.otherValue!>
                          </#if>
                          <#if carrierShippingMethod?has_content>
                            <#assign carrierParty = carrierShippingMethod.substring(0, carrierShippingMethod.indexOf("@"))>
                            <#assign shippingMethodTypeId = carrierShippingMethod.substring(carrierShippingMethod.indexOf("@")+1)>
                            <#assign description = (delegator.findOne("ShipmentMethodType", {"shipmentMethodTypeId":shippingMethodTypeId}, false)).description>
                            ${carrierParty!}&nbsp;${description}
                          <#else>
                            ${otherValue!""}
                          </#if>
                    <#elseif productPromoCond.condValue?has_content>
                           ${(productPromoCond.condValue)!}
                    </#if>
                    <#-- Category -->
                    <#assign condProductPromoCategories = productPromoCond.getRelated("ProductPromoCategory", null, null, false)>
                    <#if condProductPromoCategories?has_content>
                        <ul>
                        <#list condProductPromoCategories as condProductPromoCategory>
                            <#if condProductPromoCategory.productCategoryId?has_content>
                                <li>
                                <#assign condProductCategory = condProductPromoCategory.getRelatedOne("ProductCategory", true)>
                                <#assign condApplEnumeration = condProductPromoCategory.getRelatedOne("ApplEnumeration", true)>
                                      ${(condApplEnumeration.get("description",locale))?default(condProductPromoCategory.productPromoApplEnumId!"")} ${uiLabelMap.CommonCategory} ${(condProductCategory.get("description",locale))?default(condProductPromoCategory.productCategoryId)!""}
                                      <#if condProductPromoCategory.includeSubCategories?has_content && condProductPromoCategory.includeSubCategories=="Y">
                                        ${uiLabelMap.CommonAnd} ${uiLabelMap.ProductSubCats}
                                      </#if>
                                      <#if condProductPromoCategory.andGroupId?has_content && condProductPromoCategory.andGroupId != "_NA_"><br/>${uiLabelMap.CommonAnd} ${uiLabelMap.CommonGroup} ${condProductPromoCategory.andGroupId}</#if>
                                </li>
                            </#if>
                        </#list>
                        </ul>
                    </#if>
                    <#-- Product -->
                    <#assign condProductPromoProducts = productPromoCond.getRelated("ProductPromoProduct", null, null, false)>
                    <#if condProductPromoProducts?has_content>
                        <ul>
                        <#list condProductPromoProducts as condProductPromoProduct>
                            <li>
                                <#assign condProduct = condProductPromoProduct.getRelatedOne("Product", true)!>
                                <#assign condApplEnumeration = condProductPromoProduct.getRelatedOne("ApplEnumeration", true)>
                                ${(condApplEnumeration.get("description",locale))?default(condProductPromoProduct.productPromoApplEnumId)} ${(condProduct.internalName)?default(condProductPromoProduct.productId!"")}
                            </li>          
                        </#list>
                        </ul>
                    </#if>
                </#assign>

                <@modal id="modal_rule_condition_${(productPromoCond.productPromoRuleId)!}_${(productPromoCond.productPromoCondSeqId)!}" label=wrapAsRaw(ruleLabel, 'htmlmarkup')>                                     
                    <#assign maxCondSeqId = 1>
                    <#assign condClass = "2">
                    <@section title=(uiLabelMap.ProductCondition + productPromoCond.productPromoCondSeqId!)!>
                        <!-- if cur seq id is a number and is greater than max, set new max for input box prefill below -->
                        <#if (productPromoCond.productPromoCondSeqId)??>
                            <#assign curCondSeqId = Static["java.lang.Integer"].valueOf(productPromoCond.getString("productPromoCondSeqId"))>
                            <#if (curCondSeqId >= maxCondSeqId)>
                              <#assign maxCondSeqId = curCondSeqId + 1>
                            </#if>
                        </#if>                                 
                        <form method="post" action="<@ofbizUrl>updateProductPromoCond</@ofbizUrl>">
                          <input type="hidden" name="productPromoId" value="${(productPromoCond.productPromoId)!}"/>
                          <input type="hidden" name="productPromoRuleId" value="${(productPromoCond.productPromoRuleId)!}"/>
                          <input type="hidden" name="productPromoCondSeqId" value="${(productPromoCond.productPromoCondSeqId)!}"/>
                          <@field type="select" name="inputParamEnumId" size="1" label=uiLabelMap.ProductCondition>
                            <#if (productPromoCond.inputParamEnumId)??>
                              <#assign inputParamEnum = productPromoCond.getRelatedOne("InputParamEnumeration", true)>
                              <option value="${productPromoCond.inputParamEnumId}"><#if inputParamEnum??>${(inputParamEnum.get("description",locale))!}<#else>[${(productPromoCond.inputParamEnumId)!}]</#if></option>
                              <option value="${(productPromoCond.inputParamEnumId)!}">&nbsp;</option>
                            <#else>
                              <option value="">&nbsp;</option>
                            </#if>
                            <#list inputParamEnums as inputParamEnum>
                              <option value="${(inputParamEnum.enumId)!}">${(inputParamEnum.get("description",locale))!}</option>
                            </#list>
                          </@field>
                          <@field type="select" name="operatorEnumId" size="1">
                            <#if (productPromoCond.operatorEnumId)??>
                              <#assign operatorEnum = productPromoCond.getRelatedOne("OperatorEnumeration", true)>
                              <option value="${(productPromoCond.operatorEnumId)!}"><#if operatorEnum??>${(operatorEnum.get("description",locale))!}<#else>[${(productPromoCond.operatorEnumId)!}]</#if></option>
                              <option value="${(productPromoCond.operatorEnumId)!}">&nbsp;</option>
                            <#else>
                              <option value="">&nbsp;</option>
                            </#if>
                            <#list condOperEnums as condOperEnum>
                              <option value="${(condOperEnum.enumId)!}">${(condOperEnum.get("description",locale))!}</option>
                            </#list>
                          </@field>
                          
                          <@field type="text" size="25" name="condValue" value=(productPromoCond.condValue)! label=uiLabelMap.ProductConditionValue />
						  <@field type="text" size="10" name="otherValue" label=uiLabelMap.CommonOther value=(productPromoCond.otherValue!) />
                          <#-- Reseting the values of carrierParty and carrierShippingMethod assigned in previous iteration of productPromoConds -->
                          <#assign carrierShippingMethod = ""/>
                          <#assign carrierParty = ""/>
                          <#if otherValue?has_content && otherValue.contains("@")>
                            <#assign carrierShippingMethod = productPromoCond.otherValue!>
                          </#if>
                          <#if carrierShippingMethod?has_content>
                            <#assign carrierParty = carrierShippingMethod.substring(0, carrierShippingMethod.indexOf("@"))>
                            <#assign shippingMethodTypeId = carrierShippingMethod.substring(carrierShippingMethod.indexOf("@")+1)>
                            <#assign description = (delegator.findOne("ShipmentMethodType", {"shipmentMethodTypeId":shippingMethodTypeId}, false)).description>
                          <#else>
                            <#assign description = "">
                          </#if>
                          <@field type="select" name="carrierShipmentMethod" label=uiLabelMap.OrderSelectShippingMethod>
                            <option value="${carrierShippingMethod!}">${carrierParty!}&nbsp;${description}</option>
                            <option value="">&nbsp;</option>
                            <#list carrierShipmentMethods as carrierShipmentMethod>
                              <#assign shipmentMethodType = carrierShipmentMethod.getRelatedOne("ShipmentMethodType", true)>
                              <option value="${carrierShipmentMethod.partyId!}@${carrierShipmentMethod.shipmentMethodTypeId!}">${carrierShipmentMethod.partyId!}&nbsp;${shipmentMethodType.get("description")!}</option>
                            </#list>
                          </@field>
                          <@field type="submitarea">
                              <@field type="submit" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}" />
                              <@field type="submit" submitType="button" href="javascript:document.deleteProductPromoCondition_${productPromoRule_index}_${productPromoCond_index}.submit()" 
                              	class="${styles.link_run_sys!} ${styles.action_remove!}" text=uiLabelMap.CommonDelete />	                              
                          </@field>
                        </form>
                        <form name="deleteProductPromoCondition_${productPromoRule_index}_${productPromoCond_index}" method="post" action="<@ofbizUrl>deleteProductPromoCond</@ofbizUrl>">
                          <input type="hidden" name="productPromoId" value="${(productPromoCond.productPromoId)!}" />
                          <input type="hidden" name="productPromoRuleId" value="${(productPromoCond.productPromoRuleId)!}" />
                          <input type="hidden" name="productPromoCondSeqId" value="${(productPromoCond.productPromoCondSeqId)!}" />                              
                        </form>
					</@section>
                    
                    <#-- ======================= Categories ======================== -->
					<@section title=(uiLabelMap.ProductConditionsCategoriesForCondition + productPromoCond.productPromoCondSeqId)!>
                      <#assign condProductPromoCategories = productPromoCond.getRelated("ProductPromoCategory", null, null, false)>
                      
                      <#if condProductPromoCategories?has_content>
	                      <@table type="data-complex" autoAltRows=true>
						      <@thead>
						        <@tr class="header-row">
						          <@th>${uiLabelMap.ProductProductCategoryId}</@th>
						          <@th>&nbsp;</@th>
						          <@th>${uiLabelMap.ProductIncludeSubCategories}</@th>
						          <@th>${uiLabelMap.CommonAnd} ${uiLabelMap.CommonGroup}</@th>
						          <@th>${uiLabelMap.ProductActions}</@th>					          
						        </@tr>
						      </@thead>	                      
			                  <#list condProductPromoCategories as condProductPromoCategory>
			                  	<@tr>    
			                        <#assign condProductCategory = condProductPromoCategory.getRelatedOne("ProductCategory", true)>
			                        <#assign condApplEnumeration = condProductPromoCategory.getRelatedOne("ApplEnumeration", true)>
			                        <@td>${(condProductCategory.get("description",locale))!} [${condProductPromoCategory.productCategoryId}]</@td>
			                        <@td>${(condApplEnumeration.get("description",locale))?default(condProductPromoCategory.productPromoApplEnumId)}</@td>
	                              	<@td>${uiLabelMap.ProductSubCats}? ${condProductPromoCategory.includeSubCategories!"N"}</@td>
	                              	<@td>${condProductPromoCategory.andGroupId}</@td>
	                              	<@td><a href="javascript:document.deleteProductPromoCategoryCondition_${productPromoRule_index}_${condProductPromoCategory_index}_${productPromoCond_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>		                        
		                        </@tr>
							     <form name="deleteProductPromoCategoryCondition_${productPromoRule_index}_${condProductPromoCategory_index}_${productPromoCond_index}" method="post" action="<@ofbizUrl>deleteProductPromoCategory</@ofbizUrl>">
	                                <input type="hidden" name="productPromoId" value="${(condProductPromoCategory.productPromoId)!}" />
	                                <input type="hidden" name="productPromoRuleId" value="${(condProductPromoCategory.productPromoRuleId)!}" />
	                                <input type="hidden" name="productPromoActionSeqId" value="${(condProductPromoCategory.productPromoActionSeqId)!}" />
	                                <input type="hidden" name="productPromoCondSeqId" value="${(condProductPromoCategory.productPromoCondSeqId)!}" />
	                                <input type="hidden" name="productCategoryId" value="${(condProductPromoCategory.productCategoryId)!}" />
	                                <input type="hidden" name="andGroupId" value="${(condProductPromoCategory.andGroupId)!}" />
	                             </form>  
	                          </#list>
	                      </@table>
                      <#else>
                      	<@commonMsg type="result-norecord">${uiLabelMap.ProductNoConditionCategories}</@commonMsg>
                      </#if>
                      <hr/>
                  	  <div>
                          <form method="post" action="<@ofbizUrl>createProductPromoCategory</@ofbizUrl>" name="createProductPromoCategoryConditions">
                            <input type="hidden" name="productPromoId" value="${productPromoId}" />
                            <input type="hidden" name="productPromoRuleId" value="${productPromoCond.productPromoRuleId}" />
                            <input type="hidden" name="productPromoActionSeqId" value="_NA_" />
                            <input type="hidden" name="productPromoCondSeqId" value="${productPromoCond.productPromoCondSeqId}" />
                            <@field type="lookup" formName="createProductPromoCategoryConditions" name="productCategoryId" id="productCategoryId_cond" fieldFormName="LookupProductCategory" label=uiLabelMap.ProductProductCategoryId/>
                            <@field type="select" name="productPromoApplEnumId">
                              <#list productPromoApplEnums as productPromoApplEnum>
                                <option value="${productPromoApplEnum.enumId}">${productPromoApplEnum.get("description",locale)}</option>
                              </#list>
                            </@field>
                            <@field type="select" name="includeSubCategories" label=uiLabelMap.ProductIncludeSubCategories>
                              <option value="N">${uiLabelMap.CommonN}</option>
                              <option value="Y">${uiLabelMap.CommonY}</option>
                            </@field>
                            <@field type="text" size="10" maxlength="20" name="andGroupId" value="_NA_" label=(uiLabelMap.CommonAnd + ' ' + uiLabelMap.CommonGroup) required=true/>
                            <@field type="submit" text=uiLabelMap.ProductAddConditionCategory class="${styles.link_run_sys!} ${styles.action_add!}" />
                          </form>
                      </div>
                    </@section>

                    <#-- ======================= Products ======================== -->
                    <@section title=(uiLabelMap.ProductConditionsProductsForCondition + productPromoCond.productPromoCondSeqId)!>
		                <#if condProductPromoProducts?has_content>
	                      <@table type="data-complex" autoAltRows=true>
						      <@thead>
						        <@tr class="header-row">
						          <@th>${uiLabelMap.ProductProductId}</@th>
						          <@th>&nbsp;</@th>
						          <@th>${uiLabelMap.ProductActions}</@th>					          
						        </@tr>
						      </@thead>	                      
			                  <#list condProductPromoProducts as condProductPromoProduct>
			                    <#assign condProduct = condProductPromoProduct.getRelatedOne("Product", true)!>
			                    <#assign condApplEnumeration = condProductPromoProduct.getRelatedOne("ApplEnumeration", true)>>
			                  	<@tr>
			                        <@td>${(condProduct.internalName)!} [${condProductPromoProduct.productId}]</@td>
			                        <@td>${(condApplEnumeration.get("description",locale))?default(condProductPromoProduct.productPromoApplEnumId)}</@td>	                              	
	                              	<@td><a href="javascript:document.deleteProductPromoProductCondition_${productPromoRule_index}_${productPromoCond_index}_${condProductPromoProduct_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>	                              		                        
		                        </@tr>
							    <form name="deleteProductPromoProductCondition_${productPromoRule_index}_${productPromoCond_index}_${condProductPromoProduct_index}" method="post" action="<@ofbizUrl>deleteProductPromoProduct</@ofbizUrl>">
	                                <input type="hidden" name="productPromoId" value="${(condProductPromoProduct.productPromoId)!}" />
	                                <input type="hidden" name="productPromoRuleId" value="${(condProductPromoProduct.productPromoRuleId)!}" />
	                                <input type="hidden" name="productPromoActionSeqId" value="${(condProductPromoProduct.productPromoActionSeqId)!}" />
	                                <input type="hidden" name="productPromoCondSeqId" value="${(condProductPromoProduct.productPromoCondSeqId)!}" />
	                                <input type="hidden" name="productId" value="${(condProductPromoProduct.productId)!}" />	                                
                              	</form>
	                          </#list>
	                      </@table>
                      	<#else>
                      		<@commonMsg type="result-norecord">${uiLabelMap.ProductNoConditionProducts}</@commonMsg>
                      	</#if>
		                <hr/>
		                <div>
	                      <form method="post" action="<@ofbizUrl>createProductPromoProduct</@ofbizUrl>"  name="createProductPromoProductConditions">
	                        <input type="hidden" name="productPromoId" value="${productPromoId}" />
	                        <input type="hidden" name="productPromoRuleId" value="${productPromoCond.productPromoRuleId}" />
	                        <input type="hidden" name="productPromoActionSeqId" value="_NA_" />
	                        <input type="hidden" name="productPromoCondSeqId" value="${productPromoCond.productPromoCondSeqId}" />
	                        <@field type="lookup" formName="createProductPromoProductConditions" name="productId" id="productId_cond" fieldFormName="LookupProduct" label=uiLabelMap.ProductProductId/>
	                        <@field type="select" name="productPromoApplEnumId">
	                          <#list productPromoApplEnums as productPromoApplEnum>
	                            <option value="${productPromoApplEnum.enumId}">${productPromoApplEnum.get("description",locale)}</option>
	                          </#list>
	                        </@field>
	                        <@field type="submit" text=uiLabelMap.ProductAddConditionProducts class="${styles.link_run_sys!} ${styles.action_add!}" />
	                      </form>
		                </div>
                  	</@section>
        		</@modal>
                </li>
            </#list>
            </ol>
        </@td>

         <#-- Rule Actions -->
        <@td>
            <ol>
            <#list productPromoActions as productPromoAction>
                <li>
                <#assign actionLabel>                    
                    <#if (productPromoAction.productPromoActionEnumId)?has_content>
                        <#assign productPromoActionCurEnum = productPromoAction.getRelatedOne("ActionEnumeration", true)>
                        ${(productPromoActionCurEnum.get("description",locale))?default(productPromoAction.productPromoActionEnumId!"")}
                    </#if>
                    <ul>
                    <#if (productPromoAction.quantity)?has_content><li>${uiLabelMap.ProductQuantity}: ${(productPromoAction.quantity)!}</li></#if>
                    <#if (productPromoAction.amount)?has_content><li>${uiLabelMap.ProductAmount}: ${(productPromoAction.amount)!}</li></#if>
                    <#if (productPromoAction.productId)?has_content><li>${uiLabelMap.ProductItemId}: ${(productPromoAction.productId)!}</li></#if>
                    <#if (productPromoAction.partyId)?has_content><li>${uiLabelMap.PartyParty}: ${(productPromoAction.partyId)!}</li></#if>
                    <#if (productPromoAction.serviceName)?has_content><li>${uiLabelMap.ProductServiceName} ${(productPromoAction.serviceName)!}</li></#if>
                    <#if (productPromoAction.useCartQuantity)?has_content && (productPromoAction.useCartQuantity.equals("Y"))><li>${uiLabelMap.UseCartQuantity}</li></#if>
                    </ul>
                    <#-- Category -->
                    <#assign actionProductPromoCategories = productPromoAction.getRelated("ProductPromoCategory", null, null, false)>
                    <#if actionProductPromoCategories?has_content>
                        <ul>
                          <#list actionProductPromoCategories as actionProductPromoCategory>
                            <li>
                            <#assign actionProductCategory = actionProductPromoCategory.getRelatedOne("ProductCategory", true)>
                            <#assign actionApplEnumeration = actionProductPromoCategory.getRelatedOne("ApplEnumeration", true)>
                                  ${(actionProductCategory.description)!} [${actionProductPromoCategory.productCategoryId}]
                                  - ${(actionApplEnumeration.get("description",locale))?default(actionProductPromoCategory.productPromoApplEnumId)}
                                  - ${uiLabelMap.ProductSubCats}? ${actionProductPromoCategory.includeSubCategories!"N"}
                                  - ${uiLabelMap.CommonAnd} ${uiLabelMap.CommonGroup}: ${actionProductPromoCategory.andGroupId}
                            </li>
                          </#list>
                        </ul>
                    </#if>
                    
                    <#-- Product -->
                    <#assign actionProductPromoProducts = productPromoAction.getRelated("ProductPromoProduct", null, null, false)>
                    <#if actionProductPromoProducts?has_content>
                        <ul>
	                        <#list actionProductPromoProducts as actionProductPromoProduct>
	                        <#assign actionProduct = actionProductPromoProduct.getRelatedOne("Product", true)!>
	                        <#assign actionApplEnumeration = actionProductPromoProduct.getRelatedOne("ApplEnumeration", true)>
	                                <li>
	                                  ${(actionProduct.internalName)!} [${actionProductPromoProduct.productId}]
	                                  - ${(actionApplEnumeration.get("description",locale))?default(actionProductPromoProduct.productPromoApplEnumId)}
	                                </li>
	                        </#list>
                        </ul>
                    </#if>
                </#assign>

                <@modal id="modal_rule_action_${(productPromoCond.productPromoRuleId)!}_${(productPromoCond.productPromoCondSeqId)!}" label=wrapAsRaw(actionLabel, 'htmlmarkup')>
                    <@section title=(uiLabelMap.ProductActionForRule + productPromoRule.productPromoRuleId)!>
					  <#-- <b> ${uiLabelMap.ProductAction} ${(productPromoAction.productPromoActionSeqId)!}</b> -->
                      <form method="post" action="<@ofbizUrl>updateProductPromoAction</@ofbizUrl>">
                        <input type="hidden" name="productPromoId" value="${(productPromoAction.productPromoId)!}" />
                        <input type="hidden" name="productPromoRuleId" value="${(productPromoAction.productPromoRuleId)!}" />
                        <input type="hidden" name="productPromoActionSeqId" value="${(productPromoAction.productPromoActionSeqId)!}" />
                        <input type="hidden" name="orderAdjustmentTypeId" value="${(productPromoAction.orderAdjustmentTypeId)!}" />
                        <@field type="select" name="productPromoActionEnumId" size="1">
                          <#if (productPromoAction.productPromoActionEnumId)??>
                            <#assign productPromoActionCurEnum = productPromoAction.getRelatedOne("ActionEnumeration", true)>
                            <option value="${(productPromoAction.productPromoActionEnumId)!}"><#if productPromoActionCurEnum??>${(productPromoActionCurEnum.get("description",locale))!}<#else>[${(productPromoAction.productPromoActionEnumId)!}]</#if></option>
                            <option value="${(productPromoAction.productPromoActionEnumId)!}">&nbsp;</option>
                          <#else>
                            <option value="">&nbsp;</option>
                          </#if>
                          <#list productPromoActionEnums as productPromoActionEnum>
                            <option value="${(productPromoActionEnum.enumId)!}">${(productPromoActionEnum.get("description",locale))!}</option>
                          </#list>
                        </@field>
                        <@field type="text" size="5" name="quantity" value=(productPromoAction.quantity)! label=uiLabelMap.ProductQuantity />
                        <@field type="text" size="5" name="amount" value=(productPromoAction.amount)! label=uiLabelMap.ProductAmount />
                        <@field type="text" size="15" name="productId" value=(productPromoAction.productId)! label=uiLabelMap.ProductItemId />
                        <@field type="text" size="10" name="partyId" value=(productPromoAction.partyId)! label=uiLabelMap.PartyParty />
                        <@field type="text" size="20" name="serviceName" value=(productPromoAction.serviceName)! label=uiLabelMap.ProductServiceName />
                        <@field type="select" name="useCartQuantity" label=uiLabelMap.UseCartQuantity>
                          <#if (productPromoAction.useCartQuantity)??>
                            <#assign productPromoActionCurEnum = productPromoAction.getRelatedOne("ActionEnumeration", true)>
                            <option value="${(productPromoAction.useCartQuantity)!}"><#if (productPromoAction.useCartQuantity.equals("Y"))>${uiLabelMap.CommonY}<#else>${uiLabelMap.CommonN}</#if></option>
                            <option value="${(productPromoAction.useCartQuantity)!}">&nbsp;</option>
                          <#else>
                            <option value="">&nbsp;</option>
                          </#if>
                          <option value="N">${uiLabelMap.CommonN}</option>
                          <option value="Y">${uiLabelMap.CommonY}</option>
                        </@field>
                        <@field type="submitarea">
                        	<@field type="submit" text="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}" />
                        	<@field type="submit" submitType="button" href="javascript:document.deleteProductPromoAction_${productPromoRule_index}_${productPromoAction_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}" text=uiLabelMap.CommonDelete />
                       	</@field>
                      </form>
                      <form name="deleteProductPromoAction_${productPromoRule_index}_${productPromoAction_index}" method="post" action="<@ofbizUrl>deleteProductPromoAction</@ofbizUrl>">
                        <input type="hidden" name="productPromoId" value="${(productPromoAction.productPromoId)!}" />
                        <input type="hidden" name="productPromoRuleId" value="${(productPromoAction.productPromoRuleId)!}" />
                        <input type="hidden" name="productPromoActionSeqId" value="${(productPromoAction.productPromoActionSeqId)!}" />                        
                      </form>
                    </@section>
                            
                    <#-- ======================= Categories ======================== -->
                    <@section title=(uiLabelMap.ProductActionsCategoriesForAction + productPromoAction.productPromoActionSeqId)!>
                    	<#assign actionProductPromoCategories = productPromoAction.getRelated("ProductPromoCategory", null, null, false)>
                        <#if actionProductPromoCategories?has_content>
                            <@table type="data-complex" autoAltRows=true>
						      <@thead>
						        <@tr class="header-row">
						          <@th>${uiLabelMap.ProductProductCategoryId}</@th>
						          <@th>&nbsp;</@th>
						          <@th>${uiLabelMap.ProductIncludeSubCategories}</@th>
						          <@th>${uiLabelMap.CommonAnd} ${uiLabelMap.CommonGroup}</@th>
						          <@th>${uiLabelMap.ProductActions}</@th>					          
						        </@tr>
						      </@thead>	                      
			                  <#list actionProductPromoCategories as actionProductPromoCategory>
			                  	<@tr>    
			                        <#assign actionProductCategory = actionProductPromoCategory.getRelatedOne("ProductCategory", true)>
                                	<#assign actionApplEnumeration = actionProductPromoCategory.getRelatedOne("ApplEnumeration", true)>
			                        <@td>${(actionProductCategory.get("description",locale))!} [${actionProductPromoCategory.productCategoryId}]</@td>
			                        <@td>${(actionApplEnumeration.get("description",locale))?default(actionProductPromoCategory.productPromoApplEnumId)}</@td>
	                              	<@td>${uiLabelMap.ProductSubCats}? ${actionProductPromoCategory.includeSubCategories!"N"}</@td>
	                              	<@td>${actionProductPromoCategory.andGroupId}</@td>
	                              	<@td><a href="javascript:document.deleteProductPromoCategoryAction_${productPromoRule_index}_${productPromoAction_index}_${actionProductPromoCategory_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>	                              	     		                        
		                        </@tr>
							     <form name="deleteProductPromoCategoryAction_${productPromoRule_index}_${productPromoAction_index}_${actionProductPromoCategory_index}" action="<@ofbizUrl>deleteProductPromoCategory</@ofbizUrl>" method="post">
                                    <input type="hidden" name="productPromoId" value="${(actionProductPromoCategory.productPromoId)!}" />
                                    <input type="hidden" name="productPromoRuleId" value="${(actionProductPromoCategory.productPromoRuleId)!}" />
                                    <input type="hidden" name="productPromoCondSeqId" value="${(actionProductPromoCategory.productPromoCondSeqId)!}" />
                                    <input type="hidden" name="productPromoActionSeqId" value="${(actionProductPromoCategory.productPromoActionSeqId)!}" />
                                    <input type="hidden" name="productCategoryId" value="${(actionProductPromoCategory.productCategoryId)!}" />
                                    <input type="hidden" name="andGroupId" value="${(actionProductPromoCategory.andGroupId)!}" />
                                  </form> 
	                          </#list>
	                      </@table>
						<#else>
                            <@commonMsg type="result-norecord">${uiLabelMap.ProductNoActionCategories}</@commonMsg>
                        </#if>
                        <hr />
                        <div>
                          <form method="post" action="<@ofbizUrl>createProductPromoCategory</@ofbizUrl>" name="createProductPromoCategoryActions">
                            <input type="hidden" name="productPromoId" value="${productPromoId}" />
                            <input type="hidden" name="productPromoRuleId" value="${productPromoAction.productPromoRuleId}" />
                            <input type="hidden" name="productPromoActionSeqId" value="${productPromoAction.productPromoActionSeqId}" />
                            <input type="hidden" name="productPromoCondSeqId" value="_NA_" />
                            <@field type="lookup" formName="createProductPromoCategoryActions" name="productCategoryId" id="productCategoryId_act" fieldFormName="LookupProductCategory" label=uiLabelMap.ProductProductCategoryId />
                            <@field type="select" name="productPromoApplEnumId">
                              <#list productPromoApplEnums as productPromoApplEnum>
                                <option value="${productPromoApplEnum.enumId}">${productPromoApplEnum.get("description",locale)}</option>
                              </#list>
                            </@field>
                            <@field type="select" name="includeSubCategories" label=uiLabelMap.ProductIncludeSubCategories>
                              <option value="N">${uiLabelMap.CommonN}</option>
                              <option value="Y">${uiLabelMap.CommonY}</option>
                            </@field>
                            <@field type="text" size="10" maxlength="20" name="andGroupId" value="_NA_" label=(uiLabelMap.CommonAnd + ' ' + uiLabelMap.CommonGroup) required=true />
                            <@field type="submit" text=uiLabelMap.ProductAddActionCategory class="${styles.link_run_sys!} ${styles.action_add!}" />
                          </form>
                        </div>
                    </@section>
                        
                    <#-- ======================= Products ======================== -->
                    <@section title=(uiLabelMap.ProductActionsProductsForAction + productPromoAction.productPromoActionSeqId)!>
                      <#assign actionProductPromoProducts = productPromoAction.getRelated("ProductPromoProduct", null, null, false)>
                      <#if actionProductPromoProducts?has_content>
	                      <@table type="data-complex" autoAltRows=true>
						      <@thead>
						        <@tr class="header-row">
						          <@th>${uiLabelMap.ProductProductId}</@th>
						          <@th>&nbsp;</@th>
						          <@th>${uiLabelMap.ProductActions}</@th>					          
						        </@tr>
						      </@thead>	                      
			                  <#list actionProductPromoProducts as actionProductPromoProduct>
	                        	<#assign actionProduct = actionProductPromoProduct.getRelatedOne("Product", true)!>
	                        	<#assign actionApplEnumeration = actionProductPromoProduct.getRelatedOne("ApplEnumeration", true)>
			                  	<@tr>
			                        <@td>${(actionProduct.internalName)!} [${actionProductPromoProduct.productId}]</@td>
			                        <@td>${(actionApplEnumeration.get("description",locale))?default(actionProductPromoProduct.productPromoApplEnumId)}</@td>	                              	
	                              	<@td><a href="javascript:document.deleteProductPromoProductAction_${productPromoRule_index}_${productPromoAction_index}_${actionProductPromoProduct_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a></@td>	                              		 	                              		                        
		                        </@tr>
							    <form name="deleteProductPromoProductAction_${productPromoRule_index}_${productPromoAction_index}_${actionProductPromoProduct_index}" method="post" action="<@ofbizUrl>deleteProductPromoProduct</@ofbizUrl>"> 
		                            <input type="hidden" name="productPromoId" value="${(actionProductPromoProduct.productPromoId)!}" />
		                            <input type="hidden" name="productPromoRuleId" value="${(actionProductPromoProduct.productPromoRuleId)!}" />
		                            <input type="hidden" name="productPromoCondSeqId" value="${(actionProductPromoProduct.productPromoCondSeqId)!}" />
		                            <input type="hidden" name="productPromoActionSeqId" value="${(actionProductPromoProduct.productPromoActionSeqId)!}" />
		                            <input type="hidden" name="productId" value="${(actionProductPromoProduct.productId)!}" />
		                          </form>
	                          </#list>
	                      </@table>
                      <#else>
                        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoActionProducts}</@commonMsg>
                      </#if>
                      <hr/>
	                  <div>
	                      <form method="post" action="<@ofbizUrl>createProductPromoProduct</@ofbizUrl>" name="createProductPromoProductActions">
	                        <input type="hidden" name="productPromoId" value="${productPromoId}" />
	                        <input type="hidden" name="productPromoRuleId" value="${productPromoAction.productPromoRuleId}" />
	                        <input type="hidden" name="productPromoActionSeqId" value="${productPromoAction.productPromoActionSeqId}" />
	                        <input type="hidden" name="productPromoCondSeqId" value="_NA_" />	                        
	                        <@field type="lookup" formName="createProductPromoProductActions" name="productId" id="productId_action" fieldFormName="LookupProduct" label=uiLabelMap.ProductProductId/>
	                        <@field type="select" name="productPromoApplEnumId">
	                          <#list productPromoApplEnums as productPromoApplEnum>
	                            <option value="${productPromoApplEnum.enumId}">${productPromoApplEnum.get("description",locale)}</option>
	                          </#list>
	                        </@field>
	                        <@field type="submit" text=uiLabelMap.ProductAddActionProducts class="${styles.link_run_sys!} ${styles.action_add!}" />
	                      </form>
	                  </div>
                    </@section>      
               	</@modal>
                </li>
            </#list>
            </ol>
        </@td>

        <@td align="center">
                <@menu type="button">
                    <@menuitem type="generic">
                        <@modal id="modal_new_promo_condition_${productPromoId}_${(productPromoRule.productPromoRuleId)!}" label=uiLabelMap.ProductCreateCondition linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                            <@heading>${uiLabelMap.ProductAddPromoRule}</@heading>
                                <#-- SCIPIO: TODO: Convert to @field -->
                                <form method="post" action="<@ofbizUrl>createProductPromoCond</@ofbizUrl>">
                                  <input type="hidden" name="productPromoId" value="${(productPromoRule.productPromoId)!}" />
                                  <input type="hidden" name="productPromoRuleId" value="${(productPromoRule.productPromoRuleId)!}" />
                                  <span><b>${uiLabelMap.CommonNew}</b>&nbsp;</span>
                                  <select name="inputParamEnumId" size="1">
                                    <#list inputParamEnums as inputParamEnum>
                                      <option value="${(inputParamEnum.enumId)!}">${(inputParamEnum.get("description",locale))!}</option>
                                    </#list>
                                  </select>
                                  <select name="operatorEnumId" size="1">
                                    <#list condOperEnums as condOperEnum>
                                      <option value="${(condOperEnum.enumId)!}">${(condOperEnum.get("description",locale))!}</option>
                                    </#list>
                                  </select>
                                  <label>${uiLabelMap.ProductConditionValue}</label>
                                  <input type="text" size="25" name="condValue" />
                                  ${uiLabelMap.CommonOther}<input type="text" size="10" name="otherValue" />
                                  <label>${uiLabelMap.OrderSelectShippingMethod}</label>
                                  <select name="carrierShipmentMethod">
                                    <option value="">--${uiLabelMap.OrderSelectShippingMethod}--</option>
                                    <#list carrierShipmentMethods as carrierShipmentMethod>
                                      <#assign shipmentMethodType = carrierShipmentMethod.getRelatedOne("ShipmentMethodType", true)>
                                      <option value="${carrierShipmentMethod.partyId!}@${carrierShipmentMethod.shipmentMethodTypeId!}">${carrierShipmentMethod.partyId!}&nbsp;${shipmentMethodType.get("description")!}</option>
                                    </#list>
                                  </select>
                                  <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                                </form>
                            </@modal>
                    </@menuitem>
                    <@menuitem type="generic">
                        <@modal id="modal_new_promo_action_${productPromoId}_${(productPromoRule.productPromoRuleId)!}" label=uiLabelMap.ProductCreateAction linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_add!}">
                            <@heading>${uiLabelMap.ProductCreateAction}</@heading>
                                <#-- SCIPIO: TODO: Convert to @field -->
                                <form method="post" action="<@ofbizUrl>createProductPromoAction</@ofbizUrl>">
                                    <input type="hidden" name="productPromoId" value="${(productPromoRule.productPromoId)!}" />
                                    <input type="hidden" name="productPromoRuleId" value="${(productPromoRule.productPromoRuleId)!}" />
                                    <span><b>${uiLabelMap.CommonNew}</b>&nbsp;</span>
                                    <select name="productPromoActionEnumId" size="1">
                                      <#list productPromoActionEnums as productPromoActionEnum>
                                        <option value="${(productPromoActionEnum.enumId)!}">${(productPromoActionEnum.get("description",locale))!}</option>
                                      </#list>
                                    </select>
                                    <input type="hidden" name="orderAdjustmentTypeId" value="PROMOTION_ADJUSTMENT" />
                                    ${uiLabelMap.ProductQuantity}&nbsp;<input type="text" size="5" name="quantity" />
                                    ${uiLabelMap.ProductAmount}&nbsp;<input type="text" size="5" name="amount" />
                                    ${uiLabelMap.ProductItemId}&nbsp;<input type="text" size="15" name="productId" />
                                    ${uiLabelMap.PartyParty}&nbsp;<input type="text" size="10" name="partyId" /><br />
                                    ${uiLabelMap.ProductServiceName}&nbsp;<input type="text" size="20" name="serviceName" />
                                    ${uiLabelMap.UseCartQuantity}&nbsp;
                                    <select name="useCartQuantity">
                                      <option value="N">${uiLabelMap.CommonN}</option>
                                      <option value="Y">${uiLabelMap.CommonY}</option>
                                    </select>
                                    <input type="submit" value="${uiLabelMap.CommonAdd}" class="${styles.link_run_sys!} ${styles.action_add!}" />
                                  </form>
                            </@modal>
                    </@menuitem>

                    <#if (productPromoConds.size() == 0 && productPromoActions.size() == 0)>
                          <@menuitem type="generic">
                              <form name="deleteProductPromoRule_${productPromoRule_index}" method="post" action="<@ofbizUrl>deleteProductPromoRule</@ofbizUrl>">
                                <input type="hidden" name="productPromoId" value="${(productPromoRule.productPromoId)!}" />
                                <input type="hidden" name="productPromoRuleId" value="${(productPromoRule.productPromoRuleId)!}" />
                                <a href="javascript:document.deleteProductPromoRule_${productPromoRule_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                              </form>
                          </@menuitem>
                    </#if>
                </@menu>
        </@td>
      </@tr>
  </#list>
    </@table>
  
  <#-- This was removed in r697962, should have been only commented out as maybe in future will be used again (free shipping promo)
  <div class="tooltip"><b>${uiLabelMap.ProductNoteOnItemId} :</b> ${uiLabelMap.ProductItemIdGiftPurchaseFreeShipping}</div>
  <div class="tooltip"><b>${uiLabelMap.ProductNoteOnPartyId} :</b> ${uiLabelMap.ProductPartyFreeShipping}</div>
  -->
  <#-- </@fields> -->
</@section>


<#if promoProductPromoCategories?has_content>
    <@section title="${rawLabel('ProductPromotion')} ${rawLabel('ProductCategories')}">
      <#-- ======================= Categories ======================== -->
      <#list promoProductPromoCategories as promoProductPromoCategory>
        <#assign promoProductCategory = promoProductPromoCategory.getRelatedOne("ProductCategory", true)>
        <#assign promoApplEnumeration = promoProductPromoCategory.getRelatedOne("ApplEnumeration", true)>
        <div>
          ${(promoProductCategory.description)!} [${promoProductPromoCategory.productCategoryId}]
          - ${(promoApplEnumeration.get("description",locale))?default(promoProductPromoCategory.productPromoApplEnumId)}
          - ${uiLabelMap.ProductSubCats}? ${promoProductPromoCategory.includeSubCategories!"N"}
          - ${uiLabelMap.CommonAnd} ${uiLabelMap.CommonGroup}: ${promoProductPromoCategory.andGroupId}
          <form name="deleteProductPromoCategoryAction_${promoProductPromoCategory_index}" method="post" action="<@ofbizUrl>deleteProductPromoCategory</@ofbizUrl>">
            <input type="hidden" name="productPromoId" value="${(promoProductPromoCategory.productPromoId)!}" />
            <input type="hidden" name="productPromoRuleId" value="${(promoProductPromoCategory.productPromoRuleId)!}" />
            <input type="hidden" name="productPromoActionSeqId" value="${(promoProductPromoCategory.productPromoActionSeqId)!}" />
            <input type="hidden" name="productPromoCondSeqId" value="${(promoProductPromoCategory.productPromoCondSeqId)!}" />
            <input type="hidden" name="productCategoryId" value="${(promoProductPromoCategory.productCategoryId)!}" />
            <input type="hidden" name="andGroupId" value="${(promoProductPromoCategory.andGroupId)!}" />
            <a href="javascript:document.deleteProductPromoCategoryAction_${promoProductPromoCategory_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
          </form>
        </div>
      </#list>
    </@section>
</#if>

<#if promoProductPromoProducts?has_content>
    <@section title=uiLabelMap.ProductPromotionProducts>
      <#-- ======================= Products ======================== -->
      <#list promoProductPromoProducts as promoProductPromoProduct>
        <#assign promoProduct = promoProductPromoProduct.getRelatedOne("Product", true)!>
        <#assign promoApplEnumeration = promoProductPromoProduct.getRelatedOne("ApplEnumeration", true)>
        <div>
          ${(promoProduct.internalName)!} [${promoProductPromoProduct.productId}]
          - ${(promoApplEnumeration.get("description",locale))?default(promoProductPromoProduct.productPromoApplEnumId)}
          <form name="deleteProductPromoProductAction_${promoProductPromoProduct_index}" action="<@ofbizUrl>deleteProductPromoProduct</@ofbizUrl>" method="post">
            <input type="hidden" name="productPromoId" value="${(promoProductPromoProduct.productPromoId)!}" />
            <input type="hidden" name="productPromoRuleId" value="${(promoProductPromoProduct.productPromoRuleId)!}" />
            <input type="hidden" name="productPromoActionSeqId" value="${(promoProductPromoProduct.productPromoActionSeqId)!}" />
            <input type="hidden" name="productPromoCondSeqId" value="${(promoProductPromoProduct.productPromoCondSeqId)!}" />
            <input type="hidden" name="productId" value="${(promoProductPromoProduct.productId)!}" />
            <a href="javascript:document.deleteProductPromoProductAction_${promoProductPromoProduct_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
          </form>
        </div>
      </#list>
    </@section>
</#if>
</#if>