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
<#escape x as x?xml>
<#if orderHeader?has_content><fo:block font-size="16pt" font-weight="bold" margin-bottom="5mm">${orderHeader.getRelatedOne("OrderType", false).get("description",locale)}</fo:block></#if>


    <#-- list of terms -->
    <#if terms?has_content>
    <fo:table table-layout="fixed" width="100%" inline-progression-dimension="auto">
        <fo:table-column/>

        <fo:table-header height="10mm">
          <fo:table-row>
            <fo:table-cell>
              <fo:block font-weight="bold">${uiLabelMap.AccountingAgreementItemTerms}</fo:block>
            </fo:table-cell>
          </fo:table-row>
        </fo:table-header>

        <fo:table-body>
          <#list terms as term>
          <#assign termType = term.getRelatedOne("TermType", false)/>
          <fo:table-row>
            <fo:table-cell>
              <fo:block font-size="10pt">${termType.description!} ${term.description!} ${term.termDays!} ${term.textValue!}</fo:block>
            </fo:table-cell>
          </fo:table-row>
          </#list>
        </fo:table-body>
    </fo:table>
    </#if>
    
    

    <fo:table table-layout="fixed" width="100%" space-before="20mm">
    <fo:table-column column-width="50mm"/>
    <fo:table-column column-width="55mm"/>
    <fo:table-column column-width="15mm"/>
    <fo:table-column column-width="25mm"/>
    <fo:table-column column-width="25mm"/>

    <fo:table-header height="10mm" font-size="12pt">
      <fo:table-row border-bottom-style="solid" border-bottom-width="thin" border-bottom-color="black">
        <fo:table-cell>
          <fo:block font-weight="bold">${uiLabelMap.OrderProduct}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold">${uiLabelMap.CommonDescription}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold" text-align="right">${uiLabelMap.OrderQuantity}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold" text-align="right">${uiLabelMap.OrderUnitList}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold" text-align="right">${uiLabelMap.OrderSubTotal}</fo:block>
        </fo:table-cell>
      </fo:table-row>
    </fo:table-header>


    <fo:table-body font-size="10pt" table-layout="fixed" width="100%">
        <#list orderItemList as orderItem>
            <#assign orderItemType = orderItem.getRelatedOne("OrderItemType", false)!>
            <#assign productId = orderItem.productId!>
            <#assign remainingQuantity = (orderItem.quantity?default(0) - orderItem.cancelQuantity?default(0))>
            <#assign itemAdjustment = Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemAdjustmentsTotal(orderItem, orderAdjustments, true, false, false)>
            <#assign internalImageUrl = Static["org.ofbiz.product.imagemanagement.ImageManagementHelper"].getInternalImageUrl(request, productId!)!>
            
            <fo:table-row height="8mm" line-height="8mm">
                <fo:table-cell>
                    <fo:block text-align="left">
                        <#if orderItem.supplierProductId?has_content>
                            ${orderItem.supplierProductId}
                        <#elseif productId??>
                            ${orderItem.productId!(uiLabelMap.CommonNA)}
                        <#elseif orderItemType??>
                            ${orderItemType.get("description",locale)}
                        <#else>
                        </#if>
                    </fo:block>
                </fo:table-cell>
                <fo:table-cell>
                    <fo:block text-align="left">${orderItem.itemDescription!}</fo:block>
                </fo:table-cell>
                  <fo:table-cell>
                    <fo:block text-align="right"><#if remainingQuantity??>${remainingQuantity?string.number}</#if> </fo:block>
                </fo:table-cell>
                <fo:table-cell text-align="right">
                    <fo:block> <#if orderItem.unitPrice??><@ofbizCurrency amount=(orderItem.unitPrice!) isoCode=(currencyUomId!)/></#if> </fo:block>
                </fo:table-cell>
                <fo:table-cell text-align="right">
                    <fo:block> <#if orderItem.statusId != "ITEM_CANCELLED">
                                <@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemSubTotal(orderItem, orderAdjustments) isoCode=currencyUomId/>
                            <#else>
                                <@ofbizCurrency amount=0.00 isoCode=currencyUomId/>
                            </#if>
                    </fo:block>
                </fo:table-cell>
            </fo:table-row>        
        </#list>
                
        <#-- blank line -->
        <fo:table-row height="7px">
            <fo:table-cell number-columns-spanned="5"><fo:block><#-- blank line --></fo:block></fo:table-cell>
        </fo:table-row>

        <fo:table-row height="8mm" line-height="8mm">
           <fo:table-cell number-columns-spanned="2">
              <fo:block/>
           </fo:table-cell>
           <fo:table-cell number-columns-spanned="2" text-align="right" padding-before="3pt" padding-after="3pt">
              <fo:block>${uiLabelMap.OrderSubTotal}</fo:block>
           </fo:table-cell>
           <fo:table-cell text-align="right" border-top-style="solid" border-top-width="thin" border-top-color="black" border-bottom-style="solid" border-bottom-width="thin" border-bottom-color="black" padding-before="3pt" padding-after="3pt">
              <fo:block>
                 <@ofbizCurrency amount=orderSubTotal isoCode=currencyUomId/>
              </fo:block>
           </fo:table-cell>
        </fo:table-row>
        
        
        <#list orderHeaderAdjustments as orderHeaderAdjustment>
            <#assign adjustmentType = orderHeaderAdjustment.getRelatedOne("OrderAdjustmentType", false)>
            <#assign adjustmentAmount = Static["org.ofbiz.order.order.OrderReadHelper"].calcOrderAdjustment(orderHeaderAdjustment, orderSubTotal)>
            <#if adjustmentAmount != 0>
                <fo:table-row height="8mm" line-height="8mm" font-size="8pt" >
                    <fo:table-cell number-columns-spanned="4">
                        <fo:block text-align="right" font-weight="bold">${adjustmentType.get("description",locale)} :
                            <#if orderHeaderAdjustment.get("description")?has_content>
                                (${orderHeaderAdjustment.get("description")!})
                            </#if></fo:block>
                    </fo:table-cell>
                    <fo:table-cell text-align="right">
                        <fo:block><@ofbizCurrency amount=adjustmentAmount isoCode=currencyUomId/></fo:block>
                    </fo:table-cell>
                </fo:table-row>
            </#if>
        </#list>

        <#if otherAdjAmount != 0>
            <#-- blank line -->
            <fo:table-row height="7px">
                <fo:table-cell number-columns-spanned="5"><fo:block><#-- blank line --></fo:block></fo:table-cell>
            </fo:table-row>
            <fo:table-row height="8mm" line-height="8mm">
                <fo:table-cell number-columns-spanned="4">
                    <fo:block text-align="right" font-weight="bold">${uiLabelMap.OrderTotalOtherOrderAdjustments}:</fo:block>
                </fo:table-cell>
                <fo:table-cell text-align="right">
                    <fo:block><@ofbizCurrency amount=otherAdjAmount isoCode=currencyUomId/></fo:block>
                </fo:table-cell>
            </fo:table-row>
        </#if>
        
        <#if shippingAmount != 0>
            <fo:table-row height="8mm" line-height="8mm">
                <fo:table-cell number-columns-spanned="4">
                    <fo:block text-align="right" font-weight="bold">${uiLabelMap.OrderTotalShippingAndHandling}:</fo:block>
                </fo:table-cell>
                <fo:table-cell text-align="right" border-top-style="solid" border-top-width="thin" border-top-color="black" padding-before="3pt" padding-after="3pt">
                    <fo:block><@ofbizCurrency amount=shippingAmount isoCode=currencyUomId/></fo:block>
                </fo:table-cell>
            </fo:table-row>
        </#if>
        
        <#if taxAmount != 0>
            <fo:table-row height="8mm" line-height="8mm">
                <fo:table-cell number-columns-spanned="4">
                    <fo:block text-align="right" font-weight="bold">${uiLabelMap.OrderTotalSalesTax}:</fo:block>
                </fo:table-cell>
                <fo:table-cell text-align="right" border-top-style="solid" border-top-width="thin" border-top-color="black" padding-before="3pt" padding-after="3pt">
                    <fo:block><@ofbizCurrency amount=taxAmount isoCode=currencyUomId/></fo:block>
                </fo:table-cell>
            </fo:table-row>
        </#if>
        <#if orderVATTaxTotal != 0>
            <fo:table-row height="8mm" line-height="8mm">
                <fo:table-cell number-columns-spanned="4">
                    <fo:block text-align="right" font-weight="bold">${uiLabelMap.OrderSalesTaxIncluded}:</fo:block>
                </fo:table-cell>
                <fo:table-cell text-align="right" border-top-style="solid" border-top-width="thin" border-top-color="black">
                    <fo:block><@ofbizCurrency amount=orderVATTaxTotal isoCode=currencyUomId/></fo:block>
                </fo:table-cell>
            </fo:table-row>
        </#if>

        <#-- the grand total -->
        <#if grandTotal != 0>
            <fo:table-row>
               <fo:table-cell number-columns-spanned="2">
                  <fo:block/>
               </fo:table-cell>
               <fo:table-cell number-columns-spanned="2" padding-before="5pt" padding-after="5pt" font-size="13pt">
                  <fo:block text-align="right" font-weight="bold">${uiLabelMap.OrderTotalDue}:</fo:block>
               </fo:table-cell>
               <fo:table-cell text-align="right" border-top-style="double" border-top-width="thick" border-top-color="black" padding-before="5pt" padding-after="5pt" font-size="13pt">
                  <fo:block><@ofbizCurrency amount=grandTotal isoCode=(currencyUomId!)/></fo:block>
               </fo:table-cell>
            </fo:table-row>
            <fo:table-row height="7px">
               <fo:table-cell number-columns-spanned="5">
                  <fo:block/>
               </fo:table-cell>
            </fo:table-row>
        </#if>
        
    </fo:table-body>
 </fo:table>
</#escape>