<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
<#import "component://content/webapp/content/common/contentlib.ftl" as contentlib>

    <#if invoice?has_content><fo:block font-size="16pt" font-weight="bold" margin-bottom="5mm">${invoice.getRelatedOne("InvoiceType", false).get("description",locale)}</fo:block></#if>

    <#-- list of orders -->
    <#if orders?has_content>
    <fo:table table-layout="fixed" width="100%">
        <fo:table-column column-width="30mm"/>
        <fo:table-column/>

        <fo:table-body>
          <fo:table-row>
            <fo:table-cell>
              <fo:block font-weight="bold">${uiLabelMap.AccountingOrderNr}:</fo:block>
            </fo:table-cell>
            <fo:table-cell>
              <fo:block><#list orders as order>${order} </#list></fo:block>
            </fo:table-cell>
          </fo:table-row>
        </fo:table-body>
    </fo:table>
    </#if>

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
          <fo:block font-weight="bold">${uiLabelMap.AccountingProduct}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold">${uiLabelMap.CommonDescription}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold" text-align="right">${uiLabelMap.CommonQty}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold" text-align="right">${uiLabelMap.AccountingUnitPrice}</fo:block>
        </fo:table-cell>
        <fo:table-cell>
          <fo:block font-weight="bold" text-align="right"></fo:block>
        </fo:table-cell>
      </fo:table-row>
    </fo:table-header>

    <#-- SCIPIO: OrderItemAttributes and ProductConfigWrappers -->
    <#assign orhCache = orhCache!Static["org.ofbiz.order.order.OrderReadHelper$Cache"].create(dispatcher, locale)>
    <#macro invoiceItemAttrInfo invoiceItem showCfgOpt=true showItemAttr=true>
      <#local productId = raw(invoiceItem.productId!)>
      <#local orh = false>
      <#if showCfgOpt>
        <#local cfgWrp = false>
        <#if productCfgMap??>
          <#local cfgWrp = (productCfgMap[productId])!false>
        <#else>
          <#local product = invoiceItem.getRelatedOne("Product")!>
          <#if Static["org.ofbiz.product.product.ProductWorker"].isConfigProductConfig(product)>
            <#if orderItemInfo?is_boolean>
              <#assign orderItemInfo = Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceItemOrderItemInfo(delegator,
                invoiceItem.invoiceId, invoiceItem.invoiceItemSeqId)!>
            </#if>
            <#if orderItemInfo.orderItemSeqId??>
              <#local orderId = raw(orderItemInfo.orderId)>
              <#local orderItemSeqId = raw(orderItemInfo.orderItemSeqId)>
              <#local orh = orhCache[orderId]!>
              <#local cfgWrp = (orh.getProductConfigWrapperForOrderItem(orderItemSeqId))!false>
            </#if>
          </#if>
        </#if>
        <#if !cfgWrp?is_boolean>
          <#local selectedOptions = cfgWrp.getSelectedOptions()! />
          <#if selectedOptions?has_content>
            <fo:table-row height="8mm" line-height="8mm">
              <fo:table-cell number-columns-spanned="5">
                <fo:block text-align="left" font-size="8pt">
                  <fo:list-block line-height="10pt" start-indent="2mm" provisional-distance-between-starts="3mm" provisional-label-separation="1mm">
                    <#list selectedOptions as option>
                      <fo:list-item>
                        <fo:list-item-label end-indent="label-end()"><fo:block><fo:inline font-family="Symbol">&#x2022;</fo:inline></fo:block></fo:list-item-label>
                        <fo:list-item-body start-indent="body-start()"><fo:block>${option.getDescription()}</fo:block></fo:list-item-body>
                      </fo:list-item>
                    </#list>
                  </fo:list-block>
                </fo:block>
              </fo:table-cell>
            </fo:table-row>
          </#if>
        </#if>
      </#if>
      <#if showItemAttr>
        <#if invoiceItemAttrMap??>
          <#local orderItemAttributes = invoiceItemAttrMap[raw(invoiceItem.invoiceItemSeqId!)]!/>
        <#else>
          <#if orderItemInfo?is_boolean>
            <#assign orderItemInfo = Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceItemOrderItemInfo(delegator,
                invoiceItem.invoiceId, invoiceItem.invoiceItemSeqId)!>
          </#if>
          <#if orderItemInfo.orderItemSeqId??>
            <#local orderId = raw(orderItemInfo.orderId)>
            <#local orderItemSeqId = raw(orderItemInfo.orderItemSeqId)>
            <#local orderItemAttributes = delegator.from("OrderItemAttribute").where("orderId", orderId, "orderItemSeqId", orderItemSeqId).queryList()/>
          <#else>
            <#local orderItemAttributes = {}>
          </#if>
        </#if>
        <#if orderItemAttributes?has_content>
            <fo:table-row height="8mm" line-height="8mm">
              <fo:table-cell number-columns-spanned="5">
                <fo:block text-align="left" font-size="8pt">
                  <fo:list-block line-height="10pt" start-indent="2mm" provisional-distance-between-starts="3mm" provisional-label-separation="1mm">
                    <#list orderItemAttributes as orderItemAttribute>
                      <fo:list-item>
                        <fo:list-item-label end-indent="label-end()"><fo:block><fo:inline font-family="Symbol">&#x2022;</fo:inline></fo:block></fo:list-item-label>
                        <fo:list-item-body start-indent="body-start()"><fo:block>${orderItemAttribute.attrName} : ${orderItemAttribute.attrValue}</fo:block></fo:list-item-body>
                      </fo:list-item>
                    </#list>
                  </fo:list-block>
                </fo:block>
              </fo:table-cell>
            </fo:table-row>
        </#if>
      </#if>
    </#macro>

    <#-- SCIPIO: Based on orderlib macro -->
    <#macro invoiceItemSurvResList survResList srqaArgs={} useTitleLine=false interactive=false maxInline=-1 class="" listClass="">
      <#if survResList?has_content>
      <#local class = addClassArgDefault(class, "order-item-survres-list")>
        <#list survResList as surveyResponse>
            <fo:table-row height="8mm" line-height="8mm">
              <fo:table-cell number-columns-spanned="5">
                <fo:block text-align="left" font-size="8pt">
            <#local survey = surveyResponse.getRelatedOne("Survey")!>
            <#if useTitleLine>
              <#local surveyDesc = survey.get("description", locale)!>
              <#if surveyDesc?has_content>${surveyDesc}</#if>
            </#if>
            <#if (maxInline != 0) && ("Y" == survey.showOnInvoice!)>
              <@contentlib.renderSurveyResponse surveyResponse=surveyResponse tmplLoc="component://content/template/survey/qalistresult.fo.ftl"
                srqaArgs=({"listClass":listClass, "max":maxInline} + srqaArgs)/>
            </#if>
                </fo:block>
              </fo:table-cell>
            </fo:table-row>
        </#list>
      </#if>
    </#macro>
    <#function getInvoiceItemSurvResList invoiceItem>
      <#if orderItemInfo?is_boolean>
        <#assign orderItemInfo = Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceItemOrderItemInfo(delegator,
            invoiceItem.invoiceId, invoiceItem.invoiceItemSeqId)!>
      </#if>
      <#if orderItemInfo.orderItemSeqId??>
        <#local orderId = raw(orderItemInfo.orderId)>
        <#local orderItemSeqId = raw(orderItemInfo.orderItemSeqId)>
        <#return delegator.from("SurveyResponse").where("orderId", orderId, "orderItemSeqId", orderItemSeqId).orderBy("orderItemSeqId").queryList()><#-- This less accurately reproduces cart order: .orderBy("-responseDate") -->
      </#if>
    </#function>

    <fo:table-body font-size="10pt" table-layout="fixed" width="100%">
        <#assign currentShipmentId = "">
        <#assign newShipmentId = "">
        <#-- if the item has a description, then use its description.  Otherwise, use the description of the invoiceItemType -->
        <#list invoiceItems as invoiceItem>
            <#assign itemType = invoiceItem.getRelatedOne("InvoiceItemType", false)>
            <#assign isItemAdjustment = Static["org.ofbiz.entity.util.EntityTypeUtil"].hasParentType(delegator, "InvoiceItemType", "invoiceItemTypeId", itemType.getString("invoiceItemTypeId"), "parentTypeId", "INVOICE_ADJ")/>

            <#assign taxRate = invoiceItem.getRelatedOne("TaxAuthorityRateProduct", false)!>
            <#assign itemBillings = invoiceItem.getRelated("OrderItemBilling", null, null, false)!>
            <#if itemBillings?has_content>
                <#assign itemBilling = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(itemBillings)>
                <#if itemBilling?has_content>
                    <#assign itemIssuance = itemBilling.getRelatedOne("ItemIssuance", false)!>
                    <#if itemIssuance?has_content>
                        <#assign newShipmentId = itemIssuance.shipmentId>
                        <#assign issuedDateTime = itemIssuance.issuedDateTime/>
                    </#if>
                </#if>
            </#if>
            <#if invoiceItem.description?has_content>
                <#assign description=invoiceItem.description>
            <#elseif taxRate?has_content & taxRate.get("description",locale)?has_content>
                <#assign description=taxRate.get("description",locale)>
            <#elseif itemType.get("description",locale)?has_content>
                <#assign description=itemType.get("description",locale)>
            </#if>

            <#if newShipmentId?? & newShipmentId != currentShipmentId>
                <#-- the shipment id is printed at the beginning for each
                     group of invoice items created for the same shipment
                -->
                <fo:table-row height="10mm" line-height="10mm">
                   <fo:table-cell number-columns-spanned="5" padding-before="3pt" padding-after="3pt" display-align="after" border-bottom-style="solid" border-bottom-width="thin" border-bottom-color="#666">
                        <fo:block font-weight="bold" color="#666"> ${uiLabelMap.ProductShipmentId}: ${newShipmentId}<#if issuedDateTime??> ${uiLabelMap.CommonDate}: ${issuedDateTime?date}</#if></fo:block>
                   </fo:table-cell>
                </fo:table-row>
                <#assign currentShipmentId = newShipmentId>
            </#if>
            <#-- SCIPIO: productIdMarkup (improved) -->
            <#if invoiceItem.productId?has_content>
              <#assign origProductId = Static["org.ofbiz.product.product.ProductWorker"].getMainProductId(delegator, invoiceItem.productId, false)!"">
              <#assign productIdMarkup>${invoiceItem.productId}<#if origProductId?has_content> (${origProductId})</#if></#assign>
            <#else>
              <#assign productIdMarkup></#assign>
            </#if>
            <#if !isItemAdjustment>
                <fo:table-row height="8mm" line-height="8mm">
                    <fo:table-cell>
                        <fo:block text-align="left">${productIdMarkup}</fo:block>
                    </fo:table-cell>
                    <fo:table-cell>
                        <fo:block text-align="left">${description!}</fo:block>
                    </fo:table-cell>
                      <fo:table-cell>
                        <fo:block text-align="right"> <#if invoiceItem.quantity??>${invoiceItem.quantity?string.number}</#if> </fo:block>
                    </fo:table-cell>
                    <fo:table-cell text-align="right">
                        <fo:block> <#if invoiceItem.quantity??><@ofbizCurrency amount=(invoiceItem.amount!) isoCode=(invoice.currencyUomId!)/></#if> </fo:block>
                    </fo:table-cell>
                    <fo:table-cell text-align="right">
                        <fo:block> <@ofbizCurrency amount=(Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceItemTotal(invoiceItem)) isoCode=(invoice.currencyUomId!)/> </fo:block>
                    </fo:table-cell>
                </fo:table-row>

                <#-- SCIPIO: NOTE: You may (un)comment or modify this call to control the verbosity -->
                <#assign orderItemInfo = false>
                <@invoiceItemAttrInfo invoiceItem=invoiceItem showCfgOpt=true showItemAttr=true/>
                <#-- SCIPIO: show application survey response QA list for this item -->
                <@invoiceItemSurvResList survResList=(getInvoiceItemSurvResList(invoiceItem)!)/>
            <#else>
                <#if !(invoiceItem.parentInvoiceId?? && invoiceItem.parentInvoiceItemSeqId??)>
                    <fo:table-row>
                        <fo:table-cell><fo:block/></fo:table-cell>
                        <fo:table-cell><fo:block/></fo:table-cell>
                        <fo:table-cell number-columns-spanned="3"><fo:block/></fo:table-cell>
                    </fo:table-row>
                </#if>
                <fo:table-row height="8mm" line-height="8mm">
                     <fo:table-cell>
                        <fo:block text-align="left">${productIdMarkup}</fo:block>
                    </fo:table-cell>
                    <fo:table-cell>
                        <fo:block>${description!}</fo:block>
                    </fo:table-cell>
                    <fo:table-cell text-align="right" number-columns-spanned="3">
                        <fo:block> <@ofbizCurrency amount=(Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceItemTotal(invoiceItem)) isoCode=(invoice.currencyUomId!)/> </fo:block>
                    </fo:table-cell>
                </fo:table-row>
            </#if>
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
              <fo:block>${uiLabelMap.AccountingTotalExclTax}</fo:block>
           </fo:table-cell>
           <fo:table-cell text-align="right" border-top-style="solid" border-top-width="thin" border-top-color="black" padding-before="3pt" padding-after="3pt">
              <fo:block>
                 <@ofbizCurrency amount=invoiceNoTaxTotal isoCode=(invoice.currencyUomId!)/>
              </fo:block>
           </fo:table-cell>
        </fo:table-row>
        
        <#if vatIncludedByRate?has_content>
            <#list vatIncludedByRate.entrySet() as vatInc>  
                <fo:table-row height="8mm" line-height="8mm">
                   <fo:table-cell number-columns-spanned="4" text-align="right" padding-before="3pt" padding-after="3pt">
                      <fo:block>${uiLabelMap.AccountingSalesTaxIncluded} (${(vatInc.key/100)?string.percent})</fo:block>
                   </fo:table-cell>
                   <fo:table-cell text-align="right" border-top-style="solid" border-top-width="thin" border-top-color="black" padding-before="3pt" padding-after="3pt">
                      <fo:block>
                         <@ofbizCurrency amount=vatInc.value isoCode=(invoice.currencyUomId!)/>
                      </fo:block>
                   </fo:table-cell>
                </fo:table-row>
            </#list>
        </#if>

        <#-- the grand total -->
        <fo:table-row>
           <fo:table-cell number-columns-spanned="2">
              <fo:block/>
           </fo:table-cell>
           <fo:table-cell number-columns-spanned="2" padding-before="5pt" padding-after="5pt" font-size="13pt">
              <fo:block font-weight="bold">${uiLabelMap.FormFieldTitle_invoiceAmount}</fo:block>
           </fo:table-cell>
           <fo:table-cell text-align="right" border-top-style="double" border-top-width="thick" border-top-color="black" padding-before="5pt" padding-after="5pt" font-size="13pt">
              <fo:block><@ofbizCurrency amount=invoiceTotal isoCode=(invoice.currencyUomId!)/></fo:block>
           </fo:table-cell>
        </fo:table-row>
        <fo:table-row height="7px">
           <fo:table-cell number-columns-spanned="5">
              <fo:block/>
           </fo:table-cell>
        </fo:table-row>
        
    </fo:table-body>
 </fo:table>

<#if vatTaxIds?has_content>
 <fo:table table-layout="fixed" width="100%">
    <fo:table-column column-width="105mm"/>
    <fo:table-column column-width="40mm"/>
    <fo:table-column/>

    <fo:table-header>
      <fo:table-row>
        <fo:table-cell>
          <fo:block/>
        </fo:table-cell>
        <fo:table-cell border-top-style="solid" border-top-width="thin" border-top-color="black" padding-before="3pt" padding-after="3pt">
          <fo:block font-weight="bold">${uiLabelMap.AccountingVat}</fo:block>
        </fo:table-cell>
        <fo:table-cell text-align="right" border-top-style="solid" border-top-width="thin" border-top-color="black" padding-before="3pt" padding-after="3pt">
          <fo:block font-weight="bold">${uiLabelMap.AccountingAmount}</fo:block>
        </fo:table-cell>
      </fo:table-row>
    </fo:table-header>

    <fo:table-body font-size="10pt">

    <#list vatTaxIds as vatTaxId>
    <#assign taxRate = delegator.findOne("TaxAuthorityRateProduct", {"taxAuthorityRateSeqId":vatTaxId}, true)/>
    <fo:table-row>
        <fo:table-cell>
          <fo:block/>
        </fo:table-cell>
        <fo:table-cell number-columns-spanned="1" padding-before="3pt" padding-after="3pt">
            <fo:block>${taxRate.description!}</fo:block>
        </fo:table-cell>
        <fo:table-cell number-columns-spanned="1" text-align="right" padding-before="3pt" padding-after="3pt">
            <fo:block font-weight="bold"><@ofbizCurrency amount=vatTaxesByType[vatTaxId] isoCode=(invoice.currencyUomId!)/></fo:block>
        </fo:table-cell>
    </fo:table-row>
    </#list>
    </fo:table-body>
 </fo:table>
</#if>

 <#-- a block with the invoice message-->
 <#if invoice.invoiceMessage?has_content><fo:block>${invoice.invoiceMessage}</fo:block></#if>
 <fo:block></fo:block>
</#escape>