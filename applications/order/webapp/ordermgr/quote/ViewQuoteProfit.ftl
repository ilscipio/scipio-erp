<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<span>${uiLabelMap.OrderOrderQuoteCoefficients}</span>
<#list quoteCoefficients as quoteCoefficient>
    <div>${quoteCoefficient.coeffName}:&nbsp;${quoteCoefficient.coeffValue}</div>
</#list>
<br />
<div><span>${uiLabelMap.CommonTotalCostMult}</span>&nbsp;${costMult}</div>
<div><span>${uiLabelMap.CommonTotalCostToPriceMult}</span>&nbsp;${costToPriceMult}</div>
<br />
<div><span>${uiLabelMap.CommonTotalCost}</span>&nbsp;<@ofbizCurrency amount=totalCost isoCode=quote.currencyUomId/></div>
<div><span>${uiLabelMap.CommonTotalAmount}</span>&nbsp;<@ofbizCurrency amount=totalPrice isoCode=quote.currencyUomId/></div>
<br />
<div><span>${uiLabelMap.CommonTotalProfit}</span>&nbsp;<@ofbizCurrency amount=totalProfit isoCode=quote.currencyUomId/></div>
<div><span>${uiLabelMap.CommonTotalPercProfit}</span>&nbsp;${totalPercProfit}%</div>
<br />