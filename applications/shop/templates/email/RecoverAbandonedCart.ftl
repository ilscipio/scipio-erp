<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- SCIPIO Email template  -->

    
    <#-- BODY -->

                                
    <#-- custom logo or text can be inserted here -->
    <h1>${title!}</h1>
    <#if !isDemoStore?? || isDemoStore><p>${uiLabelMap.OrderDemoFrontNote}.</p></#if>
    <#if note??><p>${note}</p></#if>
    <br />
    <@render resource="component://shop/widget/EmailOrderScreens.xml#emailcart" />


                                

