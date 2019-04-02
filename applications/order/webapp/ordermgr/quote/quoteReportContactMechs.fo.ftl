<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
    <fo:block content-width="85mm" font-size="10pt" margin-top="45mm" margin-bottom="5mm">
        <fo:block-container height="5mm" font-size="6pt">
            <fo:block>
                <#-- Return Address -->
                ${companyName!""}
            </fo:block>
        </fo:block-container>
        <fo:block margin-bottom="2mm">
            <fo:table border-spacing="3pt">
                <fo:table-column column-width="3.75in"/>
                <fo:table-column column-width="3.75in"/>
                <fo:table-body>
                    <fo:table-row>
                        <fo:table-cell>
                            <fo:block>
                                <fo:block font-weight="bold">${uiLabelMap.OrderAddress}: </fo:block>
                                <#if quote.partyId?has_content>
                                    <#assign getPartyNameForDateCtx = {"userLogin":userLogin}>
                                    <#if quote.partyId?has_content>
                                        <#assign getPartyNameForDateCtx += {"partyId":quote.partyId}>
                                    </#if>
                                    <#if quote.issueDate?has_content>
                                        <#assign getPartyNameForDateCtx += {"compareDate":quote.issueDate?date}>
                                    </#if>
                                    <#assign quotePartyNameResult = runService("getPartyNameForDate", getPartyNameForDateCtx)/>
                                    <fo:block>${quotePartyNameResult.fullName?default("[${uiLabelMap.OrderPartyNameNotFound}]")}</fo:block>
                                <#else>
                                    <fo:block>[${uiLabelMap.OrderPartyNameNotFound}]</fo:block>
                                </#if>
                            </fo:block>
                        </fo:table-cell>
                    </fo:table-row>
                    <fo:table-row>
                        <fo:table-cell>
                            <fo:block>
                                <#if toPostalAddress??>
                                    <#assign dummy = setContextField("postalAddress", toPostalAddress)>
                                    <@render resource="component://party/widget/partymgr/PartyScreens.xml#postalAddressPdfFormatter" />
                                </#if>
                            </fo:block>
                        </fo:table-cell>
                    </fo:table-row>
                </fo:table-body>
            </fo:table>
        </fo:block>
    </fo:block>
</#escape>
