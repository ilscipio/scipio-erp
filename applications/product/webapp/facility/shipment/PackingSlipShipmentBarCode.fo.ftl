<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#escape x as x?xml>
            <fo:block>
                <fo:instream-foreign-object>
                    <barcode:barcode xmlns:barcode="http://barcode4j.krysalis.org/ns"
                            message="${shipment.shipmentId}">
                        <barcode:code39>
                            <barcode:height>8mm</barcode:height>
                        </barcode:code39>
                    </barcode:barcode>
                </fo:instream-foreign-object>
            </fo:block>
</#escape>
