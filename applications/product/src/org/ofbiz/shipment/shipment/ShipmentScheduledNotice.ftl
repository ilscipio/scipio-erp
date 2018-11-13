<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<html>
    <head>
    </head>
    <body>
        <div>Shipment #${shipment.shipmentId} <#if shipment.primaryOrderId?has_content>(from primary Order #${shipment.primaryOrderId})</#if> has been scheduled for shipment.</div>
        <div>The Estimated Ship Date is: ${(shipment.estimatedShipDate)!"Unknown"}</div>
        <div>The Estimated Arrival Date is: ${(shipment.estimatedArrivalDate)!"Unknown"}</div>
    </body>
</html>
