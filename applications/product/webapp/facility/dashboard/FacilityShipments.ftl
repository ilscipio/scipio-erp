<@section title=sectionTitle>
    <#if shipmentList?has_content>
            <@paginate mode="content" url=makeOfbizUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
                <@table type="data-list" role="grid" autoAltRows=true id="${currentShipmentType.shipmentTypeId}_shipmments">
                    <@thead>
                        <@tr valign="bottom" class="header-row">
                            <@th>${uiLabelMap.CommonId}</@th>
                            <@th>${uiLabelMap.ProductOriginFacility}</@th>
                            <@th>${uiLabelMap.CommonType}</@th>
                            <@th>${uiLabelMap.CommonStatus}</@th>
                            <@th>${uiLabelMap.CommonDate}</@th>
                        </@tr>
                    </@thead>
                    <@tbody>
                        <#list shipmentList as shipment>
                            <#assign shipmentType = shipment.getRelatedOne("ShipmentType", true) />
                            <#assign statusItem = shipment.getRelatedOne("StatusItem", true) /> 
                            <#if shipment.originFacilityId?has_content>
                                <#assign facility = shipment.getRelatedOne("OriginFacility", false) />
                                <#assign facilityName = facility.facilityName />
                            </#if>
                            <@tr>   
                                <@td><a href="<@ofbizUrl>EditShipment?shipmentId=${shipment.shipmentId!}</@ofbizUrl>">${shipment.shipmentId!}</a></@td>
                                <@td>${facilityName!}</@td>
                                <@td>${shipmentType.description!}</@td>
                                <@td>${statusItem.description!}</@td>
                                <@td>${shipment.createdDate?string('yyyy-MM-dd HH:mm')!}</@td>
                            </@tr>
                        </#list>
                    </@tbody>
                </@table>
                <script>
                    $(document).ready(function() {        
                        var table = $('#${currentShipmentType.shipmentTypeId}_shipmments').DataTable();
                        //table.order( [[ 5, 'desc' ]] ).draw();
                    } );
                </script>
            </@paginate>
     
    <#else>
        <@commonMsg type="result-norecord"/>            
    </#if>
</@section>