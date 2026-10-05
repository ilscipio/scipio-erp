/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */

jQuery(document).ready(isValidElement);

function isValidElement(element){
    jQuery('#quickAnonProcessCustomer').validate(); 
 }

jQuery(document).ready(function() {
    jQuery('#useShippingPostalAddressForBilling').click(changeText2);
});
function changeText2(){
    if(document.getElementById('useShippingPostalAddressForBilling').checked) {
        document.getElementById('billToName').value = document.getElementById('shipToName').value;
        document.getElementById('billToAttnName').value = document.getElementById('shipToAttnName').value;
        document.getElementById('billToAddress1').value = document.getElementById('shipToAddress1').value;
        document.getElementById('billToAddress2').value = document.getElementById('shipToAddress2').value;
        document.getElementById('billToCity').value = document.getElementById('shipToCity').value;
        document.getElementById('billToStateProvinceGeoId').value = document.getElementById('shipToStateProvinceGeoId').value;
        document.getElementById('billToPostalCode').value = document.getElementById('shipToPostalCode').value;
        document.getElementById('billToCountryGeoId').value = document.getElementById('shipToCountryGeoId').value;
        document.getElementById('billToName').disabled = true;
        document.getElementById('billToAttnName').disabled = true;
        document.getElementById('billToAddress1').disabled = true;
        document.getElementById('billToAddress2').disabled = true;
        document.getElementById('billToCity').disabled = true;
        document.getElementById('billToStateProvinceGeoId').disabled = true;
        document.getElementById('billToPostalCode').disabled = true;
        document.getElementById('billToCountryGeoId').disabled = true;
    } else {
        document.getElementById('billToName').disabled = false;
        document.getElementById('billToAttnName').disabled = false;
        document.getElementById('billToAddress1').disabled = false;
        document.getElementById('billToAddress2').disabled = false;
        document.getElementById('billToCity').disabled = false;
        document.getElementById('billToStateProvinceGeoId').disabled = false;
        document.getElementById('billToPostalCode').disabled = false;
        document.getElementById('billToCountryGeoId').disabled = false;
    }
}