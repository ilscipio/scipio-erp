<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
function toggle(e) {
    e.checked = !e.checked;
}
function checkToggle(e) {
    var cform = document.cartform;
    if (e.checked) {
        var len = cform.elements.length;
        var allchecked = true;
        for (var i = 0; i < len; i++) {
            var element = cform.elements[i];
            if (element.name == "selectedItem" && !element.checked) {
                allchecked = false;
            }
            cform.selectAll.checked = allchecked;
        }
    } else {
        cform.selectAll.checked = false;
    }
}
function toggleAll() {
    var cform = document.cartform;
    var len = cform.elements.length;
    for (var i = 0; i < len; i++) {
        var e = cform.elements[i];
        if (e.name == "selectedItem") {
            toggle(e);
        }
    }
}
function removeSelected() {
    var cform = document.cartform;
    cform.removeSelected.value = true;
    cform.submit();
}
function addToList() {
    var cform = document.cartform;
    cform.action = "<@ofbizUrl>addBulkToShoppingList</@ofbizUrl>";
    cform.submit();
}
function gwAll(e) {
    var cform = document.cartform;
    var len = cform.elements.length;
    var selectedValue = e.value;
    if (selectedValue == "") {
        return;
    }

    var cartSize = ${shoppingCartSize};
    var passed = 0;
    for (var i = 0; i < len; i++) {
        var element = cform.elements[i];
        var ename = element.name;
        var sname = ename.substring(0,16);
        if (sname == "option^GIFT_WRAP") {
            var options = element.options;
            var olen = options.length;
            var matching = -1;
            for (var x = 0; x < olen; x++) {
                var thisValue = element.options[x].value;
                if (thisValue == selectedValue) {
                    element.selectedIndex = x;
                    passed++;
                }
            }
        }
    }
    if (cartSize > passed && selectedValue != "NO^") {
        showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}","${escapeVal(uiLabelMap.OrderSelectedGiftNotAvailableForAll, 'js')}");
    }
    cform.submit();
}
<#-- SCIPIO: To be removed -->
<#-- function quicklookup_popup(element) {
    target = element;  // note: global var target comes from fieldlookup.js
    var searchTerm = element.value;
    var obj_lookupwindow = window.open('LookupProduct?productId_op=like&amp;productId_ic=Y&amp;productId=' + searchTerm,'FieldLookup', 'width=700,height=550,scrollbars=yes,status=no,resizable=yes,top='+my+',left='+mx+',dependent=yes,alwaysRaised=yes');
    obj_lookupwindow.opener = window;
    obj_lookupwindow.focus();
}
function quicklookup(element) {
    <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
    window.location='<@ofbizUrl>LookupBulkAddSupplierProducts</@ofbizUrl>?productId='+element.value;
    <#else>
    window.location='<@ofbizUrl>LookupBulkAddProducts</@ofbizUrl>?productId='+element.value;
    </#if>
}-->
</@script>
