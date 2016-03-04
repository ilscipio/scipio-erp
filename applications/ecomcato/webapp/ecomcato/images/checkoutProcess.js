/*
 * TODO: replace classes with ids, this is really not ideal
 * */

var isCartStepValidate 
var isCartStepValidate = false;
var isShipStepValidate = false;
var isShipOptionStepValidate = false;
var isBillStepValidate = false;
var isUserAnonymous = false;

jQuery(document).ready(function() {
	
		// Cart, user

		var validateCart = jQuery("#cartForm");
		validateCart.validate();

		/* Forget the user form for now
		var validateUser = jQuery("#userForm");
		validateUser.validate(); */
		
		var validateLogin = jQuery("#loginForm");
		validateLogin.validate();
		
		
		// Shipping, billing
		
		var validateShip = jQuery("#shippingForm");
	    validateShip.validate();

	    var validateShipOption = jQuery("#shippingOptionForm");
	    validateShipOption.validate();

	    var validateBill = jQuery("#billingForm");
	    validateBill.validate();

		var validateOrderSubmitForm = jQuery("#orderSubmitForm");
		validateOrderSubmitForm.validate();

		
		// Cart, user
		
		// Goto Edit Cart Panel
		jQuery('.onepagecheckout-cart-edit-button').click(function() {
			showEditCartPanel();
		});

		// Goto Edit User panel
		jQuery('.onepagecheckout-cart-next-button').click(function() {
			if (validateCart.valid()) {
				//showEditUserPanel();
				showEditShippingPanel();
			}
		});

		/*
		// Goto Edit user panel
		jQuery('.onepagecheckout-user-edit-button').click(function() {
			showEditUserPanel();

		});
		*/

		/*
		// Goto Process Order panel
		jQuery('.onepagecheckout-user-next-button').click(function() {
			if (!isUserAnonymous) {
				if (jQuery('.onepagecheckout-user-edit-form').is(":visible") && validateUser.valid()) {
					if (createUpdateCustomer()) 
						showEditShippingPanel();					
				} else if (jQuery('.onepagecheckout-user-login-form').is(":visible") && validateLogin.valid()) {
					if (loginUser())
						showEditShippingPanel();
				}
			} else {
				if (createUpdateCustomer())
					showEditShippingPanel();
			}
		});
		*/

		
		// Shipping, billing
		
	    // Update Shipping Address
	    jQuery('.onepagecheckout-shipping-next-button').click(function() {
	        if (validateShip.valid()) {
	            jQuery('.onepagecheckout-shipping-next-button').fadeOut('fast');
	            //jQuery('#processingShippingOptions').fadeIn('fast');
	            if (createUpdateCustomerAndShippingAddress()){
	                showEditShippingOptionPanel();
	            }
	            //jQuery('#processingShippingOptions').fadeOut('fast');
	            jQuery('.onepagecheckout-shipping-next-button').fadeIn('fast');
	        }
	    });

	    // Goto Edit Shipping Panel
	    jQuery('.onepagecheckout-shipping-edit-button').click(function() {
	        showEditShippingPanel();
	        setShippingOption();
	    });

	    // Set Shipping Method to card and goto Billing step
	    jQuery('.onepagecheckout-shippingOption-next-button').click(function() {
	        jQuery('.onepagecheckout-shippingOption-next-button').fadeOut('fast');
	        //jQuery('#processingBilling').fadeIn('fast');
	        if (setShippingOption()){
	            showEditBillingPanel();
	        }
	        //jQuery('#processingBilling').fadeOut('fast');
	        jQuery('.onepagecheckout-shippingOption-next-button').fadeIn('fast');
	    });

	    jQuery('.onepagecheckout-shippingOption-edit-button').click(function() {
	        showEditShippingOptionPanel();
	        updateBillingSummary();
	    });

	    // Billing
	    jQuery('.onepagecheckout-billing-edit-button').click(function() {
	        showEditBillingPanel();
	    });

	    jQuery('.onepagecheckout-billing-next-button').click(function() {
	        if (validateBill.valid()) {
	            jQuery('.onepagecheckout-billing-next-button').fadeOut('fast');
	            //jQuery('#processingOrderSubmitPanel').fadeIn('fast');
	            if (processBillingAndPayment()) {
	                showOrderSubmitPanel();
	            }
	            //jQuery('#processingOrderSubmitPanel').fadeOut('fast');
	            jQuery('.onepagecheckout-billing-next-button').fadeIn('fast');
	        }
	    });

	    // For Billing Address Same As Shipping
	    jQuery('#useShippingAddressForBilling').click(function() {
	        useShippingAddressForBillingToggle();
	        validateBill.valid();
	    });
		

		// Initiate Observing Edit Cart Events
		initCartProcessObservers();
		
			
		// Cart, user
		
		/*
		jQuery('#createUserButton').click(function() {
			jQuery('.onepagecheckout-user-edit-form').slideDown();
			jQuery('.onepagecheckout-user-edit-create-link').slideUp();
			isUserAnonymous = false;
		});

		jQuery('.annonymousUserButton').click(function() {
			jQuery('.onepagecheckout-user-edit-create-link').slideDown();
			jQuery('.onepagecheckout-user-edit-form').slideUp();
			jQuery('.onepagecheckout-user-login-form').slideUp();
			isUserAnonymous = true;
		});
		
		jQuery('#loginUserButton').click(function() {
			jQuery('.onepagecheckout-user-login-form').slideDown();
			jQuery('.onepagecheckout-user-edit-create-link').slideUp();
			isUserAnonymous = false;
		});
		*/

			
		// Shipping, billing
		
	    if (jQuery('#shippingForm').length) {
	        if (jQuery('#userLoginId').length) {
	        	var stateValue = jQuery('#shipToStateProvinceGeoId').val();
	            // Cato: Workaround needed because .val(stateValue) seems to have no effect
	            //getAssociatedStateList('shipToCountryGeoId', 'shipToStateProvinceGeoId', 'advice-required-shipToStateProvinceGeoId', 'shipToStates');
	            //jQuery('#shipToStateProvinceGeoId').val(stateValue);
	            getAssociatedStateListSelected('shipToCountryGeoId', 'shipToStateProvinceGeoId', 'advice-required-shipToStateProvinceGeoId', 'shipToStates', stateValue)
	            
	            stateValue = jQuery('#billToStateProvinceGeoId').val();
	            //getAssociatedStateList('billToCountryGeoId', 'billToStateProvinceGeoId', 'advice-required-billToStateProvinceGeoId', 'billToStates');
	            //jQuery('#billToStateProvinceGeoId').val(stateValue);
	            getAssociatedStateListSelected('billToCountryGeoId', 'billToStateProvinceGeoId', 'advice-required-billToStateProvinceGeoId', 'billToStates', stateValue)
	        } else {
	            getAssociatedStateList('shipToCountryGeoId', 'shipToStateProvinceGeoId', 'advice-required-shipToStateProvinceGeoId', 'shipToStates');
	            getAssociatedStateList('billToCountryGeoId', 'billToStateProvinceGeoId', 'advice-required-billToStateProvinceGeoId', 'billToStates');
	        }
	        // Get associate states for Shipping Information
	        jQuery('#shipToCountryGeoId').change(function(){
	            getAssociatedStateList('shipToCountryGeoId', 'shipToStateProvinceGeoId', 'advice-required-shipToStateProvinceGeoId', 'shipToStates');
	        });
	    }
	    if (jQuery('#billingForm').length) {
	        // Get associate states for Billing Information
	        jQuery('#billToCountryGeoId').change(function() {
	            getAssociatedStateList('billToCountryGeoId', 'billToStateProvinceGeoId', 'advice-required-billToStateProvinceGeoId', 'billToStates');
	        });
	    }
		
	    
	    // Process
	    	
		jQuery('#processOrderButton').click(function() {
			if (validateOrderSubmitForm.valid())
				processOrder();
		});
	    
	});

//Check server side error
function getServerError(data) {
    var serverErrorHash = [];
    var serverError = "";
    if (data._ERROR_MESSAGE_LIST_ != undefined) {
        serverErrorHash = data._ERROR_MESSAGE_LIST_;
        jQuery.each(serverErrorHash, function(i, error) {
            var encodedErrorMessage = jQuery('<div/>').text(error).html(); // error.message
            serverError += encodedErrorMessage + '<br/>';
        });
    }
    if (data._ERROR_MESSAGE_ != undefined) {
        serverError = jQuery('<div/>').text(data._ERROR_MESSAGE_).html();
    }
    return serverError;
}

// Begin Show/Hide Step panels

function hideEditCartPanel() {
	if (jQuery('.onepagecheckout-cart-edit').is(':visible')) {
		jQuery('.onepagecheckout-cart-edit').slideUp();
		jQuery('.onepagecheckout-cart-summary').slideDown();
	}
}
function hideEditUserPanel() {
	/*
	if (jQuery('.onepagecheckout-user-edit').is(':visible')) {
		jQuery('.onepagecheckout-user-edit').slideUp();
		jQuery('.onepagecheckout-user-summary').slideDown();
		if (isUserAnonymous) {
			jQuery('.onepagecheckout-existing-user-summary').slideUp();
			jQuery('.onepagecheckout-anonymous-user-summary').slideDown();			
		} else {
			jQuery('.onepagecheckout-anonymous-user-summary').slideUp();
			jQuery('.onepagecheckout-existing-user-summary').slideDown();
		}
	}
	*/
}
function hideEditShippingPanel() {
    if (jQuery('.onepagecheckout-shipping-edit').is(':visible')) {
        jQuery('.onepagecheckout-shipping-edit').slideUp();
        jQuery('.onepagecheckout-shipping-summary').slideDown();
    }
}
function hideEditShippingOptionPanel() {
    if (jQuery('.onepagecheckout-shippingOption-edit').is(':visible')) {
        jQuery('.onepagecheckout-shippingOption-edit').slideUp();
        jQuery('.onepagecheckout-shippingOption-summary').slideDown();
    }
}
function hideEditBillingPanel() {
   if (jQuery('.onepagecheckout-billing-edit').is(':visible')) {
       jQuery('.onepagecheckout-billing-edit').slideUp();
       jQuery('.onepagecheckout-billing-summary').slideDown();
   }
}
function hideOrderSubmitPanel() {
	if (jQuery('.onepagecheckout-submit-summary').is(':visible')) {
		jQuery('.onepagecheckout-submit-summary').slideUp();
		// Sometimes this doesn't work when going back: jQuery('#processingOrderButton').slideDown();

	}
}
function showEditCartPanel() {
	if (!jQuery('.onepagecheckout-cart-edit').is(':visible')) {
		jQuery('.onepagecheckout-cart-summary').slideUp();		
		hideEditUserPanel();
		hideEditShippingPanel();
        hideEditShippingOptionPanel();
        hideEditBillingPanel();
        hideOrderSubmitPanel();		
		jQuery('.onepagecheckout-cart-edit').slideDown();
	}
}
function showEditUserPanel() {
	if (!jQuery('.onepagecheckout-user-edit').is(':visible')) {
		jQuery('.onepagecheckout-user-summary').slideUp();
		hideEditCartPanel();
		hideEditShippingPanel();
        hideEditShippingOptionPanel();
        hideEditBillingPanel();
        hideOrderSubmitPanel();
		jQuery('.onepagecheckout-user-edit').slideDown();
	}
	if (jQuery('.onepagecheckout-user-edit-create-link').is(':visible'))
		isUserAnonymous = true;
}

function showEditShippingPanel() {
     if (!jQuery('.onepagecheckout-shipping-edit').is(':visible') ) {
         jQuery('.onepagecheckout-shipping-summary').slideUp();
         hideEditCartPanel();
         hideEditUserPanel();
         hideEditShippingOptionPanel();
         hideEditBillingPanel();
         hideOrderSubmitPanel();
         jQuery('.onepagecheckout-shipping-edit').slideDown();

     }
}

function showEditShippingOptionPanel() {
     if (!jQuery('.onepagecheckout-shippingOption-edit').is(':visible') ) {
         jQuery('.onepagecheckout-shippingOption-summary').slideUp();
         hideEditCartPanel();
         hideEditUserPanel();
         hideEditShippingPanel();
         hideEditBillingPanel();
         hideOrderSubmitPanel();
         jQuery('.onepagecheckout-shippingOption-edit').slideDown();
     }
}

function showEditBillingPanel() {
    if (!jQuery('.onepagecheckout-billing-edit').is(':visible') ) {
         jQuery('.onepagecheckout-billing-summary').slideUp();
         hideEditCartPanel();
         hideEditUserPanel();
         hideEditShippingPanel();
         hideEditShippingOptionPanel();
         hideOrderSubmitPanel();
         jQuery('.onepagecheckout-billing-edit').slideDown();
    }
    if (jQuery('#shipToContactMechId').val() != jQuery('#billToContactMechId').val()) {
        jQuery('#useShippingAddressForBilling').val('false');
        jQuery('#billingAddress').slideDown();
        jQuery('#useShippingAddressForBilling').val('N');
    }
}

function showOrderSubmitPanel() {
	if (!jQuery('.onepagecheckout-submit-summary').is(':visible')) {
		hideEditCartPanel();
		hideEditUserPanel();
		hideEditShippingPanel();
        hideEditShippingOptionPanel();
        hideEditBillingPanel();
		jQuery('.onepagecheckout-submit-summary').slideDown();
	}
}

// End Show/Hide Step panels

function createUpdateCustomer() {
	var result = false;
	jQuery('#isAnonymous').val(isUserAnonymous);
	jQuery.ajax( {
		url : 'createUpdateCustomer',
		type : 'POST',
		async : false,
		data : jQuery('#userForm').serialize(),
		success : function(json) {
			serverErrors = getServerError(json);
			if (serverErrors != "") {
				jQuery('#userFormServerError').html(serverErrors);
				result = false;
			} else {
				jQuery('#userFormServerError').fadeOut('fast');
				// Process User data response.				
				updateUserSummary(json);
				result = true;
			}
	},
	error : function(error) {
		if (error != "") {
			jQuery('#userFormServerError').html(serverError);
		}
		result = false;
	}
	});
	return result;
}

function loginUser() {
	var result = false;	
	jQuery.ajax( {
		url : 'onePageCheckoutLogin',
		type : 'POST',
		async : false,
		data : jQuery('#loginForm').serialize(),
		success : function(json) {			
			serverErrors = getServerError(json);
			if (serverErrors != "") {
				jQuery('#loginFormServerError').html(serverErrors);
				result = false;
			} else {
				jQuery('#loginFormServerError').fadeOut('fast');
				// Process User data response.				
				updateUserSummary(json);
				result = true;
			}
	},
	error : function(error) {
		if (error != "") {
			jQuery('#loginFormServerError').html(error);
		}
		result = false;
	}
	});
	return result;
}

function createUpdateCustomerAndShippingAddress() {
    var result = false;
    jQuery.ajax({
        url: 'createUpdateShippingAddress',
        type: 'POST',
        async: false,
        data: jQuery('#shippingForm').serialize(),
        success: function(json) {
                var serverError = getServerError(json);
                if (!serverError) {
                    jQuery('#shippingFormServerError').fadeOut('fast');
                    // Process Shipping data response.
                    jQuery('#shipToPartyId').val(json.partyId);
                    jQuery('#billToPartyId').val(json.partyId);
                    jQuery('#shipToContactMechId').val(json.contactMechId);
                    jQuery('#shipToPhoneContactMechId').val(json.shipToPhoneContactMechId);
                    jQuery('#emailContactMechId').val(json.emailContactMechId);
                    //jQuery('#completedShippingMethod').html(json.shippingDescription);
                    updateShippingSummary();
                    getShipOptions();
                    result = true;
                } else {
                    jQuery('#shippingFormServerError').html(serverError);
                    result = false;
                }
        },
        error: function(error) {
            if (error != "") {
                jQuery('#shippingFormServerError').html(error);
            }
            result = false;
        }
    });
    return result;
}

function getShipOptions() {
    var shipOptions = null;
    var optionList = [];
    var result = false;
    if (jQuery('#shipMethod').val() == "" || jQuery('#shipMethod').val() == null) {
        jQuery.ajax({
            url: 'getShipOptions',
            type: 'POST',
            async: false,
            success: function(json) {
                var serverError = getServerError(json);
                if (!serverError) {
                        jQuery('#shippingFormServerError').fadeOut('fast');
                        isShipStepValidate = true;
                        shipOptions = json.shippingOptions;
                        var shipMethod = jQuery('#shipMethod');
                        shipMethod.find("option").remove();
                        jQuery.each(shipOptions, function(shipOption) {
                            if (this.productStoreShipMethId){
                                shipMethod.append(jQuery("<option value = " + this.shippingMethod + ":" + this.productStoreShipMethId + " > " + this.shippingDesc  + " </option>"));
                            } else {
                                shipMethod.append(jQuery("<option value = " + this.shippingMethod + " > " + this.shippingDesc  + " </option>"));
                            }
                        });
                        result = true;
                    } else {
                        jQuery('#shippingFormServerError').html(serverError);
                        result = false;
                    }
            },
            error: function(error) {
                if (error != "") {
                    jQuery('#shippingFormServerError').fadeIn('fast');
                    jQuery('#shippingFormServerError').html(error);
                    isShipStepValidate = false;
                }
                result = false;
            }
        });
    }
    return result;
}

// Shipping option
function setShippingOption() {
    var shipTotal = null;
    var shipMethod = null;
    var result = false;
    jQuery('#shippingOptionFormServerError').fadeOut('fast');
    jQuery.ajax({
        url: 'setShippingOption',
        type: 'POST',
        async: false,
        data: jQuery('#shippingOptionForm').serialize(),
        success: function(json) {
            var serverError = getServerError(json);
            if (!serverError) {
            shipTotal = json.shippingTotal;
                isShipOptionStepValidate = true;
                jQuery('#selectedShipmentOption').html(json.shippingDescription);
                //jQuery('#shippingDescription').value = json.shippingDescription;
                //jQuery('#shippingTotal').val(json.shippingTotal);
                //jQuery('#cartGrandTotal').val(json.cartGrandTotal);
                //jQuery('#totalSalesTax').val(json.totalSalesTax);
                result = true;
            } else {
                jQuery('#shippingFormServerError').html(serverError);
                result = false;
            }
        },
        error: function(error) {
            if(error != "") {
                jQuery('#shippingOptionFormServerError').fadeIn('fast');
                jQuery('#shippingOptionFormServerError').html(error);
                isShipOptionStepValidate = false;
            }
            result = false;
        }
    });
    updateCartData();
    return result;
}
// Billing
function useShippingAddressForBillingToggle() {
    if (jQuery('#useShippingAddressForBilling').is(':checked') ) {
        jQuery('#billToAddress1').val(jQuery('#shipToAddress1').val());
        jQuery('#billToAddress2').val(jQuery('#shipToAddress2').val());
        jQuery('#billToCity').val(jQuery('#shipToCity').val());
        jQuery('#billToPostalCode').val(jQuery('#shipToPostalCode').val());
        jQuery('#billToCountryGeoId').val(jQuery('#shipToCountryGeoId').val());
        getAssociatedStateList('billToCountryGeoId', 'billToStateProvinceGeoId','advice-required-billToStateProvinceGeoId','billToStates');
        jQuery('#useShippingAddressForBilling').val("Y");
        jQuery('#billToStateProvinceGeoId').val(jQuery('#shipToStateProvinceGeoId').val());
        jQuery('#billingAddress').slideUp();
    } else {
        jQuery('#billingAddress').slideDown();
        jQuery('#useShippingAddressForBilling').val("N");
    }
}
function processBillingAndPayment() {
    var result = false;
    jQuery.ajax({
        url: 'createUpdateBillingAndPayment',
        type: 'POST',
        data: jQuery('#billingForm').serialize(),
        async: false,
        success: function(json) {
            var serverError = getServerError(json);
            if (!serverError) {
                    jQuery('#billingFormServerError').fadeOut('fast');
                    isBillStepValidate = true;
                    jQuery('#billToContactMechId').val(json.contactMechId);
                    jQuery('#paymentMethodId').val(json.paymentMethodId);
                    jQuery('#billToPhoneContactMechId').val(json.billToPhoneContactMechId);
                    updateBillingSummary();
                    result = true;
                } else {
                    jQuery('#billingFormServerError').html(serverError);
                    result = false;
                }
        },
        error: function(error) {
            if(error != "") {
                jQuery('#billingFormServerError').fadeIn('fast');
                jQuery('#billingFormServerError').html(error);
                isBillStepValidate = false;
            }
            result = false;
        }
    });
    return result;

}

function initCartProcessObservers() {
	var cartForm = jQuery('#cartForm');
	/*jQuery('#productPromoCode').change(function() {
		addPromoCode();
	});
	jQuery('.onepagecheckout-cart-next-button').click(function() {
		showEditShippingPanel();
	});
	jQuery('.onepagecheckout-cart-edit-button').click(function() {
		showEditCartPanel();
		updateShippingSummary();
	});
	var inputs = cartForm.find('input[type=text]');
	inputs.each(function(e) {
		if (this.id != 'productPromoCode' && this.id != undefined) {
			jQuery(this).change(function() {
				cartItemQtyChanged(this);
			});
		}
	});*/
	var links = jQuery("#cartForm a[id^='removeItemLink_']");
	jQuery.each(links, function() {
		jQuery(this).bind('click', function() {
			removeItem(this);
		});
	});
	/*
	 * if (jQuery('#initializedCompletedCartDiscount').length &&
	 * jQuery('#initializedCompletedCartDiscount').val() == 0) {
	 * jQuery('#completedCartDiscountRow').hide(); }
	 */
}

function addPromoCode() {
    jQuery.ajax({
        url: 'silentAddPromoCode',
        type: 'POST',
        data: {"productPromoCodeId" : jQuery('#productPromoCode').val()},
        success: function(json) {
            var serverError = getServerError(json);
            if (!serverError) {
                jQuery('#cartFormServerError').fadeOut('fast');
                updateCartData();
            } else {
                jQuery('#shippingFormServerError').html(serverError);
                result = false;
            }
        },
        error: function(error) {
            if(error != "") {
                jQuery('#cartFormServerError').fadeIn('fast');
                jQuery('#cartFormServerError').html(error);
            }
        }
    });
}

function getProductLineItemIndex(event, productId) {
    var itemIndex = null;
    var productIdParam = "productId=" + productId;
    var formValues = jQuery('#cartForm').serialize() + "&" + productIdParam;
    jQuery.ajax({
        url: 'getShoppingCartItemIndex',
        type: 'POST',
        async: false,
        data: formValues,
        success: function(json) {
            var serverError = getServerError(json);
            if (!serverError) {
                itemIndex = json.itemIndex;
            } else {
                jQuery('#shippingFormServerError').html(serverError);
            }
        }
    });
    return itemIndex;
}

function removeItem(elmt) {
    var removeElement = elmt;
    var elementId = removeElement.id;
    var qtyId = elementId.replace('removeItemLink_', 'qty_');
    var productIdElementId =  elementId.replace('removeItemLink_', 'cartLineProductId_');
    var productId = jQuery("#" + productIdElementId).val();
    var itemIndex = getProductLineItemIndex(elmt, productId);
    var formValues = "update_" + itemIndex + "= 0";
    if (jQuery(qtyId).val() == '' || isNaN(jQuery(qtyId).val())) {
        jQuery(qtyId).val("0");
    }
    updateCartData(qtyId, formValues, 0, itemIndex);
}

function cartItemQtyChanged(elmt) {
    var qtyElement = elmt;
    var elementId = qtyElement.id;
    var productIdElementId = elementId.replace('qty_', 'cartLineProductId_');
    var productId = jQuery("#" + productIdElementId).val();
    if (jQuery(qtyElement).val() && jQuery(qtyElement).val() >= 0 && !isNaN(jQuery(qtyElement).val())) {
        var itemIndex = getProductLineItemIndex(elmt, productId);
        qtyParam = "update_" + itemIndex +"="+jQuery(qtyElement).val();
        var formValues = jQuery('#cartForm').serialize() + '&' + qtyParam;
        updateCartData(elementId, formValues, qtyElement.value, itemIndex);
    }
}


function updateCartData(elementId, formValues, itemQty, itemIndex) {
    jQuery.ajax({
        url: 'cartItemQtyUpdate',
        type: 'POST',
        data: formValues,
        success: function(json) {
            if (json.totalQuantity == 0) {
                jQuery('#emptyCartCheckoutPanel').show();
                jQuery('.onepagecheckout').hide();                
            } else {
                // Replace whole cart panel with updated cart values for updating line item in case of gift item is added or remove in cart after applying coupon code
                // No need to calculate individual value for shopping cart when whole cart is updating
                jQuery.ajax({
                    url: 'UpdateCart',
                    type: 'POST',
                    cache: false,
                    success: function(data) {
                        jQuery('.onepagecheckout-cart').html(data);
                        initCartProcessObservers();
                    }
                });
            }
        }
    });
}

function updateUserSummary(json) {
	jQuery('#userPartyId').val(json.partyId);
	jQuery('#emailContactMechId').val(json.emailContactMechId);
	if (!json.username) {	
		var fullName = jQuery('#userFirstName').val() + " " + jQuery('#userLastName').val();
		jQuery('#completedEmailAddress').html(jQuery('#emailAddress').val());
		jQuery('#completedUserName').html(jQuery('#username').val());
	} else {
		var fullName = json.firstName + " " + json.lastName;
		jQuery('#completedEmailAddress').html(json.emailAddress);
		jQuery('#completedUserName').html(json.username);
	}
	jQuery('#completedFullName').html(fullName);
}

function updateShippingSummary() {
    var fullName = jQuery('#firstName').val() + " " +jQuery('#lastName').val();
    var extension = "";
    if (jQuery('#shipToExtension').val()) {
        extension = "-" + jQuery('#shipToExtension').val();
        }
    var shippingContactPhoneNumber = jQuery('#shipToCountryCode').val()+ "-" + jQuery('#shipToAreaCode').val()
        + "-" + jQuery('#shipToContactNumber').val() + extension;
    jQuery('#completedShipToAttn').html("Attn: " + fullName);
    jQuery('#completedShippingContactNumber').html(shippingContactPhoneNumber);
    jQuery('#completedEmailAddress').html(jQuery('#emailAddress').val());
    jQuery('#completedShipToAddress1').html(jQuery('#shipToAddress1').val());
    jQuery('#completedShipToAddress2').html(jQuery('#shipToAddress2').val());
    if (jQuery('#shipToStateProvinceGeoId').val() == "_NA_") {
        var shipToGeo = jQuery('#shipToCity').val()+", "+jQuery('#shipToCountryGeoId').val()+" "+jQuery('#shipToPostalCode').val();
    }
    else {
        var shipToGeo = jQuery('#shipToCity').val()+","+jQuery('#shipToStateProvinceGeoId').val() +" "+jQuery('#shipToCountryGeoId').val()+" "+jQuery('#shipToPostalCode').val();
    }
    jQuery('#completedShipToGeo').html(shipToGeo);
    // set shipToContactMechId in Billing form.
    jQuery('#shipToContactMechIdInBillingForm').val(jQuery('#shipToContactMechId').val());
}

function updateBillingSummary() {
    var fullName = jQuery('#firstNameOnCard').val() + " " +jQuery('#lastNameOnCard').val();
    jQuery('#completedBillToAttn').html("Attn: " + fullName);
    var extension = "";
    if (jQuery('#billToExtension').val()) {
        extension = "-" + jQuery('#billToExtension').val();
        }
    var billToPhoneNumber = jQuery('#billToCountryCode').val() + "-" + jQuery('#billToAreaCode').val() + "-" + jQuery('#billToContactNumber').val() + extension;
    jQuery('#completedBillToPhoneNumber').html(billToPhoneNumber);
    var cardNumber = "CC#:XXXXXXXXXXXX"+jQuery('#cardNumber').val().replace('-','').slice(12,16);
    jQuery('#completedCCNumber').html(cardNumber);
    var expiryDate = "Expires:"+jQuery('#expMonth').val()+"/"+jQuery('#expYear').val();
    jQuery('#completedExpiryDate').html(expiryDate);
    jQuery('#completedBillToAddress1').html(jQuery('#billToAddress1').val());
    jQuery('#completedBillToAddress2').html(jQuery('#billToAddress2').val());
    if (jQuery('#billToStateProvinceGeoId').val() == "_NA_") {
        var billToGeo = jQuery('#billToCity').val()+", "+jQuery('#billToCountryGeoId').val()+" "+jQuery('#billToPostalCode').val();
    }
    else {
        var billToGeo = jQuery('#billToCity').val()+", "+jQuery('#billToStateProvinceGeoId').val() +" "+jQuery('#billToCountryGeoId')+" "+jQuery('#billToPostalCode').val();
    }
    jQuery('#completedBillToGeo').html(billToGeo);
    jQuery('#paymentMethod').html(jQuery('#paymentMethodTypeId').val());
    jQuery('#billToContactMechIdInShipingForm').val(jQuery('#billToContactMechId'));
}


function processOrder() {
	jQuery('#processOrderButton').disabled = true;
	jQuery('#processOrderButton').fadeOut('fast');
	jQuery('#processingOrderButton').fadeIn('fast');
	jQuery('#orderSubmitForm').submit();
}

// Generic function for fetching country's associated state list, plus selecting an option.
// Copied and modified from /ordermgr/images/js/geoAutoCompleter.js to access a default value
// Needed as a workaround to set a default selected; the problem is the ajax call is async
// so trying to set default using .val(value) immediately after this call may happen before
// the ajax call returned to build the option list.
function getAssociatedStateListSelected(countryId, stateId, errorId, divId, selectedGeoId) {
 var countryGeoId = jQuery("#" + countryId).val();
 var requestToSend = "getAssociatedStateList";
 if (jQuery('#orderViewed')) {
     requestToSend = "/ordermgr/control/getAssociatedStateList"
 }
 jQuery.ajax({
     url: requestToSend,
     type: "POST",
     data: {countryGeoId: countryGeoId},
     success: function(data) {
         if (data._ERROR_MESSAGE_ ) {
             // no data found/ error occured
             return;
         }
         stateList = data.stateList;
         var stateSelect = jQuery("#" + stateId);
         stateSelect.find("option").remove();
         jQuery.each(stateList, function(state) {
             geoValues = this.split(': ');
             
             // Cato: Selected geo ID
             var selectedStr = '';
             if (selectedGeoId === geoValues[1]) {
             	selectedStr = ' selected="selected"';
             }
             
             stateSelect.append(jQuery('<option value="'+geoValues[1]+'"' + selectedStr + '>'+geoValues[0]+'</option>'));
         });

         if (stateList.length <= 1) {
             if (jQuery("#" + divId).is(':visible') || jQuery("#" + errorId).is(':visible')) {
                 jQuery("#divId").fadeOut("fast");
                 jQuery("#errorId").fadeOut("fast");
                 jQuery("#stateId").unbind("blur");
             }
         } else {
             jQuery("#divId").fadeIn("fast");
             jQuery("#stateId").bind("blur", function() {
                 if (jQuery("#" + stateId).val() == "") {
                     jQuery("#errorId").fadeIn("fast")
                 }
             });
         }
     }
 });
}

function getAssociatedBillingStateList(formName, divId) {
    var optionList = [];
    jQuery.ajax({
        url: "getAssociatedStateList",
        data: jQuery(formName).serialize(),
        async: false,
        success: function(transport) {
            stateList = data.stateList;
            var billingStates = jQuery("#" + divId);
            billingStates.find("option").remove();
            jQuery.each(stateList, function(state) {
                geoVolues = this.split(': ');
                billingStates.append(jQuery("<option value = " + geoVolues[1] + " >" + geoVolues[0] + "</option>"));
            });
        }
    });
}
