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

var webSocket;

// Create a new instance of the websocket
webSocket = new WebSocket('wss://' + window.location.host + '/admin/ws/pushNotifications');

webSocket.onopen = function(event){
    // Do any operation on open
};

webSocket.onmessage = function(event) {

    // Remove already present notification
    jQuery('.pushNotification').remove();

    // Create notification on the fly, this notification markup can be added in Messages.ftl and its css can be theme based.
    jQuery("body")
        .append(jQuery('<div/>')
        .addClass('pushNotification')
        .css({'position': 'fixed', 'top': '5%', 'right': '2%', 'background': 'lightgrey', 'border-radius': '10px', 'z-index': '9999'}));

    jQuery('.pushNotification')
        .append(jQuery('<a href="javascript:void(0);" class="closeNotification"/>')
        .css({'color': 'black', 'font-size': '15px', 'position': 'fixed', 'right': '2%'}).append('close'));

    jQuery('.pushNotification')
        .append(jQuery('<p/>')
        .addClass('msg')
        .css({'font-size': '15px', 'padding': '30px 20px 30px 20px'}));

    jQuery('.pushNotification').find('.msg').append(event.data);

    // show notification
    jQuery('.pushNotification').fadeIn();

    // Remove notification after 5 seconds
    setTimeout(function() {
        jQuery('.pushNotification').remove();
    }, 5000 );

    // Added observer for close link.
    jQuery('.closeNotification').click(function() {
        jQuery(this).parent('.pushNotification').remove();
    });
};

webSocket.onerror = function(event){
    // Do any operation on error
};
