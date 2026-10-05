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

imgView = {
    init: function() {
        if (document.getElementById) {
            allAnchors = document.getElementsByTagName('a');
            if (allAnchors.length) {
                for (var i = 0; i < allAnchors.length; i++) {
                    if (allAnchors[i].getAttributeNode('swapDetail') && allAnchors[i].getAttributeNode('swapDetail').value != '') {
                        allAnchors[i].onmouseover = imgView.showImage;
                        allAnchors[i].onmouseout = imgView.showDetailImage;
                    }
                }
            }
        }
    },
    showDetailImage: function() { 
        var mainImage = document.getElementById('detailImage');
        mainImage.src = document.getElementById('originalImage').value;
        return false;
    },
    showImage: function() {
        var mainImage = document.getElementById('detailImage');
        mainImage.src = this.getAttributeNode('swapDetail').value;
        return false;
    },
    addEvent: function(element, eventType, doFunction, useCapture) {
        if (element.addEventListener) {
            element.addEventListener(eventType, doFunction, useCapture);
            return true;
        }else if (element.attachEvent) {
              var r = element.attachEvent('on' + eventType, doFunction);
              return r;
        }else {
             element['on' + eventType] = doFunction;
        }
    }
}
jQuery(document).ready(imgView.init);
