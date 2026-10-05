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
/**
 * SCIPIO: jquery plugin to add a class to subsets of html elements.
 * Used to add classes to elements, so that they can be styled as boxes.
 * */
var elSelector = 'pre.scrollable,.section-screenlet-content';
var notSubSelector = 'pre.scrollable,.section-screenlet-content, table,.orbit-container, .steps, .tile-container, .signup-panel,.button-group,.trumbowyg-box';
var scipioBoxClass = 'box';


function scipio_boxify(){
    $(elSelector).each(function(){
        if($(this).has(notSubSelector).length == 0 && $(this).text().trim().length > 0){
            $(this).addClass(scipioBoxClass);
        }
    });
}

$(function(){
    scipio_boxify();
});

