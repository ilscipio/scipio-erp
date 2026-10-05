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
/*
 * SCIPIO: 4.0.0: Aurora Shop - the Aurora style map with storefront changes.
 */
import org.ofbiz.base.util.GroovyUtil

GroovyUtil.runScriptAtLocation("component://aurora-theme/includes/themeStyles.groovy", null, context);

context.styles.putAll([
        "framework" : "aurora",
        "customSideBar" : false,
        // shoppers read forms top to bottom: every label sits above its field (the back office puts it to the left)
        "fields_default_labeltype" : "vertical",
        "fields_default_labelposition" : "top",
        "fields_default_labelareaexceptions" : "submit submitarea",
        "fields_default_labelarearequirecontent" : true,
        // a single checkbox or radio takes its label (or label content, e.g. the address of a ship-to choice) inline
        "fields_default_labelareaconsumeexceptions" : "checkbox-single radio-single",
])
