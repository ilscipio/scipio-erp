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
 * SCIPIO: setup wizard static step properties (labels, icons)
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupWizardStaticProperties.groovy";

// if needed
//def setupStepList = SetupWorker.getStepsStatic(); // (excludes "finished")

// TODO: REVIEW: some of these could go in global styles

context.setupStepTitlePropMap = [
    "organization": "SetupOrganization",
    "store": "CommonStore",
    "user": "PartyParty",
    "accounting": "AccountingAccounting",
    "facility": "ProductFacility",
    "catalog": "ProductCatalog",
    "website": "SetupWebSite"
];

context.setupStepIconMap = [
    "organization": "fa-user-times",
    "store": "fa-shopping-cart", 
    "user": "fa-users", 
    "accounting": "fa-balance-scale", 
    "facility": "fa-cube", 
    "catalog": "fa-sitemap", 
    "website": "fa-file-text",
    "default": "fa-info"
];

