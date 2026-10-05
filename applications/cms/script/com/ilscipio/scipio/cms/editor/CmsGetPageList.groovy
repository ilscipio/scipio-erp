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
 * Scipio CMS Get Page List - script
 * Fetches all available pages and puts the information into context
 */

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;

import org.ofbiz.service.LocalDispatcher;

final String module = "CmsGetPageList.groovy";

/* Initialize */
userLogin = context.userLogin;

/* Parameters */
int viewSize = ((context.viewSize) ? context.viewSize : 0) as Integer;
int viewIndex = ((context.viewIndex) ? context.viewIndex : 0) as Integer;

webSiteId = parameters.webSiteId ?: null;
context.webSiteId = webSiteId;
resultMap = dispatcher.runSync("cmsGetPages", ["request":request, "response": response, 
    "userLogin":context.userLogin, "locale": context.locale, "webSiteId":webSiteId]);
if (ServiceUtil.isSuccess(resultMap)) {
    List pageList = resultMap.pages;
    
    listSize = pageList ? pageList.size() : 0;
    lowIndex = viewIndex * viewSize;
    highIndex = (viewIndex + 1) * viewSize;
    highIndex = highIndex > listSize ? listSize : highIndex;
    lowIndex = lowIndex > highIndex ? highIndex : lowIndex;
    
    List returnList = pageList.subList(lowIndex,highIndex);
    
    context.viewSize = viewSize;
    context.viewIndex = viewIndex;
    context.listSize = listSize;
    context.pages = returnList;
} else {
}
