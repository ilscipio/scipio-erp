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
 * Scipio CMS Get Template - script
 * Fetches template information based on available parameters and puts them in the page context. Can be used
 * to determine whether or not the new page dialog should be displayed instead
 */

import java.util.ArrayList;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.LocalDispatcher;
 
final String module = "CmsGetTemplate.groovy";

/*Parameters*/
pageTemplateId = parameters.pageTemplateId;
versionId = parameters.versionId;
context.pageTemplateId = pageTemplateId;

if (pageTemplateId) {
    servCtx = ["userLogin": context.userLogin, "locale": context.locale, "pageTemplateId":pageTemplateId, "versionId":versionId];
    pageTmplResult = dispatcher.runSync("cmsGetPageTemplateAndVersions", servCtx);
    if (ServiceUtil.isSuccess(pageTmplResult)) {
        pageTemplate = pageTmplResult.pageTmpAndVersions;
        context.pageTemplate = pageTemplate;
        context.versionId = pageTemplate.versionId;
    } else {
        context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, pageTmplResult);
    }
}
