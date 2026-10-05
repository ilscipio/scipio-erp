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
 * Scipio CMS Get Media - script
 * Fetches Media files and puts them into context
 */

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;

import org.ofbiz.service.LocalDispatcher;

final String module = "CmsGetMediaList.groovy";

/*Parameters*/
int viewSize = (context.viewSize ?: 0) as Integer;
int viewIndex = (context.viewIndex ?: 0) as Integer;

simpleSearchText = (parameters.simpleSearchText ?: null) as String;
inputFields = null;
if (simpleSearchText) {
    inputFields = [
        "contentName": simpleSearchText,
        "contentName_op": "contains",
        "contentName_ic": "Y"
    ];
}

mediaResult = dispatcher.runSync("cmsGetMediaFiles", ["userLogin": context.userLogin, "locale": context.locale, 
    "viewSize":viewSize?:50, "viewIndex":viewIndex?:0, "inputFields":inputFields]);
if (ServiceUtil.isSuccess(mediaResult)) {
    mediaFiles = mediaResult.mediaFiles;
    
    context.viewSize = mediaResult.viewSize;
    context.viewIndex = mediaResult.viewIndex;
    context.listSize = mediaResult.listSize;
    context.mediaFiles = mediaFiles;
} else {
    context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, mediaResult);
}
