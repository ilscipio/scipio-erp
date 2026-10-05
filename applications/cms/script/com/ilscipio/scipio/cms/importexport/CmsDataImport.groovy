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
 * SCIPIO: data import page script (original).
 */
import java.util.*;
import java.io.*;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.model.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.transaction.*;
import org.ofbiz.entity.condition.*;

final module = "CmsDataImport.groovy";

cmsErrorHandler = context.cmsErrorHandler;

messages = context.messages;
context.remove("messages"); // remove this to prevent any read by template
if (messages) {
    cmsErrorHandler.addContextEventMsgs(context, messages);
}
