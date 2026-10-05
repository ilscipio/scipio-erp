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
 * SCIPIO: Simple groovy script that gets some system info.
 */

import org.ofbiz.base.util.*;
 
final module = "GetSystemInfo.groovy";

scipioReleaseInfo = [:];

scipioReleaseInfo.name = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.name");
scipioReleaseInfo.group = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.group");
scipioReleaseInfo.desc = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.desc");
scipioReleaseInfo.branch = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.branch");
scipioReleaseInfo.version = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.version");

if (context.logSystemInfo) {
    Debug.logInfo("Scipio version info: " + scipioReleaseInfo, module);
    context.remove("logSystemInfo");
}

context.scipioReleaseInfo = scipioReleaseInfo;