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
package com.ilscipio.scipio.commerceprofile.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Guard services of the hosted profile. The service ECA rules in {@code servicedef/secas.xml} call them before a
 * guarded service runs. A guard returns an error when the hosted profile refuses the call.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-02).</p>
 */
public class HostedServices {

    @Service(
        name = "hostedGuardCode",
        engine = "java",
        location = "com.ilscipio.scipio.commerceprofile.service.HostedServiceImpl",
        invoke = "guardCode",
        description = "Hosted profile: refuses a service that runs code, loads data, schedules a job or writes a file, "
                + "unless the caller is the system or an operator (HOSTED_OPS). No effect when scipio.hosted is false.",
        auth = "false",
        validate = "false",
        useTransaction = "false"
    )
    public interface HostedGuardCode {}

    @Service(
        name = "hostedGuardCmsCode",
        engine = "java",
        location = "com.ilscipio.scipio.commerceprofile.service.HostedServiceImpl",
        invoke = "guardCmsCode",
        description = "Hosted profile: refuses a CMS service that writes a template body, a script or an asset, "
                + "unless the caller holds CMS_CODE_UPDATE. No effect when scipio.hosted is false.",
        auth = "false",
        validate = "false",
        useTransaction = "false"
    )
    public interface HostedGuardCmsCode {}

    @Service(
        name = "hostedGuardMail",
        engine = "java",
        location = "com.ilscipio.scipio.commerceprofile.service.HostedServiceImpl",
        invoke = "guardMail",
        description = "Hosted profile: refuses mail server parameters (sendVia, authUser, authPass, port, ...) from a store caller. "
                + "The platform relay settings are the only mail server.",
        auth = "false",
        validate = "false",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "sendVia", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "authUser", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "authPass", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "port", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "sendType", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "socketFactoryClass", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "socketFactoryPort", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "socketFactoryFallback", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "startTLSEnabled", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "allowCustomHeaders", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "customHeaders", type = "Object", mode = "IN", optional = "true")
        }
    )
    public interface HostedGuardMail {}

    @Service(
        name = "hostedGuardScreenFile",
        engine = "java",
        location = "com.ilscipio.scipio.commerceprofile.service.HostedServiceImpl",
        invoke = "guardScreenFile",
        description = "Hosted profile, createFileFromScreen: refuses filePath and rootDir, a fileName with a path, a screen that is not "
                + "a component:// location, and a result path outside the store folder.",
        auth = "false",
        validate = "false",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "filePath", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "rootDir", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "fileName", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "screenLocation", type = "Object", mode = "IN", optional = "true")
        }
    )
    public interface HostedGuardScreenFile {}

    @Service(
        name = "hostedGuardImport",
        engine = "java",
        location = "com.ilscipio.scipio.commerceprofile.service.HostedServiceImpl",
        invoke = "guardImport",
        description = "Hosted profile, entityImport: only the files of the property hosted.import.allow (the setup wizard files), "
                + "no fulltext, no fmfilename, no URL.",
        auth = "false",
        validate = "false",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "filename", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "fmfilename", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "fulltext", type = "Object", mode = "IN", optional = "true"),
            @Attribute(name = "isUrl", type = "Object", mode = "IN", optional = "true")
        }
    )
    public interface HostedGuardImport {}

    @Service(
        name = "hostedGuardCmsView",
        engine = "java",
        location = "com.ilscipio.scipio.commerceprofile.service.HostedServiceImpl",
        invoke = "guardCmsView",
        description = "Hosted profile: refuses a CMS service that returns template, script or asset code, unless the caller holds "
                + "CMS_CODE_VIEW or CMS_CODE_UPDATE.",
        auth = "false",
        validate = "false",
        useTransaction = "false"
    )
    public interface HostedGuardCmsView {}
}
