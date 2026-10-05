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
package @component-package@.@component-name@.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Service definitions for the @component-resource-name@ component.
 *
 * <p>SCIPIO: 4.0.0: Added by createComponent template.</p>
 */
public class @component-resource-name@Services {

    @Service(
        name = "create@component-resource-name@",
        engine = "java",
        location = "@component-package@.@component-name@.service.@component-resource-name@ServiceImpl",
        invoke = "create@component-resource-name@",
        description = "Creates one @component-resource-name@ record.",
        auth = "true",
        attributes = {
            @Attribute(name = "name", type = "String", mode = "IN")
        }
    )
    public interface Create@component-resource-name@ {}

}
