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
package @component-package@.@component-name@.widget;

import com.ilscipio.scipio.widget.def.form.*;

/**
 * Form definitions for the @component-resource-name@ component.
 *
 * <p>SCIPIO: 4.0.0: Added by createComponent template.</p>
 */
public class @component-resource-name@Forms {

    @Form(
        name = "List@component-resource-name@",
        location = "component://@component-name@/widget/@component-resource-name@Forms.xml",
        type = FormType.LIST,
        listName = "@component-resource-name@List",
        autoFieldsEntity = @AutoFieldsEntity(entityName = "@component-resource-name@", defaultFieldType = DefaultFieldType.DISPLAY)
    )
    public interface List@component-resource-name@ {}

}
