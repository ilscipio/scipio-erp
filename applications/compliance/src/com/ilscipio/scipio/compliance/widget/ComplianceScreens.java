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
package com.ilscipio.scipio.compliance.widget;

import com.ilscipio.scipio.widget.def.screen.*;

/**
 * Screens of the compliance back office.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ComplianceScreens {

    @Screen(name = "main", location = "component://compliance/widget/ComplianceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ComplianceApplication")
    @Action(type = ActionType.SCRIPT, location = "component://compliance/webapp/compliance/WEB-INF/actions/Overview.groovy")
    @DecoratorScreen(
        name = "GlobalDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/webapp/compliance/overview.ftl")
            })
        }
    )
    public interface Main {}
}
