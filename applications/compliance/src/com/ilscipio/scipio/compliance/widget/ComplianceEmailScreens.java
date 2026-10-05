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
 * E-mail screens of the compliance component.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ComplianceEmailScreens {

    /** Confirmation of receipt of a withdrawal (CRD Art. 11a(3)): content and time, on a durable medium. */
    @Screen(name = "WithdrawalConfirmationEmail", location = "component://compliance/widget/ComplianceEmailScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ComplianceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/email/withdrawalConfirmation.ftl")
            })
        }
    )
    public interface WithdrawalConfirmationEmail {}

    /** Link to confirm a privacy request made without an account. */
    @Screen(name = "PrivacyVerifyEmail", location = "component://compliance/widget/ComplianceEmailScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ComplianceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "ScipioEmailDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://compliance/templates/email/privacyVerify.ftl")
            })
        }
    )
    public interface PrivacyVerifyEmail {}
}
