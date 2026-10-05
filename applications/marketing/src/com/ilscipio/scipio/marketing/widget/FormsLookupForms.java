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
package com.ilscipio.scipio.marketing.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FormsLookupForms {

    @Form(
        name = "listLookupAccount",
        location = "component://marketing/widget/sfa/forms/LookupForms.xml",
        extendsForm = "listAccounts",
        extendsResource = "component://marketing/widget/sfa/forms/AccountForms.xml",
        fields = {
            @FormField(name = "partyId", widgetStyle = "${styles.link_run_local} ${styles.action_select}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyName} [${partyId}]", alsoHidden = false)),
            @FormField(name = "relatedCompany", hidden = @HiddenField)
        }
    )
    public interface listLookupAccount {}

    @Form(
        name = "listLookupLead",
        location = "component://marketing/widget/sfa/forms/LookupForms.xml",
        extendsForm = "listLeads",
        extendsResource = "component://marketing/widget/sfa/forms/LeadForms.xml",
        fields = {
            @FormField(name = "partyId", widgetStyle = "${styles.link_run_local} ${styles.action_select}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyName} [${partyId}]", alsoHidden = false)),
            @FormField(name = "relatedCompany", hidden = @HiddenField)
        }
    )
    public interface listLookupLead {}

    @Form(
        name = "listLookupAccountLead",
        location = "component://marketing/widget/sfa/forms/LookupForms.xml",
        extendsForm = "listAccountLeads",
        extendsResource = "component://marketing/widget/sfa/forms/AccountForms.xml",
        fields = {
            @FormField(name = "partyId", widgetStyle = "${styles.link_run_local} ${styles.action_select}", hyperlink = @HyperlinkField(target = "javascript:set_value('${partyId}')", urlMode = UrlMode.PLAIN, description = "${partyName} [${partyId}]", alsoHidden = false)),
            @FormField(name = "relatedCompany", hidden = @HiddenField)
        }
    )
    public interface listLookupAccountLead {}

}
