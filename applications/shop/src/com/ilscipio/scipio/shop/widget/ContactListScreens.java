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
package com.ilscipio.scipio.shop.widget;

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContactListScreens {

    @Screen(name = "DefaultOptOutScreen", location = "component://shop/widget/ContactListScreens.xml")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "Opt-Out Results", labels = {
                    @Label(text = "You have been successfully removed from the ${contactList.contactListName} mailing list!", style = "common-msg-info-important"
                )})})
        }
    )
    public interface DefaultOptOutScreen {}

    @Screen(name = "OptOutResponse", location = "component://shop/widget/ContactListScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "optOutOfListFromCommEvent", resultMapName = "optOutResult")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList", fieldMaps = {@FieldMap(fieldName = "contactListId", fromField = "optOutResult.contactListId")})
    @Action(type = ActionType.SET, field = "contactListId", fromField = "contactList.contactListId")
    @Action(type = ActionType.SET, field = "screenName", fromField = "contactList.optOutScreen", defaultValue = "component://shop/widget/ContactListScreens.xml#DefaultOptOutScreen")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${screenName}", shareScope = true)}))
    public interface OptOutResponse {}

}
