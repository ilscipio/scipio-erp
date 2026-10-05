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

/**
 * SCIPIO: Cross-app decorator forwarding stubs for the shop webapp.
 *
 * <p>Some order-entry screens (e.g. compareProducts) are reachable from the shop webapp and
 * include their decorators using {@code ${parameters.mainDecoratorLocation}}, i.e. they look
 * up the decorator NAME at the LOCAL webapp's CommonScreens location (here:
 * component://shop/widget/CommonScreens.xml). shop never defined these names, causing
 * "Could not find screen with name [X] in class resource [...shop/widget/CommonScreens.xml]"
 * errors (pre-existing hole; the XML era had the same gap).</p>
 *
 * <p>Each entry forwards the lookup to the screen's canonical (owning-component) definition,
 * following the pattern of {@code SfaCrossAppDecorators}.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (repair wave, cross-app decorator gap fix).</p>
 */
public class ShopCrossAppDecorators {

    @Screen(name = "CommonPopUpDecorator", location = "component://shop/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonPopUpDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    public interface CommonPopUpDecorator {}
}
