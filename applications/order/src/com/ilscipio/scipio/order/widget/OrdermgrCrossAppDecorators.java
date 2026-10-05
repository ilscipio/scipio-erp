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
package com.ilscipio.scipio.order.widget;

import com.ilscipio.scipio.widget.def.screen.*;

/**
 * SCIPIO: Cross-app decorator forwarding stubs for the ordermgr webapp.
 *
 * <p>Several content screens are reachable from the ordermgr webapp, and those screens include
 * their decorators using {@code ${parameters.mainDecoratorLocation}}, i.e. they look up the
 * decorator NAME at the LOCAL webapp's CommonScreens location (here:
 * component://order/widget/ordermgr/CommonScreens.xml). ordermgr never defined these names,
 * causing "Could not find screen with name [X] in class resource [...ordermgr/CommonScreens.xml]"
 * errors.</p>
 *
 * <p>Each entry here simply forwards the lookup to the screen's canonical (content component)
 * definition. Because the canonical decorator itself resolves its own main-decorator include via
 * {@code ${parameters.mainDecoratorLocation}}, the local (ordermgr) chrome/menu is preserved.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (repair wave, cross-app decorator gap fix).</p>
 */
public class OrdermgrCrossAppDecorators {

    @Screen(name = "CommonContentAppDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentAppDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentAppDecorator {}

    @Screen(name = "CommonContentDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentDecorator {}

    @Screen(name = "CommonContentSetupDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentSetupDecorator {}

    @Screen(name = "CommonDataResourceDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonDataResourceDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonDataResourceDecorator {}

    @Screen(name = "CommonDataResourceSetupDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonDataResourceSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonDataResourceSetupDecorator {}

    @Screen(name = "CommonForumDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonForumDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonForumDecorator {}

    @Screen(name = "CommonLayoutDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonLayoutDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonLayoutDecorator {}

    @Screen(name = "CommonWebAnalyticsDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonWebAnalyticsDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonWebAnalyticsDecorator {}

    @Screen(name = "CommonWebSiteDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "CommonWebSiteDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonWebSiteDecorator {}

    @Screen(name = "ContentDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @IncludeScreen(name = "ContentDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface ContentDecorator {}

}
