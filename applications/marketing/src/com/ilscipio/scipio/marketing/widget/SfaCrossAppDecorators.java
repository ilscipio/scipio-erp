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

import com.ilscipio.scipio.widget.def.screen.*;

/**
 * SCIPIO: Cross-app decorator forwarding stubs for the sfa (CRM) webapp.
 *
 * <p>Several content/party/catalog/marketing screens are reachable from the sfa webapp, and
 * those screens include their decorators using {@code ${parameters.mainDecoratorLocation}},
 * i.e. they look up the decorator NAME at the LOCAL webapp's CommonScreens location (here:
 * component://marketing/widget/sfa/CommonScreens.xml). sfa never defined these names, causing
 * "Could not find screen with name [X] in class resource [...sfa/CommonScreens.xml]" errors.
 * Note this includes CommonMarketingAppDecorator itself: it is defined at
 * component://marketing/widget/CommonScreens.xml (the "marketing" webapp), a DIFFERENT location
 * from the sfa webapp's own CommonScreens, so sfa needs a forwarding stub for it too.</p>
 *
 * <p>Each entry here simply forwards the lookup to the screen's canonical (owning-component)
 * definition. Because the canonical decorator itself resolves its own main-decorator include via
 * {@code ${parameters.mainDecoratorLocation}}, the local (sfa) chrome/menu is preserved.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (repair wave, cross-app decorator gap fix).</p>
 */
public class SfaCrossAppDecorators {

    @Screen(name = "CommonCatalogAppDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonCatalogAppDecorator", location = "component://product/widget/catalog/CommonScreens.xml")
    public interface CommonCatalogAppDecorator {}

    @Screen(name = "CommonContentAppDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentAppDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentAppDecorator {}

    @Screen(name = "CommonContentDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentDecorator {}

    @Screen(name = "CommonContentSetupDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentSetupDecorator {}

    @Screen(name = "CommonDataResourceDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonDataResourceDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonDataResourceDecorator {}

    @Screen(name = "CommonDataResourceSetupDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonDataResourceSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonDataResourceSetupDecorator {}

    @Screen(name = "CommonForumDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonForumDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonForumDecorator {}

    @Screen(name = "CommonLayoutDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonLayoutDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonLayoutDecorator {}

    @Screen(name = "CommonMarketingAppDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonMarketingAppDecorator", location = "component://marketing/widget/CommonScreens.xml")
    public interface CommonMarketingAppDecorator {}

    @Screen(name = "CommonPartyAppDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonPartyAppDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonPartyAppDecorator {}

    @Screen(name = "CommonPartyClassificationDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonPartyClassificationDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonPartyClassificationDecorator {}

    @Screen(name = "CommonPartyInvitationDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonPartyInvitationDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonPartyInvitationDecorator {}

    @Screen(name = "CommonWebAnalyticsDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonWebAnalyticsDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonWebAnalyticsDecorator {}

    @Screen(name = "CommonWebSiteDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonWebSiteDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonWebSiteDecorator {}

    @Screen(name = "ContentDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "ContentDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface ContentDecorator {}

    @Screen(name = "SecurityDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "SecurityDecorator", location = "component://common/widget/SecurityScreens.xml")
    public interface SecurityDecorator {}

    /** SCIPIO: ofbizsetup screens (EditWebSite etc.) reached through the sfa/crm controller includes. */
    @Screen(name = "CommonSetupDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonSetupDecorator", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    public interface CommonSetupDecorator {}

    @Screen(name = "CommonSetupAppDecorator", location = "component://marketing/widget/sfa/CommonScreens.xml")
    @IncludeScreen(name = "CommonSetupAppDecorator", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    public interface CommonSetupAppDecorator {}

}
