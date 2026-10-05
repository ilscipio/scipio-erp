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
package com.ilscipio.scipio.humanres.widget;

import com.ilscipio.scipio.widget.def.screen.*;

/**
 * SCIPIO: Cross-app decorator forwarding stubs for the humanres webapp.
 *
 * <p>Some content/party screens are reachable from the humanres webapp and
 * include their decorators using {@code ${parameters.mainDecoratorLocation}}, i.e. they look
 * up the decorator NAME at the LOCAL webapp's CommonScreens location (here:
 * component://humanres/widget/CommonScreens.xml). humanres never defined these names,
 * causing "Could not find screen with name [X] in class resource [...humanres/widget/CommonScreens.xml]"
 * errors (pre-existing hole; the XML era had the same gap).</p>
 *
 * <p>Each entry forwards the lookup to the screen's canonical (owning-component) definition,
 * following the pattern of {@code SfaCrossAppDecorators}.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (repair wave, cross-app decorator gap fix).</p>
 */
public class HumanresCrossAppDecorators {

    @Screen(name = "CommonContentAppDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentAppDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentAppDecorator {}

    @Screen(name = "CommonDataResourceDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonDataResourceDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonDataResourceDecorator {}

    @Screen(name = "CommonCommunicationEventDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonCommunicationEventDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonCommunicationEventDecorator {}

    @Screen(name = "SecurityDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "SecurityDecorator", location = "component://common/widget/SecurityScreens.xml")
    public interface SecurityDecorator {}

    @Screen(name = "CommonLayoutDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonLayoutDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonLayoutDecorator {}

    @Screen(name = "CommonContentDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentDecorator {}

    @Screen(name = "CommonPartyAppDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonPartyAppDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonPartyAppDecorator {}

    @Screen(name = "CommonForumDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonForumDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonForumDecorator {}

    @Screen(name = "CommonDataResourceSetupDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonDataResourceSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonDataResourceSetupDecorator {}

    @Screen(name = "CommonContentSetupDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonContentSetupDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonContentSetupDecorator {}

    @Screen(name = "CommonWebSiteDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonWebSiteDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonWebSiteDecorator {}

    @Screen(name = "ContentDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "ContentDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface ContentDecorator {}

    @Screen(name = "CommonWebAnalyticsDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonWebAnalyticsDecorator", location = "component://content/widget/CommonScreens.xml")
    public interface CommonWebAnalyticsDecorator {}

    @Screen(name = "CommonPartyInvitationDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonPartyInvitationDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonPartyInvitationDecorator {}

    @Screen(name = "CommonPartyClassificationDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonPartyClassificationDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonPartyClassificationDecorator {}

    @Screen(name = "CommonRequestDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @IncludeScreen(name = "CommonRequestDecorator", location = "component://party/widget/partymgr/CommonScreens.xml")
    public interface CommonRequestDecorator {}

}
