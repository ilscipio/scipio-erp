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
package com.redfin.sitemapgenerator;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

/**
 * Represents a single sitemap for inclusion in a sitemap index.
 * @author Dan Fabulich
 *
 */
public class SitemapIndexUrl {
	final URL url;
	final Date lastMod;
	/** Configures the sitemap URL with a specified lastMod */
	public SitemapIndexUrl(URL url, Date lastMod) {
		this.url = url;
		this.lastMod = lastMod;
	}
	
	/** Configures the sitemap URL with a specified lastMod */
	public SitemapIndexUrl(String url, Date lastMod) throws MalformedURLException {
		this(new URL(url), lastMod);
	}
	
	/** Configures the sitemap URL with no specified lastMod; we'll use {@link SitemapIndexGenerator.Options#defaultLastMod(Date)} or leave it blank if no default is specified */
	public SitemapIndexUrl(URL url) {
		this(url, null);
	}
	
	/** Configures the sitemap URL with no specified lastMod; we'll use {@link SitemapIndexGenerator.Options#defaultLastMod(Date)} or leave it blank if no default is specified */
	public SitemapIndexUrl(String url) throws MalformedURLException {
		this(new URL(url));
	}
}
