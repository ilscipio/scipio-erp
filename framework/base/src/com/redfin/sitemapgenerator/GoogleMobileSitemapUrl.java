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

/**
 * One configurable Google Mobile Search URL.  To configure, use {@link Options}
 * @author Dan Fabulich
 * @see Options
 * @see <a href="http://www.google.com/support/webmasters/bin/answer.py?answer=34648">Creating Mobile Sitemaps</a>
 */
public class GoogleMobileSitemapUrl extends WebSitemapUrl {

	/** Options to configure mobile URLs */
	public static class Options extends AbstractSitemapUrlOptions<GoogleMobileSitemapUrl, Options> {

		/** Specifies the url */
		public Options(String url) throws MalformedURLException {
			this(new URL(url));
		}
		
		/** Specifies the url */
		public Options(URL url) {
			super(url, GoogleMobileSitemapUrl.class);
		}
	}
	
	/** Specifies the url */
	public GoogleMobileSitemapUrl(String url) throws MalformedURLException {
		this(new Options(url));
	}

	/** Specifies the url */
	public GoogleMobileSitemapUrl(URL url) {
		this(new Options(url));
	}

	/** Specifies configures url with options */
	public GoogleMobileSitemapUrl(Options options) {
		super(options);
	}

}
