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
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * One configurable Google Link URL. To configure, use {@link Options}
 *
 * @author Sergio Vico
 * @see Options
 * @see <a href="https://support.google.com/webmasters/answer/2620865">Creating alternate language pages Sitemaps</a>
 * @see <a href="https://developers.google.com/search/mobile-sites/mobile-seo/separate-urls?hl=en">Mobile SEO configurations | Separate URLs </a>
 */
public class GoogleLinkSitemapUrl extends WebSitemapUrl {

	/** Options to configure URLs with alternates */
	public static class Options extends AbstractSitemapUrlOptions<GoogleLinkSitemapUrl, Options> {
		private final Map<URI, Map<String, String>> alternates;

		private static Map<URI, Map<String, String>> convertAlternates(final Map<String, Map<String, String>> alternates)
				throws URISyntaxException {

			final Map<URI, Map<String, String>> converted = new LinkedHashMap<URI, Map<String, String>>();
			for (final Map.Entry<String, Map<String, String>> entry : alternates.entrySet()) {
				converted.put(new URI(entry.getKey()), new LinkedHashMap<String, String>(entry.getValue()));
			}
			return converted;
		}

		/**
		 * Options constructor with the alternates configurations
		 *
		 * @param url Base URL into which we will be adding alternates
		 * @param alternates Map&lt;String, Map&lt;String, String&gt;&gt; where the key is the href and
		 *					the value is a generic Map&lt;String, String&gt; holding the attributes of
		 *					the link (e.g. hreflang, media, ...)
		 */
		public Options(final String url, final Map<String, Map<String, String>> alternates) throws URISyntaxException, MalformedURLException {

			this(new URL(url), convertAlternates(alternates));
		}

		/**
		 * Options constructor with the alternates configurations
		 *
		 * @param url Base URL into which we will be adding alternates
		 * @param alternates Map&lt;URL, Map&lt;String, String&gt;&gt; where the key is the href and
		 *					the value is a generic Map&lt;String, String&gt; holding the attributes of
		 *					the link (e.g. hreflang, media, ...)
		 */
		public Options(final URL url, final Map<URI, Map<String, String>> alternates) {
			super(url, GoogleLinkSitemapUrl.class);
			this.alternates = new LinkedHashMap<URI, Map<String, String>>(alternates);
		}
	}

	private final Map<URI, Map<String, String>> alternates;

	/**
	 * Constructor specifying the URL and the alternates configurations with Options object
	 *
	 * @param options Configuration object to initialize the GoogleLinkSitemapUrl with.
	 * @see Options#Options(java.lang.String, java.util.Map)
	 */
	public GoogleLinkSitemapUrl(final Options options) {
		super(options);
		alternates = options.alternates;
	}

	/**
	 * Constructor specifying the URL as a String and the alternates configurations
	 *
	 * @param url Base URL into which we will be adding alternates
	 * @param alternates Map&lt;String, Map&lt;String, String&gt;&gt; where the key is the href and
	 *					the value is a generic Map&lt;String, String&gt; holding the attributes of
	 *					the link (e.g. hreflang, media, ...)
	 */
	public GoogleLinkSitemapUrl(final String url, final Map<String, Map<String, String>> alternates) throws URISyntaxException, MalformedURLException {
		this(new Options(url, alternates));
	}

	/**
	 * Constructor specifying the URL as a URL and the alternates configurations
	 *
	 * @param url Base URL into which we will be adding alternates
	 * @param alternates Map&lt;String, Map&lt;String, String&gt;&gt; where the key is the href and
	 *					the value is a generic Map&lt;String, String&gt; holding the attributes of
	 *					the link (e.g. hreflang, media, ...)
	 */
	public GoogleLinkSitemapUrl(final URL url, final Map<URI, Map<String, String>> alternates) {
		this(new Options(url, alternates));
	}

	public Map<URI, Map<String, String>> getAlternates() {

		return this.alternates;
	}
}
