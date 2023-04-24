/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/

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
