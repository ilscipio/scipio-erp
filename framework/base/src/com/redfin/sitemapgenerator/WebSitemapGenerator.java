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

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

/**
 * Generates a regular old sitemap (USE THIS CLASS FIRST).  To configure options, use {@link #builder(URL, File)}
 * @author Dan Fabulich
 *
 */
public class WebSitemapGenerator extends SitemapGenerator<WebSitemapUrl,WebSitemapGenerator> {
	
	WebSitemapGenerator(AbstractSitemapGeneratorOptions<?> options) {
		super(options, new Renderer(options));
	}
	
	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<WebSitemapGenerator> builder(URL baseUrl, File baseDir) {
		return new SitemapGeneratorBuilder<WebSitemapGenerator>(baseUrl, baseDir, WebSitemapGenerator.class);
	}
	
	/** Configures a builder so you can specify sitemap generator options
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 * @return a builder; call .build() on it to make a sitemap generator
	 */
	public static SitemapGeneratorBuilder<WebSitemapGenerator> builder(String baseUrl, File baseDir) throws MalformedURLException {
		return new SitemapGeneratorBuilder<WebSitemapGenerator>(baseUrl, baseDir, WebSitemapGenerator.class);
	}

	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 */

	public WebSitemapGenerator(String baseUrl, File baseDir)
			throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl), baseDir));
	}

	/**Configures the generator with a base URL and directory to write the sitemap files.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 * @param baseDir Sitemap files will be generated in this directory as either "sitemap.xml" or "sitemap1.xml" "sitemap2.xml" and so on.
	 */
	public WebSitemapGenerator(URL baseUrl, File baseDir) {
		this(new SitemapGeneratorOptions(baseUrl, baseDir));
	}

	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public WebSitemapGenerator(String baseUrl) throws MalformedURLException {
		this(new SitemapGeneratorOptions(new URL(baseUrl)));
	}
	
	/**Configures the generator with a base URL and a null directory. The object constructed
	 * is not intended to be used to write to files. Rather, it is intended to be used to obtain
	 * XML-formatted strings that represent sitemaps.
	 * 
	 * @param baseUrl All URLs in the generated sitemap(s) should appear under this base URL
	 */
	public WebSitemapGenerator(URL baseUrl) {
		this(new SitemapGeneratorOptions(baseUrl));
	}
	
	private static class Renderer extends AbstractSitemapUrlRenderer<WebSitemapUrl> implements ISitemapUrlRenderer<WebSitemapUrl> {

		private final String namespaces; // SCIPIO: 3.0.0: Added

		public Renderer(AbstractSitemapGeneratorOptions<?> options) { // SCIPIO: 3.0.0: Added
			String namespaces = null;
			if (options.namespaces != null && !options.namespaces.isEmpty()) {
				String ns = "";
				for (Map.Entry<String, String> entry : options.namespaces.entrySet()) {
					String name = entry.getKey();
					String value = entry.getValue();
					if (!name.contains(":")) {
						name = "xmlns:" + name;
					}
					if (!ns.isEmpty()) {
						ns += " ";
					}
					ns += name + "=\"" + value + "\"";
				}
				namespaces = ns;
			}
			this.namespaces = namespaces;
		}

		public Class<WebSitemapUrl> getUrlClass() {
			return WebSitemapUrl.class;
		}
		
		public String getXmlNamespaces() {
			return namespaces;
		}

		public void render(WebSitemapUrl url, StringBuilder sb, W3CDateFormat dateFormat) {
			super.render(url, sb, dateFormat, null);
		}

	}
}
