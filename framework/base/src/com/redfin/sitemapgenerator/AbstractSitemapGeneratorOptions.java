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
import java.net.URL;

// that weird thing with generics is so sub-classed objects will return themselves
// It makes sense, I swear! http://madbean.com/2004/mb2004-3/
abstract class AbstractSitemapGeneratorOptions<THIS extends AbstractSitemapGeneratorOptions<THIS>> {
	File baseDir;
	URL baseUrl;
	String fileNamePrefix = "sitemap";
	boolean allowEmptySitemap = false;
	boolean allowMultipleSitemaps = true;
	String suffixStringPattern; // this will store some type of string pattern suitable per needs.
	W3CDateFormat dateFormat;
	int maxUrls = SitemapGenerator.MAX_URLS_PER_SITEMAP;
	boolean autoValidate = false;
	boolean gzip = false;
	
	public AbstractSitemapGeneratorOptions(URL baseUrl, File baseDir) {
		if (baseUrl == null) throw new NullPointerException("baseUrl may not be null");
		this.baseDir = baseDir;
		this.baseUrl = baseUrl;
	}
	
	public AbstractSitemapGeneratorOptions(URL baseUrl) {
		this(baseUrl, null);
	}
	
	/** The prefix of the name of the sitemaps we'll create; by default this is "sitemap" */
	public THIS fileNamePrefix(String fileNamePrefix) {
		if (fileNamePrefix == null) throw new NullPointerException("fileNamePrefix may not be null");
		this.fileNamePrefix = fileNamePrefix;
		return getThis();
	}

	public THIS suffixStringPattern(String pattern) {
		this.suffixStringPattern = pattern;
		return getThis();
	}

	/**
	 * Permit writing a sitemap that contains no URLs.
	 *
	 * @param allowEmpty {@code true} if an empty sitemap is permissible
	 * @return this instance, for chaining
	 */
	public THIS allowEmptySitemap(boolean allowEmpty) {
		this.allowEmptySitemap = allowEmpty;
		return getThis();
	}

	/** When more than the maximum number of URLs are passed in, should we split into multiple sitemaps automatically, or just throw an exception? */
	public THIS allowMultipleSitemaps(boolean allowMultipleSitemaps) {
		this.allowMultipleSitemaps = allowMultipleSitemaps;
		return getThis();
	}
	/** The date formatter, typically configured with a {@link W3CDateFormat.Pattern} and/or a time zone */
	public THIS dateFormat(W3CDateFormat dateFormat) {
		this.dateFormat = dateFormat;
		return getThis();
	}
	/**
	 * The maximum number of URLs to allow per sitemap; the default is the
	 * maximum allowed (50,000), but you can decrease it if you wish (to make
	 * your auto-generated sitemaps smaller)
	 */
	public THIS maxUrls(int maxUrls) {
		if (maxUrls > SitemapGenerator.MAX_URLS_PER_SITEMAP) {
			throw new RuntimeException("You can only have 50,000 URLs per sitemap; to use more, allowMultipleSitemaps and generate a sitemap index. You asked for " + maxUrls);
		}
		this.maxUrls = maxUrls;
		return getThis();
	}
	/**
	 * Validate the sitemaps automatically after writing them; this takes time (and may fail for Google-specific sitemaps)
	 */
	public THIS autoValidate(boolean autoValidate) {
		this.autoValidate = autoValidate;
		return getThis();
	}
	/** Gzip the sitemaps after they are written to disk */
	public THIS gzip(boolean gzip) {
		this.gzip = gzip;
		return getThis();
	}
	
	@SuppressWarnings("unchecked")
	THIS getThis() {
		return (THIS)this;
	}
}
