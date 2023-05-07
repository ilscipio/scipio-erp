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
import java.net.URL;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/** Container for optional URL parameters */
//that weird thing with generics is so sub-classed objects will return themselves
//It makes sense, I swear! http://madbean.com/2004/mb2004-3/
abstract class AbstractSitemapUrlOptions<U extends WebSitemapUrl, THIS extends AbstractSitemapUrlOptions<U,THIS>> {
	Date lastMod;
	ChangeFreq changeFreq;
	Double priority;
	URL url;
	Class<U> clazz;
	List<AltLink> altLinks; // SCIPIO: 3.0.0: Added
	
	public AbstractSitemapUrlOptions(String url, Class<U> clazz) throws MalformedURLException {
		this(new URL(url), clazz);
	}
	
	public AbstractSitemapUrlOptions(URL url, Class<U> clazz) {
		if (url == null) throw new NullPointerException("URL may not be null");
		this.url = url;
		this.clazz = clazz;
	}
	
	/**
	 * The date of last modification of the file. Note that this tag is
	 * separate from the If-Modified-Since (304) header the server can
	 * return, and search engines may use the information from both sources
	 * differently.
	 */
	public THIS lastMod(Date lastMod) {
		this.lastMod = lastMod;
		return getThis();
	}
	
	/**
	 * The date of last modification of the file. Note that this tag is
	 * separate from the If-Modified-Since (304) header the server can
	 * return, and search engines may use the information from both sources
	 * differently.
	 * @throws ParseException if the string isn't a valid W3C date time
	 * @see W3CDateFormat
	 */
	public THIS lastMod(String lastMod) throws ParseException {
		this.lastMod = new W3CDateFormat().parse(lastMod);
		return getThis();
	}
	
	/**
	 * How frequently the page is likely to change. This value provides
	 * general information to search engines and may not correlate exactly
	 * to how often they crawl the page. The value {@link ChangeFreq#ALWAYS} should be used to
	 * describe documents that change each time they are accessed. The value
	 * {@link ChangeFreq#NEVER} should be used to describe archived URLs.
	 * 
	 * <p>Please note that the
	 * value of this tag is considered a <em>hint</em> and not a command. Even though
	 * search engine crawlers may consider this information when making
	 * decisions, they may crawl pages marked {@link ChangeFreq#HOURLY} less frequently than
	 * that, and they may crawl pages marked {@link ChangeFreq#YEARLY} more frequently than
	 * that. Crawlers may periodically crawl pages marked {@link ChangeFreq#NEVER} so that
	 * they can handle unexpected changes to those pages.</p>
	 */
	public THIS changeFreq(ChangeFreq changeFreq) {
		this.changeFreq = changeFreq;
		return getThis();
	}
	
	/**
	 * <p>The priority of this URL relative to other URLs on your site. Valid
	 * values range from 0.0 to 1.0. This value does not affect how your
	 * pages are compared to pages on other sites-it only lets the search
	 * engines know which pages you deem most important for the crawlers.</p>
	 * 
	 * <p>The default priority of a page is 0.5.</p>
	 * 
	 * <p>Please note that the priority you assign to a page is not likely to
	 * influence the position of your URLs in a search engine's result
	 * pages. Search engines may use this information when selecting between
	 * URLs on the same site, so you can use this tag to increase the
	 * likelihood that your most important pages are present in a search
	 * index.</p>
	 * 
	 * <p>Also, please note that assigning a high priority to all of the URLs
	 * on your site is not likely to help you. Since the priority is
	 * relative, it is only used to select between URLs on your site.</p>
	 */
	public THIS priority(Double priority) {
		if (priority > 1.0) throw new IllegalArgumentException("Priority may not be greater than 1.0: " + priority);
		if (priority < 0) throw new IllegalArgumentException("Priority may not be less than 0: " + priority);
		this.priority = priority;
		return getThis();
	}

	public AbstractSitemapUrlOptions<U, THIS> altLinks(List<AltLink> altLinks) {
		this.altLinks = altLinks;
		return this;
	}

	public AbstractSitemapUrlOptions<U, THIS> addAltLink(AltLink altLink) {
		if (altLinks == null) {
			altLinks = new ArrayList<>();
		}
		altLinks.add(altLink);
		return this;
	}

	@SuppressWarnings("unchecked")
	THIS getThis() {
		return (THIS)this;
	}
	
	/** Return an URL based on these settings */
	public U build() {
		try {
			return clazz.getConstructor(getClass()).newInstance(this);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

}