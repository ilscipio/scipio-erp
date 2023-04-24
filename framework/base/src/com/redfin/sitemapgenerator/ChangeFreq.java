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

/**
 * How frequently the page is likely to change. This value provides
 * general information to search engines and may not correlate exactly
 * to how often they crawl the page. The value {@link #ALWAYS} should be used to
 * describe documents that change each time they are accessed. The value
 * {@link #NEVER} should be used to describe archived URLs.
 * 
 * <p>Please note that the
 * value of this tag is considered a <em>hint</em> and not a command. Even though
 * search engine crawlers may consider this information when making
 * decisions, they may crawl pages marked {@link #HOURLY} less frequently than
 * that, and they may crawl pages marked {@link #YEARLY} more frequently than
 * that. Crawlers may periodically crawl pages marked {@link #NEVER} so that
 * they can handle unexpected changes to those pages.</p>
 */
public enum ChangeFreq {
	ALWAYS, HOURLY, DAILY, WEEKLY, MONTHLY, YEARLY, NEVER;
	String lowerCase;
	private ChangeFreq() {
		lowerCase = this.name().toLowerCase();
	}
	
	@Override
	public String toString() {
		return lowerCase;
	}
}
