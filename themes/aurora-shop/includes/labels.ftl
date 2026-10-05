<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#--
SCIPIO: 4.0.0: labels of the Aurora Shop theme (English, German). A theme has no label resource on the class
path, so the few storefront texts live here; asL(key) falls back to English, then to the key.
-->
<#assign asLabels = {
"en": {
  "AuroraShopNewsletter": "New arrivals, first.",
  "AuroraShopSubscribe": "Subscribe to the newsletter",
  "AuroraShopShopNow": "Shop now",
  "AuroraShopNewArrivals": "New arrivals",
  "AuroraShopSeeAll": "See all",
  "AuroraShopMarketplace": "Marketplace",
  "AuroraShopMeetMakers": "Meet the new makers",
  "AuroraShopMeetMakersText": "Independent sellers who joined recently. Each one is a verified business.",
  "AuroraShopSeeSellers": "See new sellers",
  "AuroraShopFeatured": "Featured",
  "AuroraShopViewProduct": "View product",
  "AuroraShopThisSeason": "This season",
  "AuroraShopVisitShop": "Visit shop",
  "AuroraShopNoSmallPrint": "No small print",
  "AuroraShopKnowWhatYouBuy": "Know what you buy.",
  "AuroraShopKnowText": "Each product lists its manufacturer, its safety information and its packaging. Each order in the EU has a right of withdrawal, with a withdrawal button that is always one click away.",
  "AuroraShopWhoMadeIt": "Who made it",
  "AuroraShopWhoMadeItText": "Manufacturer and EU contact on every product page.",
  "AuroraShopHonestPrices": "Honest prices",
  "AuroraShopHonestPricesText": "A sale price always shows the lowest price of the last 30 days.",
  "AuroraShopWithdraw": "Withdraw in two clicks",
  "AuroraShopWithdrawText": "Fill in the form, confirm, and get a receipt by e-mail.",
  "AuroraShopYourData": "Your data, your call",
  "AuroraShopYourDataText": "Nothing tracks you before you agree. We honour GPC.",
  "AuroraShopPrevious": "Previous",
  "AuroraShopNext": "Next",
  "AuroraShopPause": "Pause",
  "AuroraShopSlide": "Slide",
  "AuroraShopRooms": "Categories"
},
"de": {
  "AuroraShopNewsletter": "Neuheiten zuerst.",
  "AuroraShopSubscribe": "Newsletter abonnieren",
  "AuroraShopShopNow": "Jetzt entdecken",
  "AuroraShopNewArrivals": "Neu eingetroffen",
  "AuroraShopSeeAll": "Alle ansehen",
  "AuroraShopMarketplace": "Marktplatz",
  "AuroraShopMeetMakers": "Die neuen Hersteller",
  "AuroraShopMeetMakersText": "Unabh\x00E4ngige Verk\x00E4ufer, die neu dabei sind. Jeder ist ein gepr\x00FCftes Unternehmen.",
  "AuroraShopSeeSellers": "Neue Verk\x00E4ufer ansehen",
  "AuroraShopFeatured": "Empfohlen",
  "AuroraShopViewProduct": "Produkt ansehen",
  "AuroraShopThisSeason": "Diese Saison",
  "AuroraShopVisitShop": "Shop besuchen",
  "AuroraShopNoSmallPrint": "Kein Kleingedrucktes",
  "AuroraShopKnowWhatYouBuy": "Wissen, was man kauft.",
  "AuroraShopKnowText": "Jedes Produkt nennt Hersteller, Sicherheitshinweise und Verpackung. Jede Bestellung in der EU hat ein Widerrufsrecht mit einem Widerrufsbutton, der immer einen Klick entfernt ist.",
  "AuroraShopWhoMadeIt": "Wer es gemacht hat",
  "AuroraShopWhoMadeItText": "Hersteller und EU-Kontakt auf jeder Produktseite.",
  "AuroraShopHonestPrices": "Ehrliche Preise",
  "AuroraShopHonestPricesText": "Ein Sonderpreis zeigt immer den niedrigsten Preis der letzten 30 Tage.",
  "AuroraShopWithdraw": "Widerruf in zwei Klicks",
  "AuroraShopWithdrawText": "Formular ausf\x00FCllen, best\x00E4tigen, Best\x00E4tigung per E-Mail.",
  "AuroraShopYourData": "Ihre Daten, Ihre Wahl",
  "AuroraShopYourDataText": "Nichts verfolgt Sie ohne Ihre Zustimmung. Wir beachten GPC.",
  "AuroraShopPrevious": "Zur\x00FCck",
  "AuroraShopNext": "Weiter",
  "AuroraShopPause": "Pause",
  "AuroraShopSlide": "Folie",
  "AuroraShopRooms": "Kategorien"
}}>
<#function asL key>
  <#local lang = (locale.getLanguage())!"en">
  <#return ((asLabels[lang]!asLabels.en)[key])!((asLabels.en[key])!key)>
</#function>
