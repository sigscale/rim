/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import { setPassiveTouchGestures, setRootPath } from '@polymer/polymer/lib/utils/settings.js';
import '@polymer/app-layout/app-drawer/app-drawer.js';
import '@polymer/app-layout/app-drawer-layout/app-drawer-layout.js';
import '@polymer/app-layout/app-header/app-header.js';
import '@polymer/app-layout/app-header-layout/app-header-layout.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/app-route/app-location.js';
import '@polymer/app-route/app-route.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/iron-selector/iron-selector.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/paper-toast/paper-toast.js';
import '@polymer/iron-collapse/iron-collapse.js';
import './inventory-management-icons.js';
import './style-element.js';

// Gesture events like tap and track generated from touch will not be
// preventable, allowing for better scrolling performance.
setPassiveTouchGestures(true);

// Set Polymer's root path to the same value we passed to our service worker
// in `index.html`.
setRootPath(MyAppGlobals.rootPath);

class InventoryManagement extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<app-location
					route="{{route}}"
					url-space-regex="^[[rootPath]]">
			</app-location>
			<app-route
					route="{{route}}"
					pattern="[[rootPath]]:page"
					data="{{routeData}}"
					tail="{{subroute}}">
			</app-route>
			<app-drawer-layout
					force-narrow
					fullbleed>
				<app-header-layout
						has-scrolling-region>
					<app-header
							slot="header"
							condenses
							reveals
							effects="waterfall">
						<app-toolbar
								class="toolbar-top">
							<paper-icon-button
									icon="my-icons:menu"
									drawer-toggle>
							</paper-icon-button>
							<div main-title>[[viewTitle]]</div>
							<paper-icon-button
									icon="my-icons:refresh"
									on-click="refreshClick">
							</paper-icon-button>
							<paper-icon-button
									toggles
									id="overFlowIcon"
									active="{{overFlowActive}}"
									on-click="_overFlowMenu"
									icon="my-icons:overFlowMenu">
							</paper-icon-button>
						</app-toolbar>
						<paper-progress
							indeterminate
							class="slow red"
							disabled="{{!loading}}">
						</paper-progress>
					</app-header>
					<iron-pages
							id="load"
							selected="[[page]]"
							attr-for-selected="name"
							role="main">
						<catalog-list
								id="catalogList"
								loading="{{catalogLoading}}"
								name="catalogView"
								active-item="{{activeCatalogItem}}">
						</catalog-list>
						<category-list
								id="categoryList"
								loading="{{categoryLoading}}"
								name="categoryView"
								active-item="{{activeCategoryItem}}">
						</category-list>
						<candidate-list
								id="candidateList"
								loading="{{candidateLoading}}"
								name="candidateView"
								active-item="{{activeCandidateItem}}">
						</candidate-list>
						<specification-list
								id="specificationList"
								loading="{{specificationLoading}}"
								name="specificationView"
								active-item="{{activeSpecificationItem}}">
						</specification-list>
						<rule-list
								id="ruleList"
								loading="{{ruleLoading}}"
								name="ruleView"
								active-item="{{activeRuleItem}}">
						</rule-list>
						<inventory-list
								id="inventoryList"
								loading="{{inventoryLoading}}"
								name="inventoryView"
								graph-size="[[graphSize]]"
								active-item="{{activeInventoryItem}}">
						</inventory-list>
						<user-list
								id="userList"
								loading="{{userLoading}}"
								name="userView"
								active-item="{{activeUserItem}}">
						</user-list>
						<http-list
								id="httpList"
								loading="{{httpLoading}}"
								name="httpView"
						</http-list>
					</iron-pages>
					<paper-toast
							id="restError"
							class="fit-bottom"
							duration="8000">
					</paper-toast>
				</app-header-layout>
				<app-drawer
						id="drawer"
						slot="drawer">
					<iron-selector
							selected="[[page]]"
							attr-for-selected="name"
							class="drawer-list"
							role="navigation">
						<a href="" on-click="_collapseCatalog">
							<paper-icon-button
									icon="my-icons:resourceCatalog">
							</paper-icon-button>
							Catalog
						</a>
						<iron-collapse id="catalog">
							<a name="catalogView" href="[[rootPath]]catalogView">
								<paper-icon-button
									icon="my-icons:catalog">
								</paper-icon-button>
								Catalog
							</a>
							<a name="categoryView" href="[[rootPath]]categoryView">
								<paper-icon-button
									icon="my-icons:category">
								</paper-icon-button>
								Category
							</a>
							<a name="candidateView" href="[[rootPath]]candidateView">
								<paper-icon-button
									icon="my-icons:candidate">
								</paper-icon-button>
								Candidate
							</a>
							<a name="specificationView" href="[[rootPath]]specificationView">
								<paper-icon-button
									icon="my-icons:specification">
								</paper-icon-button>
								Specification
							</a>
						</iron-collapse>
						<a name="ruleView" href="[[rootPath]]ruleView">
							<paper-icon-button
									icon="my-icons:rule">
							</paper-icon-button>
							Rule
						</a>
						<a name="inventoryView" href="[[rootPath]]inventoryView">
							<paper-icon-button
									icon="my-icons:inventory">
							</paper-icon-button>
							Inventory
						</a>
						<a name="userView" href="[[rootPath]]userView">
							<paper-icon-button
									icon="my-icons:user">
							</paper-icon-button>
							User
						</a>
						<a name="httpView" href="[[rootPath]]httpView">
							<paper-icon-button
									icon="my-icons:data">
							</paper-icon-button>
							HTTP
						</a>
					</iron-selector>
				</app-drawer>
			</app-drawer-layout>
			<!-- Modal Definitions -->
			<catalog-update id="catalogUpdate" active-item="[[activeCatalogItem]]"></catalog-update>
			<catalog-add id="catalogAdd"></catalog-add>
			<candidate-update id="candidateUpdate" active-item="[[activeCandidateItem]]"></candidate-update>
			<candidate-add id="candidateAdd"></candidate-add>
			<category-update id="categoryUpdate" active-item="[[activeCategoryItem]]"></category-update>
			<category-add id="categoryAdd"></category-add>
			<specification-update id="specificationUpdate" active-item="[[activeSpecificationItem]]"></specification-update>
			<specification-add id="specificationAdd"></specification-add>
			<rule-update id="ruleUpdate" active-item="[[activeRuleItem]]"></rule-update>
			<inventory-add id="inventoryAdd"></inventory-add>
			<inventory-help id="inventoryGetHelp" active="[[overFlowActive]]"></inventory-help>
			<inventory-topology id="topologyGraph" graph-size={{graphSize}}></inventory-topology>
		`;
	}

	_collapseCatalog(event) {
		var cat = document.body.querySelector('inventory-management').shadowRoot.getElementById('catalog');
		if(cat.opened == false) {
			cat.show();
		} else {
			cat.hide();
		}
	}

	_overFlowMenu() {
		import('./inventory-help.js');
	}

	refreshClick() {
		var grid;
		switch(this.$.load.selected) {
			case "catalogView":
				var catalog = this.shadowRoot.getElementById('catalogList');
				if (!catalog.loading) {
					grid = catalog.shadowRoot.getElementById('catalogGrid');
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "categoryView":
				var category = this.shadowRoot.getElementById('categoryList');
				if (!category.loading) {
					grid = category.shadowRoot.getElementById('categoryGrid');
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "candidateView":
				var candidate = this.shadowRoot.getElementById('candidateList');
				if (!candidate.loading) {
					grid = candidate.shadowRoot.getElementById('candidateGrid');
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "specificationView":
				var specification = this.shadowRoot.getElementById('specificationList');
				if (!specification.loading) {
					grid = specification.shadowRoot.getElementById('specificationGrid');
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "inventoryView":
				var inventory = this.shadowRoot.getElementById('inventoryList');
				if (!inventory.loading) {
					grid = inventory.shadowRoot.getElementById('inventoryGrid');
					for(var index in grid.detailsOpenedItems) {
						grid.closeItemDetails(grid.detailsOpenedItems[index]);
					}
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "ruleView":
				var rule = this.shadowRoot.getElementById('ruleList');
				if (!rule.loading) {
					grid = rule.shadowRoot.getElementById('ruleGrid');
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "httpView":
				var http = this.shadowRoot.getElementById('httpList');
				if (!http.loading) {
					grid = http.shadowRoot.getElementById('httpGrid');
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "userView":
				var user = this.shadowRoot.getElementById('userList');
				if (!user.loading) {
					grid = user.shadowRoot.getElementById('userGrid');
					grid.size = undefined;
					grid.clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
		}
	}

	static get properties() {
		return {
			page: {
				type: String,
				reflectToAttribute: true,
				observer: '_pageChanged'
			},
			routeData: Object,
			ubroute: Object,
			viewTitle: {
				type: String
			},
			loading: {
				type: Boolean,
				value: false
			},
			dashLoading: {
				type: Boolean
			},
			catalogLoading: {
				type: Boolean
			},
			categoryLoading: {
				type: Boolean
			},
			candidateLoading: {
				type: Boolean
			},
			specificationLoading: {
				type: Boolean
			},
			ruleLoading: {
				type: Boolean
			},
			inventoryLoading: {
				type: Boolean
			},
			httpLoading: {
				type: Boolean
			},
			userLoading: {
				type: Boolean
			}
		};
	}

	static get observers() {
		return [
			'_routePageChanged(routeData.page)',
			'_loadingChanged(userLoading, catalogLoading, categoryLoading, candidateLoading, specificationLoading, inventoryLoading, httpLoading)'
		];
	}

	_routePageChanged(page) {
		// Show the corresponding page according to the route.
		//
		// If no page was found in the route data, page will be an empty string.
		// Show 'inventoryView' in that case. And if the page doesn't exist, show 'view404'.
		if (!page) {
			this.page = 'catalogView';
		} else if (['ruleView', 'inventoryView', 'catalogView', 'categoryView', 'candidateView', 'userView', 'specificationView', 'httpView'].indexOf(page) !== -1) {
			this.page = page;
		}
		switch (this.page) {
			case 'catalogView':
				this.viewTitle = 'Resource Catalogs';
				break;
			case 'categoryView':
				this.viewTitle = 'Resource Categories';
				break;
			case 'candidateView':
				this.viewTitle = 'Resource Candidates';
				break;
			case 'specificationView':
				this.viewTitle = 'Resource Specifications';
				break;
			case 'inventoryView':
				this.viewTitle = 'Resource Inventory';
				break;
			case 'ruleView':
				this.viewTitle = 'Rules';
				break;
			case 'httpView':
				this.viewTitle = "HTTP Log";
				break;
			case 'userView':
				this.viewTitle = 'Users';
				break;
		}
		// Close a non-persistent drawer when the page & route are changed.
		if (!this.$.drawer.persistent) {
			this.$.drawer.close();
		}
	}

	_pageChanged(page) {
		// Import the page component on demand.
		//
		// Note: `polymer build` doesn't like string concatenation in the import
		// statement, so break it up.
		switch (page) {
			case 'catalogView':
				import('./catalog-update.js');
				import('./catalog-add.js');
				import('./catalog-list.js');
				break;
			case 'categoryView':
				import('./category-update.js');
				import('./category-add.js');
				import('./category-list.js');
				break;
			case 'candidateView':
				import('./candidate-update.js');
				import('./candidate-add.js');
				import('./candidate-list.js');
				break;
			case 'specificationView':
				import('./specification-update.js');
				import('./specification-add.js');
				import('./specification-list.js');
				break;
			case 'inventoryView':
				import('./inventory-add.js');
				import('./inventory-list.js');
				import('./inventory-topology.js');
				break;
			case 'ruleView':
				import('./rule-update.js');
				import('./rule-list.js');
				break;
			case 'userView':
				import('./user-list.js');
				break;
			case 'httpView':
				import('./http-list.js');
				break;
		}
	}

	_loadingChanged() {
		if (this.userLoading || this.ruleLoading || this.inventoryLoading || this.catalogLoading || this.categoryLoading || this.candidateLoading || this.specificationLoading || this.httpLoading) {
			this.loading = true;
		} else {
			this.loading = false;
		}
	}
}

window.customElements.define('inventory-management', InventoryManagement);
