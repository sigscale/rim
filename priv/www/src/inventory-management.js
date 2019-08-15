/**
 * @license
 * Copyright (c) 2019 The Polymer Project Authors. All rights reserved.
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
import '@polymer/app-layout/app-scroll-effects/app-scroll-effects.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-styles/typography.js';
import '@polymer/app-route/app-location.js';
import '@polymer/app-route/app-route.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/iron-selector/iron-selector.js';
import '@polymer/paper-icon-button/paper-icon-button.js';
import '@polymer/paper-progress/paper-progress.js';
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
			<style include="style-element">
			</style>
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
								name="catalogView">
						</catalog-list>
						<category-list
								id="categoryList"
								loading="{{categoryLoading}}"
								name="categoryView">
						</category-list>
						<candidate-list
								id="candidateList"
								loading="{{candidateLoading}}"
								name="candidateView">
						</candidate-list>
						<specification-list
								id="specificationList"
								loading="{{specificationLoading}}"
								name="specificationView"
								active-item="{{activeItem}}">
						</specification-list>
						<rules-list
								id="rulesList"
								loading="{{rulesLoading}}"
								name="rulesView">
						</rules-list>
						<inventory-list
								id="inventoryList"
								loading="{{inventoryLoading}}"
								name="inventoryView">
						</inventory-list>
						<user-list
								id="userList"
								loading="{{userLoading}}"
								name="userView">
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
						<a href="" on-click="_collapseLogs">
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
						<a name="rulesView" href="[[rootPath]]rulesView">
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
			<!-- Model Definitions -->
			<specification-update id="updateSpec" specification="[[activeItem]]"></specification-update>
			<specification-add id="addSpecification"></specification-add>
			<catalog-update id="updateCatalog" catalog="[[activeItem]]"></catalog-update>
			<catalog-add id="addCatalog"></catalog-add>
			<candidate-update id="updateCandidate" candidate="[[activeItem]]"></candidate-update>
			<candidate-add id="addCandidate"></candidate-add>
			<category-update id="updateCategory" category="[[activeItem]]"></category-update>
			<category-add id="addCategory"></category-add>
			<rules-update id="updateRule" category="[[activeItem]]"></rules-update>
			<inventory-help id="inventoryGetHelp" active="[[overFlowActive]]"></inventory-help>
		`;
	}

	_collapseLogs(event) {
		var im = document.body.querySelector('inventory-management')
		var catObj = im.shadowRoot.getElementById('catalog');
		if(catObj.opened == false) {
			catObj.show();
		} else {
			catObj.hide();
		}
	}

	_overFlowMenu() {
		import('./inventory-help.js');
	}

	refreshClick() {
		switch(this.$.load.selected) {
			case "catalogView":
				var catalog = this.shadowRoot.getElementById('catalogList');
				if (!catalog.loading) {
					catalog.shadowRoot.getElementById('catalogGrid').clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "categoryView":
				var category = this.shadowRoot.getElementById('categoryList');
				if (!category.loading) {
					category.shadowRoot.getElementById('categoryGrid').clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "candidateView":
				var candidate = this.shadowRoot.getElementById('candidateList');
				if (!candidate.loading) {
					candidate.shadowRoot.getElementById('candidateGrid').clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "specificationView":
				var specification = this.shadowRoot.getElementById('specificationList');
				if (!specification.loading) {
					specification.shadowRoot.getElementById('specificationGrid').clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "inventoryView":
				var inventory = this.shadowRoot.getElementById('inventoryList');
				if (!inventory.loading) {
					inventory.shadowRoot.getElementById('inventoryGrid').clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "rulesView":
				var rules = this.shadowRoot.getElementById('rulesList');
				if (!rules.loading) {
					rules.shadowRoot.getElementById('inventoryGrid').clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "httpView":
				var http = this.shadowRoot.getElementById('httpList');
				if (!http.loading) {
					http.shadowRoot.getElementById('httpGrid').clearCache();
				} else {
					console.log('Have patience dude!');
				}
				break;
			case "userView":
				var user = this.shadowRoot.getElementById('userList');
				if (!user.loading) {
					user.shadowRoot.getElementById('userGrid').clearCache();
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
				type: String,
				value: false
			},
			dashLoading: {
				type: String
			},
			catalogLoading: {
				type: String
			},
			categoryLoading: {
				type: String
			},
			candidateLoading: {
				type: String
			},
			specificationLoading: {
				type: String
			},
			rulesLoading: {
				type: String
			},
			inventoryLoading: {
				type: String
			},
			httpLoading: {
				type: String
			},
			userLoading: {
				type: String
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
		} else if (['rulesView', 'inventoryView', 'catalogView', 'categoryView', 'candidateView', 'userView', 'specificationView', 'httpView'].indexOf(page) !== -1) {
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
			case 'rulesView':
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
				import('./catalog-list.js');
				import('./catalog-update.js');
				import('./catalog-add.js');
				break;
			case 'categoryView':
				import('./category-list.js');
				import('./category-update.js');
				import('./category-add.js');
				break;
			case 'candidateView':
				import('./candidate-list.js');
				import('./candidate-update.js');
				import('./candidate-add.js');
				break;
			case 'specificationView':
				import('./specification-list.js');
				import('./specification-update.js');
				import('./specification-add.js');
				break;
			case 'inventoryView':
				import('./inventory-list.js');
				break;
			case 'rulesView':
				import('./rules-list.js');
				import('./rules-update.js');
				break;
			case 'httpView':
				import('./http-list.js');
				break;
			case 'userView':
				import('./user-list.js');
				break;
		}
	}

	_loadingChanged() {
		if (this.userLoading || this.rulesLoading || this.inventoryLoading || this.catalogLoading || this.categoryLoading || this.candidateLoading || this.specificationLoading || this.httpLoading) {
			this.loading = true;
		} else {
			this.loading = false;
		}
	}
}

window.customElements.define('inventory-management', InventoryManagement);
