/**
 * @license
 * Copyright (c) 2016 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-sorter.js';
import './style-element.js';

class rulesList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="rulesGrid"
					loading="{{loading}}">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="id">
							<vaadin-grid-filter
									id="filter"
									aria-label="Id"
									path="id"
									value="{{_filterRulesId}}">
								<input
										slot="filter"
										placeholder="Id"
										value="{{_filterRulesId::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
               <template>
						[[item.rulesId]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="description">
							<vaadin-grid-filter
									id="filter"
									aria-label="Description"
									path="description"
									value="{{_filterRulesDescription}}">
								<input
										slot="filter"
										placeholder="Description"
										value="{{_filterRulesDescription::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
               <template>
						[[item.rulesDescription]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-sorter
								path="rules">
							<vaadin-grid-filter
									id="filter"
									aria-label="Rules"
									path="rules"
									value="{{_filterRules}}">
								<input
										slot="filter"
										placeholder="Rules"
										value="{{_filterRules::input}}"
										focus-target>
							</vaadin-grid-filter>
						</vaadin-grid-sorter>
					</template>
               <template>
						[[item.rules]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddRulesModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getRulesAjax"
				url="resourceCatalogManagement/v3/resourceRules"
				rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true
			},
			etag: {
				type: String,
				value: null
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('rulesGrid');
		grid.dataProvider = this._getLog;
	}

	_getLog() {
	}
}

window.customElements.define('rules-list', rulesList);
