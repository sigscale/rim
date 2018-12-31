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
import '@vaadin/vaadin-grid/vaadin-grid.js';
import './style-element.js';

class userList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<vaadin-grid
					id="userGrid">
				<vaadin-grid-column>
					<template class="header">
						User name
					</template>
					<template>
						[[item.id]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Language
					</template>
					<template>
						[[item.language]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
		`;
	}

	static get properties() {
		return {
			etag: {
				type: String,
				value: null
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('userGrid');
		grid.dataProvider = this._getLog;
	}

	_getLog(params, callback) {
	}
}

window.customElements.define('user-list', userList);
