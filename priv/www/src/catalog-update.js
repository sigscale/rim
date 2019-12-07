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
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/paper-toolbar/paper-toolbar.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class catalogUpdateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="updateCatalogModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Update Catalog</h2></div>
			</paper-toolbar>
				<paper-input
						id="catalogSpecId"
						label="Id"
						value="{{catalog.catalogId}}"
						disabled>
				</paper-input>
				<paper-input
						id="catalogSpecName"
						label="Name"
						value="{{catalog.catalogName}}"
						required>
				</paper-input>
				<paper-input
						id="catalogSpecDesc"
						label="Description"
						value="{{catalog.catalogDescription}}">
				</paper-input>
				<paper-input
						id="catalogSpecType"
						label="Class"
						value="{{catalog.catalogClass}}">
				</paper-input>
				<paper-dropdown-menu
					id="catalogStatus" 
					class="drop"
					label="Status"
					value="{{catalog.catalogStatus}}"
					no-animations="true">
					<paper-listbox
							id="updateStatus"
							slot="dropdown-content">
						<paper-item>In Study</paper-item>
						<paper-item>In Design</paper-item>
						<paper-item>In Test</paper-item>
						<paper-item>Rejected</paper-item>
						<paper-item>Active</paper-item>
						<paper-item>Launched</paper-item>
						<paper-item>Retired</paper-item>
						<paper-item>Obsolete</paper-item>
					</paper-listbox>
				</paper-dropdown-menu>
				<div class="buttons">
					<paper-button
							raised
							class="update-button"
							on-tap="_updateSpec">
						Update
					</paper-button>
					<paper-button
							class="cancel-button"
							dialog-dismiss
							on-tap="cancelSpec">
						Cancel
					</paper-button>
					<paper-button
							toggles
							raised
							class="delete-button"
							on-tap="_deleteSpec">
						Delete
					</paper-button>
				</div>
		</paper-dialog>
		<iron-ajax
			id="deleteCatalogAjax"
			on_response="_deleteCatalogResponse"
			on-error="_catalogUpdateError">
		</iron-ajax>
		<iron-ajax
			id="catalogUpdateAjax"
			content-type="application/merge-patch+json"
			on-loading-changed="_onLoadingChanged"
			on-response="_catalogSpecResponse"
			on-error="_catalogUpdateError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			catalog: {
				type: Object,
			}
		}
	}

	ready() {
		super.ready()
	}

	_deleteSpec() {
		var ajax1 = this.$.deleteCatalogAjax;
		ajax1.method = "DELETE";
		ajax1.url = "/resourceCatalogManagement/v3/resourceCatalog/" + this.$.catalogSpecId.value;
		ajax1.generateRequest();
		var deleteObj =  document.body.querySelector('inventory-management').shadowRoot.querySelector('catalog-update').shadowRoot.getElementById('updateCatalogModal');
		deleteObj.close();
	}

	_deleteCatalogResponse() {
		document.getElementById("catalogGrid").clearCache();
	}

	_updateSpec() {
		var ajax = this.$.catalogUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v3/resourceCatalog/" + this.$.catalogSpecId.value;
		var cat = new Object();
		if(this.$.catalogSpecName.value) {
			cat.name = this.$.catalogSpecName.value;
		}
		if(this.$.catalogSpecDesc.value) {
			cat.description = this.$.catalogSpecDesc.value;
		}
		if(this.$.catalogSpecType) {
			cat["@type"] = this.$.catalogSpecType.value;
		}
		if(this.$.catalogStatus.value) {
			cat.lifecycleStatus = this.$.catalogStatus.value;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_catalogSpecResponse() {
		document.getElementById("catalogGrid").clearCache();
	}

	_catalogUpdateError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('catalog-update', catalogUpdateList);
