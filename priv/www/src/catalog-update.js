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
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
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
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateCatalogModal" modal>
				<app-toolbar>
					<div main-title>Update Catalog</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<paper-input
							id="catalogId"
							label="Id"
							value="{{catalog.catalogId}}"
							disabled>
					</paper-input>
					<paper-input
							id="catalogName"
							label="Name"
							value="{{catalog.catalogName}}"
							required>
					</paper-input>
					<paper-input
							id="catalogDesc"
							label="Description"
							value="{{catalog.catalogDescription}}">
					</paper-input>
					<paper-input
							id="catalogType"
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
								on-tap="_updateCatalog">
							Update
						</paper-button>
						<paper-button
								class="cancel-button"
								dialog-dismiss>
							Cancel
						</paper-button>
						<paper-button
								toggles
								raised
								class="delete-button"
								on-tap="_deleteCatalog">
							Delete
						</paper-button>
					</div>
			</paper-dialog>
			<iron-ajax
				id="deleteCatalogAjax"
				loading="{{loading}}"
				on-response="_catalogUpdateResponse"
				on-error="_catalogUpdateError">
			</iron-ajax>
			<iron-ajax
				id="catalogUpdateAjax"
				content-type="application/merge-patch+json"
				loading="{{loading}}"
				on-response="_catalogUpdateResponse"
				on-error="_catalogUpdateError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			catalog: {
				type: Object,
			}
		}
	}

	ready() {
		super.ready()
	}

	_delete() {
		var ajax = this.$.deleteCatalogAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceCatalogManagement/v3/resourceCatalog/" + this.$.catalogId.value;
		ajax.generateRequest();
	}

	_update() {
		var ajax = this.$.catalogUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/resourceCatalogManagement/v3/resourceCatalog/" + this.$.catalogId.value;
		var cat = new Object();
		if(this.$.catalogName.value) {
			cat.name = this.$.catalogName.value;
		}
		if(this.$.catalogDesc.value) {
			cat.description = this.$.catalogDesc.value;
		}
		if(this.$.catalogType) {
			cat["@type"] = this.$.catalogType.value;
		}
		if(this.$.catalogStatus.value) {
			cat.lifecycleStatus = this.$.catalogStatus.value;
		}
		ajax.body = JSON.stringify(cat);
		ajax.generateRequest();
	}

	_catalogUpdateResponse() {
		var shell = document.body.querySelector('inventory-management').shadowRoot;
		shell.querySelector('catalog-update').shadowRoot.getElementById('updateCatalogModal').close();
		shell.getElementById('catalogList').shadowRoot.getElementById('catalogGrid').clearCache();
	}

	_catalogUpdateError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('catalog-update', catalogUpdateList);
