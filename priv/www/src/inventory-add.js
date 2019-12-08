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
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class inventoryAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
		<paper-dialog class="dialog" id="addInventoryModal" modal>
			<app-toolbar>
				<div main-title>Add Inventory</div>
			</app-toolbar>
			<paper-progress
					indeterminate
					class="slow red"
					disabled="{{!loading}}">
			</paper-progress>
			<paper-input
					id="inventoryName"
					label="Name"
					value="{{inventory.inventoryName}}">
			</paper-input>
			<paper-input
					id="inventoryDesc"
					label="Description"
					value="{{inventory.inventoryDesc}}">
			</paper-input>
			<paper-input
					id="inventoryVersion"
					label="Version"
					value="{{inventory.inventoryVersion}}">
			</paper-input>
			<paper-input
					id="inventoryType"
					label="Type"
					value="{{inventory.inventoryType}}">
			</paper-input>
			<paper-input
					id="inventoryStatus"
					label="Status"
					value="{{inventory.inventoryStatus}}">
			</paper-input>
			<paper-input
					id="inventoryCategory"
					label="Category"
					value="{{inventory.inventoryCategory}}">
			</paper-input>
			<paper-input
					id="inventorySpecification"
					label="Specification"
					value="{{inventory.inventorySpecification}}">
			</paper-input>
			<div class="buttons">
				<paper-button
						raised
						class="submit-button"
						on-tap="_addInventory">
					Add
				</paper-button>
				<paper-button
						class="cancel-button"
						on-tap="_cancelInventory">
					Cancel
				</paper-button>
			</div>
		</paper-dialog>
		<iron-ajax
				id="inventoryAddAjax"
				content-type="application/json"
				loading="{{loading}}"
				on-response="_inventoryAddResponse"
				on-error="_inventoryAddError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			inventory: {
				type: Object,
				value: function() {
					return {};
				}
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancelInventory() {
		this.$.addInventoryModal.close();
		this.inventory = {};
	}

	_addInventory() {
		var ajax = this.$.inventoryAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v3/resource/";
		var inv = new Object();
		if(this.inventory.name) {
			inv.name = this.inventory.name;
		}
		if(this.inventory.description) {
			inv.description = this.inventory.description;
		}
		if(this.inventory.version) {
			inv.version = this.inventory.version;
		}
		if(this.inventory.version) {
			inv.version = this.inventory.version;
		}
		if(this.inventory.type) {
			inv.type = this.inventory.type;
		}
		if(this.inventory.status) {
			inv.lifecycleStatus = this.inventory.status;
		}
		if(this.inventory.category) {
			inv.category = this.inventory.category;
		}
		ajax.body = JSON.stringify(inv);
		ajax.generateRequest();
	}

	_inventoryAddResponse() {
		this.$.addInventoryModal.close();
		this.inventory = {};
		document.body.querySelector('inventory-management').shadowRoot.getElementById('inventoryList').shadowRoot.getElementById('inventoryGrid').clearCache();
	}

	_inventoryAddError(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('inventory-add', inventoryAdd);
