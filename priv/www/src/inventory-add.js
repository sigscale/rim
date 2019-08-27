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

class inventoryAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
		<paper-dialog class="dialog" id="addInventoryModal" modal>
			<paper-toolbar>
				<div slot="top"><h2>Add Inventory</h2></div>
			</paper-toolbar>
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
			id="inventoryAddAjax"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_inventoryAddResponse"
			on-error="_inventoryAddError">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			inventory: {
				type: Object,
			}
		}
	}

	ready() {
		super.ready()
	}

	_addInventory() {
		var ajax = this.$.inventoryAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v3/resource/";
		var inv = new Object();
		if(this.$.inventoryName.value) {
			inv.name = this.$.inventoryName.value;
		}
		if(this.$.inventoryDesc.value) {
			inv.description = this.$.inventoryDesc.value;
		}
		if(this.$.inventoryVersion.value) {
			inv.version = this.$.inventoryVersion.value;
		}
		if(this.$.inventoryType.value) {
			inv.type = this.$.inventoryType.value;
		}
		if(this.$.inventoryStatus.value) {
			inv.status = this.$.inventoryStatus.value;
		}
		if(this.$.inventoryCategory.value) {
			inv.category = this.$.inventoryCategory.value;
		}
		if(this.$.inventorySpecification.value) {
			inv.specification = this.$.inventorySpecification.value;
		}
		ajax.body = JSON.stringify(inv);
		ajax.generateRequest();
	}

	_inventoryAddResponse() {
		document.body.querySelector('inventory-management').shadowRoot.querySelector('inventory-add').shadowRoot.getElementById('addInventoryModal').close();
	}
}

window.customElements.define('inventory-add', inventoryAdd);