/*
 * @license
 * Copyright 2018 - 2023 SigScale Global Inc.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-input/paper-textarea.js';
import '@polymer/paper-button/paper-button.js';
import './style-element.js';

class inventoryAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
		<paper-dialog class="dialog" id="inventoryAddModal" modal>
			<app-toolbar>
				<div main-title>Add Inventory</div>
			</app-toolbar>
			<paper-progress
					indeterminate
					class="slow red"
					disabled="{{!loading}}">
			</paper-progress>
			<paper-textarea
					label="Name"
					value="{{inventoryName}}">
			</paper-textarea>
			<paper-input
					label="Description"
					value="{{inventoryDescription}}">
			</paper-input>
			<paper-input
					label="Version"
					value="{{inventoryVersion}}">
			</paper-input>
			<paper-input
					label="Type"
					value="{{inventoryType}}">
			</paper-input>
			<paper-input
					label="Status"
					value="{{inventoryStatus}}">
			</paper-input>
			<paper-input
					label="Category"
					value="{{inventoryCategory}}">
			</paper-input>
			<paper-input
					label="Specification"
					value="{{inventorySpecification}}">
			</paper-input>
			<div class="buttons">
				<paper-button
						raised
						class="submit-button"
						on-tap="_add">
					Add
				</paper-button>
				<paper-button
						class="cancel-button"
						on-tap="_cancel">
					Cancel
				</paper-button>
			</div>
		</paper-dialog>
		<iron-ajax
				id="inventoryAddAjax"
				content-type="application/json"
				loading="{{loading}}"
				on-response="_response"
				on-error="_error">
		</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			inventoryName: {
				type: String
			},
			inventoryDescription: {
				type: String
			},
			inventoryType: {
				type: String
			},
			inventoryStatus: {
				type: String
			},
			inventoryVersion: {
				type: String
			},
			inventoryCategory: {
				type: String
			},
			inventorySpecification: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancel() {
		this.$.inventoryAddModal.close();
		this.inventoryName = null;
		this.inventoryDescription = null;
		this.inventoryType = null;
		this.inventoryStatus = null;
		this.inventoryVersion = null;
		this.inventoryCategory = null;
		this.inventorySpecification = null;
	}

	_add() {
		var ajax = this.$.inventoryAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v4/resource/";
		var inv = new Object();
		if(this.inventoryName) {
			inv.name = this.inventoryName;
		}
		if(this.inventoryDescription) {
			inv.description = this.inventoryDescription;
		}
		if(this.inventoryVersion) {
			inv.version = this.inventoryVersion;
		}
		if(this.inventoryType) {
			inv['@type'] = this.inventoryType;
		}
		if(this.inventoryStatus) {
			inv.lifecycleStatus = this.inventoryStatus;
		}
		if(this.inventoryCategory) {
			inv.category = this.inventoryCategory;
		}
		if(this.inventorySpecification) {
			inv.category = this.inventorySpecification;
		}
		ajax.body = JSON.stringify(inv);
		ajax.generateRequest();
	}

	_response() {
		this.$.inventoryAddModal.close();
		this.inventoryName = null;
		this.inventoryDescription = null;
		this.inventoryType = null;
		this.inventoryStatus = null;
		this.inventoryVersion = null;
		this.inventoryCategory = null;
		this.inventorySpecification = null;
		document.body.querySelector('inventory-management').shadowRoot.getElementById('inventoryList').shadowRoot.getElementById('inventoryGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('inventory-add', inventoryAdd);
