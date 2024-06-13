/*
 * @license
 * Copyright 2018 - 2024 SigScale Global Inc.
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

class catalogAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="catalogAddModal" modal>
				<app-toolbar>
					<div main-title>Add Catalog</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						label="Name"
						value="{{catalogName}}">
				</paper-input>
				<paper-textarea
						label="Description"
						value="{{catalogDescription}}">
				</paper-textarea>
				<paper-input
						label="Version"
						value="{{catalogVersion}}">
				</paper-input>
				<paper-input
						label="Class"
						value="{{catalogClass}}">
				</paper-input>
				<paper-dropdown-menu
						class="drop"
						label="Status"
						value="{{catalogStatus}}"
						no-animations="true">
					<paper-listbox
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
					id="catalogAddAjax"
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
			catalogName: {
				type: String
			},
			catalogDescription: {
				type: String
			},
			catalogVersion: {
				type: String
			},
			catalogClass: {
				type: String
			},
			catalogStatus: {
				type: String
			}
		}
	}

   ready() {
      super.ready()
	}

	_cancel() {
		this.$.catalogAddModal.close();
		this.catalogName = null;
		this.catalogDescription = null;
		this.catalogVersion = null;
		this.catalogClass = null;
		this.catalogStatus = null;
	}

	_add() {
		var ajax = this.$.catalogAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceCatalogManagement/v4/resourceCatalog/";
		var catalogAddObj = new Object();
		if(this.catalogName) {
			catalogAddObj.name = this.catalogName;
		}
		if(this.catalogDescription) {
			catalogAddObj.description = this.catalogDescription;
		}
		if(this.catalogClass) {
			catalogAddObj['@type'] = this.catalogClass;
		}
		if(this.catalogStatus) {
			catalogAddObj.lifecycleStatus = this.catalogStatus;
		}
		if(this.catalogVersion) {
			catalogAddObj.version = this.catalogVersion;
		}
		ajax.body = JSON.stringify(catalogAddObj);
		ajax.generateRequest();
	}

	_response() {
		this.$.catalogAddModal.close();
		this.catalogName = null;
		this.catalogDescription = null;
		this.catalogVersion = null;
		this.catalogClass = null;
		this.catalogStatus = null;
		document.body.querySelector('inventory-management').shadowRoot.getElementById('catalogList').shadowRoot.getElementById('catalogGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('catalog-add', catalogAdd);
